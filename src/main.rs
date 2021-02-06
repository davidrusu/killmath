#![feature(iter_intersperse)]
#![feature(if_let_guard)]

use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader, Write};

use bevy::prelude::*;
use bevy_egui::{egui, EguiContext, EguiPlugin};

fn unique_hole() -> String {
    format!("{}", rand::random::<u16>())
}

/// This example illustrates the various features of Bevy UI.
fn main() {
    App::build()
        .add_plugins(DefaultPlugins)
        .add_plugin(EguiPlugin)
        .add_resource(ARSTimer(Timer::from_seconds(1.0, true)))
        .add_resource(PersistenceTimer(Timer::from_seconds(5.0, true)))
        .add_resource(ListenerState::default())
        .add_startup_system(setup.system())
        .add_startup_stage(
            "ars_setup",
            SystemStage::single(spawn_initial_state.system()),
        )
        .add_system(listener_prompt.system())
        .add_system(ars_ui.system())
        // .add_system(ars.system())
        .add_system(persistence.system())
        // .add_system(ars_layout.system())
        // .add_system(update_positions.system())
        .run();
}

fn macro_rewrite() -> Rewrite {
    Rewrite(
        Pattern::parse("macro ?pattern ?rewrite".into()),
        Pattern::parse("<defined>"),
    )
}

fn fork_rewrite() -> Rewrite {
    Rewrite(
        Pattern::parse("fork ?left ?right".into()),
        Pattern::parse("<?left and ?right as individual entities>"),
    )
}

fn spawn_rewrite(commands: &mut Commands, materials: &Res<Materials>, rewrite: Rewrite) {
    commands.spawn((ARS, rewrite));
}

fn spawn_pattern(
    commands: &mut Commands,
    materials: &Res<Materials>,
    font: &Res<ARSFont>,
    pattern: Pattern,
) {
    commands.spawn((ARS, pattern));
    //     commands
    //         .spawn(ButtonBundle {
    //             style: Style {
    //                 position_type: PositionType::Absolute,
    //                 // size: Size::new(Val::Px(150.0), Val::Px(65.0)),
    //                 // center button
    //                 margin: Rect::all(Val::Auto),
    //                 // horizontally center child text
    //                 justify_content: JustifyContent::Center,
    //                 // vertically center child text
    //                 align_items: AlignItems::Center,
    //                 ..Default::default()
    //             },
    //             material: materials.pattern_color.clone(),
    //             ..Default::default()
    //         })
    //         .with_children(|parent| {
    //             parent.spawn(TextBundle {
    //                 text: Text {
    //                     value: format!("{}", pattern),
    //                     font: font.0.clone(),
    //                     style: TextStyle {
    //                         font_size: 12.0,
    //                         color: Color::WHITE,
    //                         ..Default::default()
    //                     },
    //                 },
    //                 ..Default::default()
    //             });
    //         })
    //         .with(ARS)
    //         .with(pattern);
}

struct ARSFont(Handle<Font>);
fn setup(
    commands: &mut Commands,
    mut materials: ResMut<Assets<ColorMaterial>>,
    asset_server: Res<AssetServer>,
) {
    commands
        .spawn(CameraUiBundle::default())
        .insert_resource(Materials {
            pattern_color: materials.add(Color::rgb(0.1, 0.1, 0.1).into()),
            rewrite_color: materials.add(Color::rgb(0.2, 0.15, 0.1).into()),
            font_color: materials.add(Color::rgb(0.9, 0.9, 0.9).into()),
        })
        .insert_resource(ARSFont(asset_server.load("fonts/iosevka-medium.ttf")));
}

fn spawn_initial_state(commands: &mut Commands, materials: Res<Materials>, font: Res<ARSFont>) {
    spawn_rewrite(commands, &materials, macro_rewrite());
    spawn_rewrite(commands, &materials, fork_rewrite());

    if let Ok(reader) = File::open("listings.nimic").map(BufReader::new) {
        for line in reader.lines() {
            if let Ok(pat) = line {
                spawn_pattern(commands, &materials, &font, Pattern::parse(&pat));
            }
        }
    }
}

fn ars_layout(
    time: Res<Time>,
    mut timer: ResMut<PersistenceTimer>,
    mut transforms: Query<&mut Transform, With<ARS>>,
) {
    let other_positions: Vec<Vec2> = transforms
        .iter_mut()
        .map(|t| t.translation.truncate())
        .collect();

    use rand::Rng;
    let mut thread_rng = rand::thread_rng();
    for mut transform in transforms.iter_mut() {
        for p_other in other_positions.iter() {
            let mut diff = transform.translation.truncate() - *p_other;
            let d = diff.length();
            if d > 0.0 {
                diff += Vec2::new(
                    thread_rng.gen_range(-1.0..1.0),
                    thread_rng.gen_range(-1.0..1.0),
                ) * 0.0001;
                let push = (diff.normalize()).extend(0.0) * 0.001;
                println!("Pushing {:?}", push);
                // transform.translation += push;
            }
        }
        println!("Transform: {:?}", transform.translation);
        transform.translation.y += 1.0;
    }
}

fn update_positions(mut styles: Query<(&Transform, &mut Style), With<ARS>>) {
    //println!("Updating positions");
    //for (transform, mut style) in styles.iter_mut() {
    //    println!(
    //        "Updating: {:?} -> {}",
    //        style.position, transform.translation
    //    );
    //    //style.position.top = Val::Px(transform.translation.y);
    //    //style.position.left = Val::Px(transform.translation.x);
    //}
}

fn persistence(
    time: Res<Time>,
    mut timer: ResMut<PersistenceTimer>,
    rewrites: Query<&Rewrite, With<ARS>>,
    free_patterns: Query<&Pattern, With<ARS>>,
) {
    if !timer.0.tick(time.delta_seconds()).just_finished() {
        return;
    }

    let listings_file = "listings.nimic";
    let listings_file_tmp = "listings.nimic.tmp";
    {
        let mut tmp = File::create(listings_file_tmp).unwrap();

        for pattern in free_patterns.iter() {
            tmp.write_all(format!("{}\n", pattern).as_bytes()).unwrap();
        }

        for rewrite in rewrites.iter() {
            if rewrite == &macro_rewrite() || rewrite == &fork_rewrite() {
                continue;
            }

            tmp.write_all(format!("macro {} {}\n", rewrite.0, rewrite.1).as_bytes())
                .unwrap();
        }
    }

    std::fs::rename(listings_file_tmp, listings_file).unwrap();
    println!("Saved listings");
}

#[derive(Default)]
struct ListenerState {
    command: String,
}

struct ARSTimer(Timer);
struct PersistenceTimer(Timer);
struct ARS;

struct Materials {
    pattern_color: Handle<ColorMaterial>,
    rewrite_color: Handle<ColorMaterial>,
    font_color: Handle<ColorMaterial>,
}

struct Kinematics {
    pos: Vec2,
    vel: Vec2,
}

#[derive(Clone, PartialEq, Eq)]
struct Rewrite(Pattern, Pattern);

#[derive(PartialEq, Eq, Clone, Debug)]
enum Pattern {
    Sym(String),       // ==> concrete value
    Hole(String),      // ==> ?var
    Seq(Vec<Pattern>), // ==> [ x y ?v ]
}

impl Default for Pattern {
    fn default() -> Self {
        Self::Seq(Default::default())
    }
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sym(sym) => write!(f, "{}", sym),
            Self::Hole(hole) => write!(f, "?{}", hole),
            Self::Seq(pats) => {
                write!(f, "[")?;
                for p in pats.iter().intersperse(&Self::Sym(" ".into())) {
                    write!(f, "{}", p)?;
                }
                write!(f, "]")
            }
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Tok {
    StartSeq,     // => [
    EndSeq,       // => ]
    Sym(String),  // => word
    Hole(String), // => ?var
}

impl Tok {
    fn tokenize(raw: &str) -> VecDeque<Self> {
        let mut tokens: Vec<Self> = Default::default();
        let mut token_start = 0;
        let mut tok_builder: fn(String) -> Self = Self::Sym;
        for (i, c) in raw.chars().enumerate() {
            let parsed_tok: Option<(_, fn(String) -> Self)> = match c {
                '[' => Some((Some(Self::StartSeq), Self::Sym)),
                ']' => Some((Some(Self::EndSeq), Self::Sym)),
                '?' => Some((None, Self::Hole)),
                c if c.is_whitespace() => Some((None, Self::Sym)),
                _ => None,
            };

            if let Some((boundary_token, next_builder)) = parsed_tok {
                tokens.push(tok_builder(raw[token_start..i].to_string()));
                if let Some(tok) = boundary_token {
                    tokens.push(tok);
                }
                tok_builder = next_builder;
                token_start = i + 1;
            }
        }
        tokens.push(tok_builder(raw[token_start..].to_string()));
        tokens
            .into_iter()
            .filter(|t| match t {
                Self::Sym(s) if s.chars().all(char::is_whitespace) => false,
                _ => true,
            })
            .collect()
    }
}

impl Pattern {
    fn parse_seq(tokens: &mut VecDeque<Tok>) -> Vec<Pattern> {
        let mut seq: Vec<Pattern> = Default::default();
        while let Some(pat) = Self::parse_token(tokens) {
            seq.push(pat)
        }
        seq
    }

    fn parse_token(tokens: &mut VecDeque<Tok>) -> Option<Pattern> {
        tokens.pop_front().and_then(|tok| match tok {
            Tok::StartSeq => Some(Self::Seq(Self::parse_seq(tokens))),
            Tok::EndSeq => None,
            Tok::Sym(sym) => Some(Self::Sym(sym)),
            Tok::Hole(hole) => Some(Self::Hole(hole)),
        })
    }

    fn parse(raw: &str) -> Pattern {
        let mut tokens = Tok::tokenize(raw);

        let mut pattern = None;
        while !tokens.is_empty() {
            let seq = Self::parse_seq(&mut tokens);
            if let Some(pat) = pattern {
                pattern = Some(Pattern::Seq(std::iter::once(pat).chain(seq).collect()));
            } else {
                pattern = Some(Pattern::Seq(seq));
            }
        }

        match pattern.unwrap_or_default() {
            Pattern::Seq(mut s) if s.len() == 1 => s.pop().unwrap(),
            p => p,
        }
    }

    fn bind(&self, other: &Self) -> Result<Vec<(String, Pattern)>, ()> {
        match (self, other) {
            (Self::Sym(a), Self::Sym(b)) => {
                if a == b {
                    Ok(Default::default())
                } else {
                    Err(())
                }
            }
            (Self::Hole(hole), pat) => Ok(vec![(hole.clone(), pat.clone())]),
            (Self::Seq(a_seq), Self::Seq(b_seq)) => {
                if a_seq.len() != b_seq.len() {
                    Err(())
                } else {
                    let mut bindings: Vec<_> = Default::default();
                    for (a, b) in a_seq.iter().zip(b_seq.iter()) {
                        bindings.extend(a.bind(&b)?);
                    }
                    Ok(bindings)
                }
            }
            _ => Err(()),
        }
    }

    fn apply(&self, bindings: Vec<(String, Pattern)>) -> Self {
        let bindings_map: BTreeMap<String, Pattern> = bindings.iter().cloned().collect();
        match self {
            Self::Hole(hole) if bindings_map.contains_key(hole) => {
                bindings_map.get(hole).unwrap().clone()
            }
            Self::Seq(seq) => Self::Seq(seq.iter().map(|p| p.apply(bindings.clone())).collect()),
            pat => pat.clone(),
        }
    }

    fn holes(&self) -> BTreeSet<String> {
        match self {
            Self::Hole(hole) => vec![hole.clone()].into_iter().collect(),
            Self::Seq(seq) => seq.iter().flat_map(|p| p.holes()).collect(),
            pat => Default::default(),
        }
    }

    fn rename_holes(self, mut mapping: BTreeMap<String, String>) -> Self {
        match self {
            Self::Hole(hole) if let Some(renamed_hole) = mapping.remove(&hole) => Self::Hole(renamed_hole),
            Self::Seq(seq) => Self::Seq(seq.into_iter().map(|p| p.rename_holes(mapping.clone())).collect()),
            pat => pat,
        }
    }

    fn complexity(&self) -> usize {
        match self {
            Self::Sym(s) => s.len(),
            Self::Hole(h) => 1,
            Self::Seq(seq) => seq.iter().map(Self::complexity).sum::<usize>() + 1,
        }
    }
}

fn listener_prompt(
    commands: &mut Commands,
    materials: Res<Materials>,
    font: Res<ARSFont>,
    rewrites: Query<(Entity, &Rewrite), With<ARS>>,
    free_patterns: Query<(Entity, &Pattern), With<ARS>>,
    mut listener_state: ResMut<ListenerState>,
    mut egui_context: ResMut<EguiContext>,
) {
    let ctx = &mut egui_context.ctx;
    egui::Window::new("Listener").show(ctx, |ui| {
        let listener_resp = ui.text_edit_multiline(&mut listener_state.command);
        if (ui.button("parse").clicked || listener_resp.lost_kb_focus)
            && !listener_state.command.is_empty()
        {
            spawn_pattern(
                commands,
                &materials,
                &font,
                Pattern::parse(&listener_state.command),
            );
            listener_state.command = Default::default();
        }
        if ui.button("clear").clicked {
            for (e, r) in rewrites.iter() {
                if r != &macro_rewrite() && r != &fork_rewrite() {
                    commands.despawn(e);
                }
            }

            for (e, _) in free_patterns.iter() {
                commands.despawn(e);
            }
        }
        if ui.button("step").clicked {
            step_ars(commands, materials, font, rewrites, free_patterns);
        }
    });
}

fn step_ars(
    commands: &mut Commands,
    materials: Res<Materials>,
    font: Res<ARSFont>,
    rewrites: Query<(Entity, &Rewrite), With<ARS>>,
    free_patterns: Query<(Entity, &Pattern), With<ARS>>,
) {
    let mut spent_rewrites: BTreeSet<Entity> = Default::default();

    for (pattern_entity, pattern) in free_patterns.iter() {
        let mut candidate_rewrites: Vec<(Rewrite, Entity)> = Default::default();

        let pattern_holes: BTreeMap<_, _> = pattern
            .holes()
            .into_iter()
            .map(|h| (h, unique_hole()))
            .collect();

        let pattern = pattern.clone().rename_holes(pattern_holes);

        for (rewrite_entity, rewrite) in rewrites.iter() {
            if spent_rewrites.contains(&rewrite_entity) {
                continue;
            }
            if let Ok(bindings) = rewrite.0.bind(&pattern) {
                candidate_rewrites.push((rewrite.clone(), rewrite_entity));
            }
        }
        candidate_rewrites
            .sort_by(|(r_a, _), (r_b, _)| r_a.0.complexity().cmp(&r_b.0.complexity()));
        if let Some((rewrite, rewrite_entity)) = candidate_rewrites.pop() {
            if let Ok(bindings) = rewrite.0.bind(&pattern) {
                println!("Bindings {:?}", bindings);
                commands.despawn(pattern_entity);
                spent_rewrites.insert(rewrite_entity);

                if rewrite == macro_rewrite() {
                    let bindings_map: BTreeMap<_, _> = bindings.iter().cloned().collect();
                    if bindings_map.len() != bindings.len() {
                        panic!("Unification is not supported yet");
                    }

                    if let (Some(pattern), Some(rewrite)) = (
                        bindings_map.get("pattern").cloned(),
                        bindings_map.get("rewrite").cloned(),
                    ) {
                        if pattern == rewrite {
                            println!("Dropping identity rewrite");
                            continue;
                        }
                        spawn_rewrite(commands, &materials, Rewrite(pattern, rewrite));
                    }
                } else if rewrite == fork_rewrite() {
                    let bindings_map: BTreeMap<_, _> = bindings.iter().cloned().collect();
                    if bindings_map.len() != bindings.len() {
                        panic!("Unification is not supported yet");
                    }

                    if let (Some(left), Some(right)) = (
                        bindings_map.get("left").cloned(),
                        bindings_map.get("right").cloned(),
                    ) {
                        spawn_pattern(commands, &materials, &font, left);
                        spawn_pattern(commands, &materials, &font, right);
                    }
                } else {
                    let rewritten_pattern = rewrite.1.apply(bindings);
                    commands.despawn(rewrite_entity);
                    spawn_pattern(commands, &materials, &font, rewritten_pattern);
                }
            }
        }
    }
}

fn ars(
    time: Res<Time>,
    mut timer: ResMut<ARSTimer>,
    commands: &mut Commands,
    materials: Res<Materials>,
    font: Res<ARSFont>,
    rewrites: Query<(Entity, &Rewrite), With<ARS>>,
    free_patterns: Query<(Entity, &Pattern), With<ARS>>,
) {
    if !timer.0.tick(time.delta_seconds()).just_finished() {
        return;
    }

    step_ars(commands, materials, font, rewrites, free_patterns);
}

fn ars_ui(
    mut egui_context: ResMut<EguiContext>,
    patterns: Query<&Pattern, With<ARS>>,
    rewrites: Query<&Rewrite, With<ARS>>,
) {
    let ctx = &mut egui_context.ctx;
    for (i, pattern) in patterns.iter().enumerate() {
        egui::Window::new(format!("{}. {}", i, pattern))
            .title_bar(false)
            .show(ctx, |ui| {
                ui.label(format!("{}", pattern));
            });
    }

    for (i, rewrite) in rewrites.iter().enumerate() {
        egui::Window::new(format!("{}. {} => {}", i, rewrite.0, rewrite.1))
            .title_bar(false)
            .resizable(false)
            .show(ctx, |ui| {
                ui.vertical(|ui| {
                    ui.monospace(format!("{}", rewrite.0));
                    ui.separator();
                    ui.monospace(format!("{}", rewrite.1));
                });
            });
    }
}
