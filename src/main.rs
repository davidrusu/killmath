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
        Pattern::parse("<left and right as individual entities>"),
    )
}

fn spawn_rewrite(commands: &mut Commands, materials: Res<Materials>, rewrite: Rewrite) {
    commands.spawn((ARS, rewrite));
}

fn setup(commands: &mut Commands, mut materials: ResMut<Assets<ColorMaterial>>) {
    commands.spawn(Camera2dBundle::default());
    commands.insert_resource(Materials {
        pattern_color: materials.add(Color::rgb(0.1, 0.1, 0.1).into()),
        rewrite_color: materials.add(Color::rgb(0.2, 0.15, 0.1).into()),
        font_color: materials.add(Color::rgb(0.9, 0.9, 0.9).into()),
    });
}

fn spawn_initial_state(commands: &mut Commands, materials: Res<Materials>) {
    commands.spawn((ARS, macro_rewrite()));
    commands.spawn((ARS, fork_rewrite()));

    if let Ok(reader) = File::open("listings.nimic").map(BufReader::new) {
        for line in reader.lines() {
            if let Ok(pat) = line {
                commands.spawn((ARS, Pattern::parse(&pat)));
            }
        }
    }
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

#[derive(Clone, PartialEq, Eq)]
struct Rewrite(Pattern, Pattern);

#[derive(PartialEq, Eq, Clone, Debug)]
enum Pattern {
    Sym(String),
    Hole(String),
    Seq(Vec<Pattern>),
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

impl Pattern {
    fn parse_seq(tokens: &mut VecDeque<Tok>) -> Vec<Pattern> {
        let mut seq: Vec<Pattern> = Default::default();
        while let Some(pat) = Self::parse_token(tokens) {
            seq.push(pat)
        }
        seq
    }

    fn parse_token(tokens: &mut VecDeque<Tok>) -> Option<Pattern> {
        if let Some(tok) = tokens.pop_front() {
            match tok {
                Tok::StartSeq => Some(Self::Seq(Self::parse_seq(tokens))),
                Tok::EndSeq => None,
                Tok::Sym(sym) => Some(Self::Sym(sym)),
                Tok::Hole(hole) => Some(Self::Hole(hole)),
            }
        } else {
            None
        }
    }

    fn parse(raw: &str) -> Pattern {
        let mut tokens = Tok::tokenize(raw);

        let mut pattern = None;
        while !tokens.is_empty() {
            let mut next_pats = Self::parse_seq(&mut tokens);
            let next_pat = if next_pats.len() == 1 {
                next_pats.pop().unwrap()
            } else {
                Pattern::Seq(next_pats)
            };
            if let Some(pat) = pattern {
                pattern = Some(Pattern::Seq(vec![pat, next_pat]));
            } else {
                pattern = Some(next_pat);
            }
        }

        pattern.unwrap_or_default()
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
#[derive(PartialEq, Eq, Clone, Debug)]
enum Tok {
    // TAI: what if we simply have seperators like `|`? then we don't need to track start and end
    StartSeq,
    EndSeq,
    Sym(String),
    Hole(String),
}

impl Tok {
    fn tokenize(raw: &str) -> VecDeque<Self> {
        let mut tokens: Vec<Self> = Default::default();
        let mut token_start = 0;
        let mut tok: fn(String) -> Self = Self::Sym;
        for (i, c) in raw.chars().enumerate() {
            if c == '[' {
                tokens.push(tok(raw[token_start..i].to_string()));
                tokens.push(Self::StartSeq);
                tok = Self::Sym;
                token_start = i + 1;
            } else if c == ']' {
                tokens.push(tok(raw[token_start..i].to_string()));
                tokens.push(Self::EndSeq);
                tok = Self::Sym;
                token_start = i + 1;
            } else if c == '?' {
                tokens.push(tok(raw[token_start..i].to_string()));
                tok = Self::Hole;
                token_start = i + 1;
            } else if c.is_whitespace() {
                tokens.push(tok(raw[token_start..i].to_string()));
                tok = Self::Sym;
                token_start = i + 1;
            }
        }
        tokens.push(tok(raw[token_start..].to_string()));

        tokens
            .into_iter()
            .filter(|t| match t {
                Self::Sym(s) if s.chars().all(char::is_whitespace) => false,
                _ => true,
            })
            .collect()
    }
}

fn listener_prompt(
    commands: &mut Commands,
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
            commands.spawn((ARS, Pattern::parse(&listener_state.command)));
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
            step_ars(commands, rewrites, free_patterns);
        }
    });
}

fn step_ars(
    commands: &mut Commands,
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
                        commands.spawn((ARS, Rewrite(pattern, rewrite)));
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
                        // let anon_holes_left: BTreeMap<_, _> = left
                        //     .holes()
                        //     .into_iter()
                        //     .map(|h| (h, unique_hole()))
                        //     .collect();

                        // let anon_holes_right: BTreeMap<_, _> = right
                        //     .holes()
                        //     .into_iter()
                        //     .map(|h| (h, unique_hole()))
                        //     .collect();

                        // commands.spawn((ARS, left.rename_holes(anon_holes_left)));
                        // commands.spawn((ARS, right.rename_holes(anon_holes_right)));
                        commands.spawn((ARS, left));
                        commands.spawn((ARS, right));
                    }
                } else {
                    let rewritten_pattern = rewrite.1.apply(bindings);
                    // let anon_holes: BTreeMap<_, _> = rewritten_pattern
                    //     .holes()
                    //     .into_iter()
                    //     .map(|h| (h, unique_hole()))
                    //     .collect();

                    commands.despawn(rewrite_entity);
                    // commands.spawn((ARS, rewritten_pattern.rename_holes(anon_holes)));
                    commands.spawn((ARS, rewritten_pattern));
                }
            }
        }
    }
}

fn ars(
    time: Res<Time>,
    mut timer: ResMut<ARSTimer>,
    commands: &mut Commands,
    rewrites: Query<(Entity, &Rewrite), With<ARS>>,
    free_patterns: Query<(Entity, &Pattern), With<ARS>>,
) {
    if !timer.0.tick(time.delta_seconds()).just_finished() {
        return;
    }

    step_ars(commands, rewrites, free_patterns);
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

// use bevy::{
//     diagnostic::{Diagnostics, FrameTimeDiagnosticsPlugin},
//     prelude::*,
// };

// /// This example is for debugging text layout
// fn main() {
//     App::build()
//         .add_resource(WindowDescriptor {
//             vsync: false,
//             ..Default::default()
//         })
//         .add_plugins(DefaultPlugins)
//         .add_plugin(FrameTimeDiagnosticsPlugin)
//         .add_startup_system(infotext_system.system())
//         .add_system(change_text_system.system())
//         .run();
// }

// struct TextChanges;

// fn infotext_system(commands: &mut Commands, asset_server: Res<AssetServer>) {
//     let font = asset_server.load("fonts/iosevka-regular.ttf");
//     commands.spawn(CameraUiBundle::default());
//     commands.spawn(TextBundle {
//         style: Style {
//             align_self: AlignSelf::FlexEnd,
//             position_type: PositionType::Absolute,
//             position: Rect {
//                 top: Val::Px(5.0),
//                 left: Val::Px(15.0),
//                 ..Default::default()
//             },
//             ..Default::default()
//         },
//         text: Text {
//             value: "This is\ntext with\nline breaks\nin the top left".to_string(),
//             font: font.clone(),
//             style: TextStyle {
//                 font_size: 50.0,
//                 color: Color::WHITE,
//                 alignment: TextAlignment::default(),
//             },
//         },
//         ..Default::default()
//     });
//     commands.spawn(TextBundle {
//         style: Style {
//             align_self: AlignSelf::FlexEnd,
//             position_type: PositionType::Absolute,
//             position: Rect {
//                 top: Val::Px(5.0),
//                 right: Val::Px(15.0),
//                 ..Default::default()
//             },
//             max_size: Size {
//                 width: Val::Px(400.),
//                 height: Val::Undefined,
//             },
//             ..Default::default()
//         },
//         text: Text {
//             value: "This is very long text with limited width in the top right and is also pink"
//                 .to_string(),
//             font: font.clone(),
//             style: TextStyle {
//                 font_size: 50.0,
//                 color: Color::rgb(0.8, 0.2, 0.7),
//                 alignment: TextAlignment {
//                     horizontal: HorizontalAlign::Center,
//                     vertical: VerticalAlign::Center,
//                 },
//             },
//         },
//         ..Default::default()
//     });
//     commands
//         .spawn(TextBundle {
//             style: Style {
//                 align_self: AlignSelf::FlexEnd,
//                 position_type: PositionType::Absolute,
//                 position: Rect {
//                     bottom: Val::Px(5.0),
//                     right: Val::Px(15.0),
//                     ..Default::default()
//                 },
//                 ..Default::default()
//             },
//             text: Text {
//                 value: "This text changes in the bottom right".to_string(),
//                 font: font.clone(),
//                 style: TextStyle {
//                     font_size: 30.0,
//                     color: Color::WHITE,
//                     alignment: TextAlignment::default(),
//                 },
//             },
//             ..Default::default()
//         })
//         .with(TextChanges);
//     commands.spawn(TextBundle {
//         style: Style {
//             align_self: AlignSelf::FlexEnd,
//             position_type: PositionType::Absolute,
//             position: Rect {
//                 bottom: Val::Px(5.0),
//                 left: Val::Px(15.0),
//                 ..Default::default()
//             },
//             size: Size {
//                 width: Val::Px(200.0),
//                 ..Default::default()
//             },
//             ..Default::default()
//         },
//         text: Text {
//             value: "This\ntext has\nline breaks and also a set width in the bottom left"
//                 .to_string(),
//             font,
//             style: TextStyle {
//                 font_size: 50.0,
//                 color: Color::WHITE,
//                 alignment: TextAlignment::default(),
//             },
//         },
//         ..Default::default()
//     });
// }

// fn change_text_system(
//     time: Res<Time>,
//     diagnostics: Res<Diagnostics>,
//     mut query: Query<&mut Text, With<TextChanges>>,
// ) {
//     for mut text in query.iter_mut() {
//         let mut fps = 0.0;
//         if let Some(fps_diagnostic) = diagnostics.get(FrameTimeDiagnosticsPlugin::FPS) {
//             if let Some(fps_avg) = fps_diagnostic.average() {
//                 fps = fps_avg;
//             }
//         }

//         let mut frame_time = time.delta_seconds_f64();
//         if let Some(frame_time_diagnostic) = diagnostics.get(FrameTimeDiagnosticsPlugin::FRAME_TIME)
//         {
//             if let Some(frame_time_avg) = frame_time_diagnostic.average() {
//                 frame_time = frame_time_avg;
//             }
//         }

//         text.value = format!(
//             "This text changes in the bottom right - {:.1} fps, {:.3} ms/frame",
//             fps,
//             frame_time * 1000.0,
//         );
//     }
// }

// // use bevy::prelude::*;

// // struct Person;

// // struct Name(String);

// // pub struct HelloPlugin;

// // impl Plugin for HelloPlugin {
// //     fn build(&self, app: &mut AppBuilder) {
// //         app.add_resource(GreetTimer(Timer::from_seconds(2.0, true)))
// //             .add_startup_system(setup.system())
// //             .add_system(greet_people.system());
// //     }
// // }

// // fn setup(commands: &mut Commands, asset_server: Res<AssetServer>) {
// //     commands
// //         .spawn(Camera2dBundle::default())
// //         .spawn(Text2dBundle {
// //             text: Text {
// //                 value: "This is text in the 2D scene".to_string(),
// //                 font: asset_server.load("fonts/iosevka-regular.ttf"),
// //                 style: TextStyle {
// //                     font_size: 60.0,
// //                     color: Color::WHITE,
// //                     alignment: TextAlignment {
// //                         vertical: VerticalAlign::Center,
// //                         horizontal: HorizontalAlign::Center,
// //                     },
// //                 },
// //             },
// //             ..Default::default()
// //         });
// // }

// // struct GreetTimer(Timer);

// // fn greet_people(time: Res<Time>, mut timer: ResMut<GreetTimer>, query: Query<&Name, With<Person>>) {
// //     if !timer.0.tick(time.delta_seconds()).just_finished() {
// //         return;
// //     }

// //     for name in query.iter() {
// //         println!("Hey {}!", name.0)
// //     }
// // }

// // fn main() {
// //     App::build()
// //         .add_plugins(DefaultPlugins)
// //         .add_plugin(HelloPlugin)
// //         .run();
// // }
