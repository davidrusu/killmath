#![feature(iter_intersperse)]
#![feature(if_let_guard)]

use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader, Write};

use bevy::input::{
    mouse::{MouseButtonInput, MouseMotion, MouseWheel},
    ElementState,
};
use bevy::prelude::*;
use bevy::render::camera::Camera;
use bevy::text::CalculatedSize;
use bevy::window::CursorMoved;
use bevy_egui::{egui, EguiContext, EguiPlugin};

/// This example illustrates the various features of Bevy UI.
fn main() {
    App::build()
        .add_plugins(DefaultPlugins)
        .add_plugin(EguiPlugin)
        .add_startup_system(setup.system())
        .add_startup_stage(
            "ars_setup",
            SystemStage::single(spawn_initial_state.system()),
        )
        .add_system(listener_prompt.system())
        // .add_system(ars_ui.system())
        .add_system(update_listener.system())
        .add_system(ars.system())
        .add_system(persistence.system())
        .add_system(ars_layout.system())
        .add_system(update_positions.system())
        .add_system(kinematics_system.system())
        .add_system(ars_term_bg.system())
        .add_system(rewrite_layout.system())
        .add_system(print_mouse_events_system.system())
        .add_system(focus_system.system())
        .add_system(propagate_bboxes.system())
        .run();
}

fn rewrite_rewrite() -> Rewrite {
    Rewrite(
        Pattern::parse("?pattern -> ?rewrite".into()),
        Pattern::parse("<defined>"),
    )
}

fn fork_rewrite() -> Rewrite {
    Rewrite(
        Pattern::parse("fork ?left ?right".into()),
        Pattern::parse("<?left and ?right as individual entities>"),
    )
}

fn spawn_rewrite(
    commands: &mut Commands,
    materials: &Res<Materials>,
    font: &Res<ARSFont>,
    rewrite: Rewrite,
) {
    // commands.spawn((ARS, rewrite));
    commands
        .spawn((
            ARS,
            rewrite.clone(),
            Kinematics::random(),
            GlobalTransform::default(),
            Transform::default(),
            BBox::default(),
        ))
        .with_children(|parent| {
            parent
                .spawn(Text2dBundle {
                    text: Text::with_section(
                        rewrite.0.pprint(),
                        TextStyle {
                            font: font.0.clone(),
                            font_size: 12.0,
                            color: Color::WHITE,
                        },
                        TextAlignment {
                            vertical: VerticalAlign::Bottom,
                            horizontal: HorizontalAlign::Left,
                        },
                    ),
                    ..Default::default()
                })
                .with_children(|parent| {
                    parent
                        .spawn(SpriteBundle {
                            material: materials.rewrite_color.clone(),
                            sprite: Sprite::new(Vec2::default()),
                            ..Default::default()
                        })
                        .with(PatternBG)
                        .with(FollowParent)
                        .with(BBox::default());
                })
                .with(TextWithBG)
                .with(FollowParent)
                .with(TopPattern)
                .with(BBox::default());
            parent
                .spawn(SpriteBundle {
                    material: materials.surfboard_line_color.clone(),
                    sprite: Sprite::new(Vec2::new(500.0, 10.0)),
                    ..Default::default()
                })
                .with(SurfboardLine)
                .with(FollowParent)
                .with(BBox::default());
            parent
                .spawn(Text2dBundle {
                    text: Text::with_section(
                        rewrite.1.pprint(),
                        TextStyle {
                            font: font.0.clone(),
                            font_size: 12.0,
                            color: Color::WHITE,
                        },
                        TextAlignment {
                            vertical: VerticalAlign::Bottom,
                            horizontal: HorizontalAlign::Left,
                        },
                    ),
                    ..Default::default()
                })
                .with_children(|parent| {
                    parent
                        .spawn(SpriteBundle {
                            material: materials.rewrite_color.clone(),
                            sprite: Sprite::new(Vec2::new(100.0, 100.0)),
                            ..Default::default()
                        })
                        .with(PatternBG)
                        .with(FollowParent)
                        .with(BBox::default());
                })
                .with(TextWithBG)
                .with(FollowParent)
                .with(BottomPattern)
                .with(BBox::default());
        });
}

fn spawn_pattern(
    commands: &mut Commands,
    materials: &Res<Materials>,
    font: &Res<ARSFont>,
    pattern: Pattern,
) {
    // commands.spawn((ARS, pattern));
    commands
        .spawn(Text2dBundle {
            text: Text::with_section(
                pattern.pprint(),
                TextStyle {
                    font: font.0.clone(),
                    font_size: 12.0,
                    color: Color::WHITE,
                },
                TextAlignment {
                    vertical: VerticalAlign::Bottom,
                    horizontal: HorizontalAlign::Left,
                },
            ),
            ..Default::default()
        })
        .with_children(|parent| {
            parent
                .spawn(SpriteBundle {
                    material: materials.pattern_color.clone(),
                    sprite: Sprite::new(Vec2::new(100.0, 100.0)),
                    ..Default::default()
                })
                .with(PatternBG)
                .with(BBox::default());
        })
        .with(TextWithBG)
        .with(ARS)
        .with(pattern)
        .with(Kinematics::random())
        .with(BBox::default());
}

struct ARSFont(Handle<Font>);
fn setup(
    commands: &mut Commands,
    mut materials: ResMut<Assets<ColorMaterial>>,
    asset_server: Res<AssetServer>,
) {
    commands
        .spawn(OrthographicCameraBundle::new_2d())
        .insert_resource(Holes(0))
        .insert_resource(ARSTimer(Timer::from_seconds(0.01, true)))
        .insert_resource(RewriteTimer(Timer::from_seconds(0.5, true)))
        .insert_resource(PersistenceTimer(Timer::from_seconds(5.0, true)))
        .insert_resource(ListenerState::default())
        .insert_resource(Pointer::default())
        .insert_resource(Materials {
            pattern_color: materials.add(Color::rgba(0.0, 0.0, 0.0, 0.3).into()),
            rewrite_color: materials.add(Color::rgba(1.0, 0.8, 0.1, 0.1).into()),
            highlight_color: materials.add(Color::rgba(1.0, 0.0, 0.0, 0.5).into()),
            font_color: materials.add(Color::rgb(0.9, 0.9, 0.9).into()),
            surfboard_line_color: materials.add(Color::rgb(0.0, 0.0, 0.0).into()),
        })
        .insert_resource(ARSFont(asset_server.load("fonts/iosevka-medium.ttf")));
}

fn spawn_initial_state(commands: &mut Commands, materials: Res<Materials>, font: Res<ARSFont>) {
    spawn_rewrite(commands, &materials, &font, rewrite_rewrite());
    spawn_rewrite(commands, &materials, &font, fork_rewrite());

    if let Ok(reader) = File::open("listings.nimic").map(BufReader::new) {
        for line in reader.lines() {
            if let Ok(pat) = line {
                spawn_pattern(commands, &materials, &font, Pattern::parse(&pat));
            }
        }
    }
}

fn ars_layout(
    timer: ResMut<ARSTimer>,
    bboxes: Query<(Entity, &BBox), With<ARS>>,
    mut kinematics: Query<&mut Kinematics, With<ARS>>,
) {
    if !timer.0.just_finished() {
        return;
    }
    for (e_1, bbox_1) in bboxes.iter() {
        for (e_2, bbox_2) in bboxes.iter() {
            if kinematics.get_mut(e_1).is_err() || kinematics.get_mut(e_2).is_err() {
                continue;
            };
            if e_1 == e_2 {
                continue;
            }

            let buffer = 10.0;
            let bbox_1 = bbox_1.buffer(buffer);
            let bbox_2 = bbox_2.buffer(buffer);

            let delta = bbox_2.center() - bbox_1.center();

            if delta.x.abs() > (bbox_1.width() + bbox_2.width()) * 0.5 {
                continue;
            }
            if delta.y.abs() > (bbox_1.height() + bbox_2.height()) * 0.5 {
                continue;
            }

            let push_vecs = vec![
                Vec2::new(0.0, (bbox_2.top() - bbox_1.bottom()).y),
                Vec2::new((bbox_2.right() - bbox_1.left()).x, 0.0),
                Vec2::new(0.0, (bbox_2.bottom() - bbox_1.top()).y),
                Vec2::new((bbox_2.left() - bbox_1.right()).x, 0.0),
            ];
            let push_vec = push_vecs
                .into_iter()
                .min_by(|a, b| a.length().partial_cmp(&b.length()).unwrap())
                .unwrap();

            if let Ok(mut e_kin) = kinematics.get_mut(e_1) {
                e_kin.vel += push_vec * 2.0;
            }
        }
    }
}

fn kinematics_system(
    time: Res<Time>,
    mut timer: ResMut<ARSTimer>,
    mut kinematics: Query<&mut Kinematics>,
) {
    if !timer.0.tick(time.delta_seconds()).just_finished() {
        return;
    }
    for mut k in kinematics.iter_mut() {
        k.pos = k.pos + k.vel * time.delta_seconds();
        k.vel = k.vel - k.pos * 0.1;
        k.vel *= 0.8;
    }
}

fn update_positions(mut kin_and_trans: Query<(&Kinematics, &BBox, &mut GlobalTransform)>) {
    //println!("Updating positions");
    for (kinematics, bbox, mut transform) in kin_and_trans.iter_mut() {
        transform.translation.x = kinematics.pos.x + bbox.width() * 0.5;
        transform.translation.y = kinematics.pos.y + bbox.height() * 0.5;
    }
}

fn ars_term_bg(
    calculated_size_q: Query<(&CalculatedSize, &Children), With<TextWithBG>>,
    mut sprites: Query<(&mut Sprite, &mut Transform), With<PatternBG>>,
) {
    for (calculated_size, children) in calculated_size_q.iter() {
        for child in children.iter() {
            if let Ok((mut s, mut trans)) = sprites.get_mut(*child) {
                let margin = 0.0;
                s.size = Vec2::new(
                    calculated_size.size.width + margin,
                    calculated_size.size.height + margin,
                );
                trans.translation.x = -calculated_size.size.width * 0.5;
                trans.translation.y = -calculated_size.size.height * 0.5;
            }
        }
    }
}

fn rewrite_layout(
    terms: Query<&Children, With<Rewrite>>,
    mut top_patterns: Query<(&mut Transform, &CalculatedSize), With<TopPattern>>,
    mut surfboards: Query<(&mut Transform, &mut Sprite), With<SurfboardLine>>,
    mut bottom_patterns: Query<(&mut Transform, &CalculatedSize), With<BottomPattern>>,
) {
    for children in terms.iter() {
        let mut top_height = None;
        let mut top_width = None;
        let mut bottom_height = None;
        let mut bottom_width = None;
        for child in children.iter() {
            if let Ok((_, calc_size)) = top_patterns.get_mut(*child) {
                top_height = Some(calc_size.size.height);
                top_width = Some(calc_size.size.width);
            }
            if let Ok((_, calc_size)) = bottom_patterns.get_mut(*child) {
                bottom_height = Some(calc_size.size.height);
                bottom_width = Some(calc_size.size.width);
            }
        }
        if let (Some(top_h), Some(top_w), Some(bottom_w)) = (top_height, top_width, bottom_width) {
            let rewrite_w = top_w.max(bottom_w);
            let buffer = 5.0;
            for child in children.iter() {
                if let Ok((mut trans, mut sprite)) = surfboards.get_mut(*child) {
                    let surfboard_w = rewrite_w + 20.0;
                    sprite.size = Vec2::new(surfboard_w, 1.5);
                    trans.translation.y = -top_h - buffer;
                    trans.translation.x = -rewrite_w * 0.5;
                    // trans.translation.x = -rewrite_w * 0.5;
                }
                if let Ok((mut trans, _)) = bottom_patterns.get_mut(*child) {
                    trans.translation.y = -(top_h + buffer * 2.0);
                    trans.translation.x = -(rewrite_w - bottom_w) * 0.5;
                }
                if let Ok((mut trans, _)) = top_patterns.get_mut(*child) {
                    // trans.translation.y = -(top_h + 10.0);
                    trans.translation.x = -(rewrite_w - top_w) * 0.5;
                }
            }
        }
    }
}

fn propagate_bboxes(
    mut bboxes: Query<&mut BBox>,
    sprites: Query<(Entity, &Sprite), With<BBox>>,
    transforms: Query<&Transform, With<BBox>>,
    parents: Query<(Entity, &Children), With<BBox>>,
    global_transforms: Query<&GlobalTransform, With<BBox>>,
    roots: Query<Entity, Without<Parent>>,
) {
    for (e, sprite) in sprites.iter() {
        if let (Ok(mut bbox), Ok(trans)) = (bboxes.get_mut(e), transforms.get(e)) {
            bbox.upper_right = sprite.size * 0.5 + trans.translation.truncate();
            bbox.lower_left = -sprite.size * 0.5 + trans.translation.truncate();
            assert!(bbox.height() >= 0.0, "{:?}", bbox);
            assert!(bbox.width() >= 0.0, "{:?}", bbox);
        }
    }

    for (parent, children) in parents.iter() {
        let mut parent_bbox = BBox::default();

        for child in children.iter() {
            let child_bbox = bboxes.get_mut(*child);
            if let Ok(child_bbox) = child_bbox {
                parent_bbox.upper_right = child_bbox.upper_right.max(parent_bbox.upper_right);
                parent_bbox.lower_left = child_bbox.lower_left.min(parent_bbox.lower_left);
                assert!(parent_bbox.height() >= 0.0, "{:?}", parent_bbox);
                assert!(parent_bbox.width() >= 0.0, "{:?}", parent_bbox);
            }
        }

        let mut global_translation = if roots.get(parent).is_ok() {
            global_transforms
                .get(parent)
                .ok()
                .cloned()
                .unwrap_or_default()
                .translation
                .truncate()
        } else {
            transforms
                .get(parent)
                .ok()
                .cloned()
                .unwrap_or_default()
                .translation
                .truncate()
        };
        if global_translation.x.is_nan() || global_translation.y.is_nan() {
            global_translation = Default::default();
        }

        if let Ok(mut bbox) = bboxes.get_mut(parent) {
            assert!(bbox.height() >= 0.0, "{:?}", bbox);
            assert!(bbox.width() >= 0.0, "{:?}", bbox);
            bbox.upper_right = parent_bbox.upper_right + global_translation;
            bbox.lower_left = parent_bbox.lower_left + global_translation;
            assert!(bbox.height() >= 0.0, "{:?}", bbox);
            assert!(bbox.width() >= 0.0, "{:?}", bbox);
        }
    }
}

// This system prints out all mouse events as they come in
fn print_mouse_events_system(
    windows: Res<Windows>,
    mut pointer: ResMut<Pointer>,
    camera_query: Query<(&Camera, &GlobalTransform)>,
    mut mouse_button_input_events: EventReader<MouseButtonInput>,
    mut mouse_motion_events: EventReader<MouseMotion>,
    mut cursor_moved_events: EventReader<CursorMoved>,
    mut mouse_wheel_events: EventReader<MouseWheel>,
    holdable_entities: Query<(Entity, &BBox, &GlobalTransform), With<ARS>>,
) {
    for event in mouse_button_input_events.iter() {
        match event {
            MouseButtonInput {
                button: MouseButton::Left,
                state,
            } => {
                pointer.drag_start = pointer.pos;
                pointer.down = state == &ElementState::Pressed;
            }
            event => println!("{:?}", event),
        }
    }

    for _event in mouse_motion_events.iter() {
        //println!("{:?}", event);
    }

    for event in cursor_moved_events.iter() {
        if let Some(window) = windows.get(event.id) {
            pointer.pos = event.position;
        }
    }

    for _event in mouse_wheel_events.iter() {
        //println!("{:?}", event);
    }

    if !pointer.down {
        pointer.holding = None;
    }
    if let Some((camera, cam_trans)) = camera_query.iter().next() {
        if pointer.down && pointer.holding.is_none() {
            for (entity, bbox, trans) in holdable_entities.iter() {
                if let Some(pos) = camera.world_to_screen(&windows, cam_trans, trans.translation) {
                    let bbox_on_screen = BBox {
                        upper_right: pos,
                        lower_left: pos - bbox.size(),
                    };
                    if bbox_on_screen.contains(pointer.pos) {
                        pointer.holding = Some(entity);
                        break;
                    }
                }
            }
        }
    }
}

fn focus_system(
    windows: Res<Windows>,
    pointer: Res<Pointer>,
    mut terms: Query<(&BBox, &GlobalTransform, &mut Kinematics), With<ARS>>,
    camera_query: Query<(&Camera, &GlobalTransform)>,
) {
    if let (Some((camera, cam_trans)), Some(holding_entity)) =
        (camera_query.iter().next(), pointer.holding)
    {
        if let Ok((bbox, trans, mut kin)) = terms.get_mut(holding_entity) {
            if let Some(pos) = camera.world_to_screen(&windows, cam_trans, trans.translation) {
                let bbox_on_screen = BBox {
                    upper_right: pos,
                    lower_left: pos - bbox.size(),
                };
                kin.pos += (pointer.pos - bbox_on_screen.center()) * 0.5;
                kin.vel *= 0.0;
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
            if rewrite.is_primitive() {
                continue;
            }

            tmp.write_all(format!("{}\n", rewrite).as_bytes()).unwrap();
        }
    }

    std::fs::rename(listings_file_tmp, listings_file).unwrap();
    println!("Saved listings");
}

#[derive(Default)]
struct ListenerState {
    command: String,
}

#[derive(Default)]
struct Holes(u64);
struct ARSTimer(Timer);
struct RewriteTimer(Timer);
struct PersistenceTimer(Timer);
struct ARS;
struct PatternBG;
struct SurfboardLine;
struct TopPattern;
struct BottomPattern;
struct FollowParent;
struct TextWithBG;
#[derive(Default, Debug)]
struct Pointer {
    down: bool,
    pos: Vec2,
    drag_start: Vec2,
    holding: Option<Entity>,
}
#[derive(Default, Debug)]
struct BBox {
    upper_right: Vec2,
    lower_left: Vec2,
}

impl BBox {
    fn buffer(&self, buf: f32) -> Self {
        let buf = Vec2::new(buf, buf);
        Self {
            upper_right: self.upper_right + buf,
            lower_left: self.lower_left - buf,
        }
    }

    fn size(&self) -> Vec2 {
        Vec2::new(self.width(), self.height())
    }

    fn width(&self) -> f32 {
        self.upper_right.x - self.lower_left.x
    }

    fn height(&self) -> f32 {
        self.upper_right.y - self.lower_left.y
    }

    fn center(&self) -> Vec2 {
        (self.upper_right + self.lower_left) * 0.5
    }

    fn top(&self) -> Vec2 {
        self.center() + Vec2::new(0.0, self.height() * 0.5)
    }

    fn bottom(&self) -> Vec2 {
        self.center() + Vec2::new(0.0, -self.height() * 0.5)
    }

    fn right(&self) -> Vec2 {
        self.center() + Vec2::new(self.width() * 0.5, 0.0)
    }

    fn left(&self) -> Vec2 {
        self.center() + Vec2::new(-self.width() * 0.5, 0.0)
    }

    fn contains(&self, p: Vec2) -> bool {
        p.x <= self.upper_right.x
            && p.x >= self.lower_left.x
            && p.y <= self.upper_right.y
            && p.y >= self.lower_left.y
    }
}

struct Materials {
    pattern_color: Handle<ColorMaterial>,
    rewrite_color: Handle<ColorMaterial>,
    highlight_color: Handle<ColorMaterial>,
    surfboard_line_color: Handle<ColorMaterial>,
    font_color: Handle<ColorMaterial>,
}

#[derive(Debug, Default, Clone)]
struct Kinematics {
    pos: Vec2,
    vel: Vec2,
}

impl Kinematics {
    fn random() -> Self {
        use rand::Rng;
        let mut thread_rng = rand::thread_rng();

        Self {
            pos: Vec2::new(
                thread_rng.gen_range(-1.0..1.0),
                thread_rng.gen_range(-1.0..1.0),
            ) * 4.,
            vel: Vec2::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Rewrite(Pattern, Pattern);

impl Rewrite {
    fn is_primitive(&self) -> bool {
        self == &rewrite_rewrite() || self == &fork_rewrite()
    }

    fn pprint(&self) -> String {
        format!("[{} -> {}]", self.0.pprint(), self.1.pprint())
    }
}

impl std::fmt::Display for Rewrite {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} -> {}]", self.0, self.1)
    }
}

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

    fn pprint(&self) -> String {
        self.pprint_indented(0, false)
    }

    fn pprint_indented(&self, indent_level: usize, parent_wrapped: bool) -> String {
        let indent: String = std::iter::repeat(" ".to_string())
            .take(indent_level)
            .collect();
        match self {
            Self::Sym(s) => format!("{}", s),
            Self::Hole(h) => format!("?{}", h),
            Self::Seq(seq) => {
                let mut cumulative_complexity = 0;
                let mut wrapped = false;
                let mut pprint = format!("{}[", if parent_wrapped { &indent } else { "" });
                for pat in seq.iter().intersperse(&Self::Sym(" ".into())) {
                    cumulative_complexity += pat.complexity();
                    if cumulative_complexity > 15 {
                        //cumulative_complexity = 0;
                        wrapped = true;
                        if pat != &Self::Sym(" ".into()) {
                            pprint = format!("{}\n{}", pprint, indent);
                        }
                    }
                    pprint = format!(
                        "{}{}",
                        pprint,
                        pat.pprint_indented(indent_level + (if wrapped { 1 } else { 0 }), wrapped)
                    );
                }
                pprint = format!("{}]", pprint);
                pprint
            }
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
            _ => Default::default(),
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
            Self::Sym(s) | Self::Hole(s) => ((s.len() as f64).log2() as usize) + 1,
            Self::Seq(seq) => seq.iter().map(Self::complexity).sum::<usize>() + 1,
        }
    }
}

fn listener_prompt(
    commands: &mut Commands,
    mut rewrite_timer: ResMut<RewriteTimer>,
    holes: ResMut<Holes>,
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
        if ui.button("parse").clicked() && !listener_state.command.is_empty() {
            spawn_pattern(
                commands,
                &materials,
                &font,
                Pattern::parse(&listener_state.command),
            );
            listener_state.command = Default::default();
        }
        if ui.button("clear").clicked() {
            for (e, r) in rewrites.iter() {
                if !r.is_primitive() {
                    commands.despawn_recursive(e);
                }
            }

            for (e, _) in free_patterns.iter() {
                commands.despawn_recursive(e);
            }
        }
        if rewrite_timer.0.paused() {
            if ui.button("unpause").clicked() {
                rewrite_timer.0.unpause()
            }
        } else {
            if ui.button("pause").clicked() {
                rewrite_timer.0.pause()
            }
        }
        if ui.button("step").clicked() {
            step_ars(commands, holes, materials, font, rewrites, free_patterns);
        }
    });
}

fn update_listener(
    mut listener_state: ResMut<ListenerState>,
    pointer: Res<Pointer>,
    rewrites: Query<&Rewrite>,
    patterns: Query<&Pattern>,
) {
    if let Some(holding_entity) = pointer.holding {
        if let Ok(rewrite) = rewrites.get(holding_entity) {
            listener_state.command = rewrite.pprint()
        } else if let Ok(pat) = patterns.get(holding_entity) {
            listener_state.command = pat.pprint()
        };
    }
}

fn step_ars(
    commands: &mut Commands,
    mut holes: ResMut<Holes>,
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
            .map(|h| {
                holes.0 += 1;
                (h, format!("{}", holes.0))
            })
            .collect();

        let pattern = pattern.clone().rename_holes(pattern_holes);

        for (rewrite_entity, rewrite) in rewrites.iter() {
            if spent_rewrites.contains(&rewrite_entity) {
                continue;
            }
            if let Ok(_bindings) = rewrite.0.bind(&pattern) {
                candidate_rewrites.push((rewrite.clone(), rewrite_entity));
            }
        }
        candidate_rewrites
            .sort_by(|(r_a, _), (r_b, _)| r_a.0.complexity().cmp(&r_b.0.complexity()));
        if let Some((rewrite, rewrite_entity)) = candidate_rewrites.pop() {
            if let Ok(bindings) = rewrite.0.bind(&pattern) {
                println!("Bindings {:?}", bindings);
                commands.despawn_recursive(pattern_entity);
                spent_rewrites.insert(rewrite_entity);

                if rewrite == rewrite_rewrite() {
                    let bindings_map: BTreeMap<_, _> = bindings.iter().cloned().collect();
                    if bindings_map.len() != bindings.len() {
                        panic!("Unification is not supported yet");
                    }

                    if let (Some(pattern), Some(rewrite)) = (
                        bindings_map.get("pattern").cloned(),
                        bindings_map.get("rewrite").cloned(),
                    ) {
                        spawn_rewrite(commands, &materials, &font, Rewrite(pattern, rewrite));
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
                    commands.despawn_recursive(rewrite_entity);
                    spawn_pattern(commands, &materials, &font, rewritten_pattern);
                }
            }
        }
    }
}

fn ars(
    time: Res<Time>,
    mut timer: ResMut<RewriteTimer>,
    holes: ResMut<Holes>,
    commands: &mut Commands,
    materials: Res<Materials>,
    font: Res<ARSFont>,
    rewrites: Query<(Entity, &Rewrite), With<ARS>>,
    free_patterns: Query<(Entity, &Pattern), With<ARS>>,
) {
    if !timer.0.tick(time.delta_seconds()).just_finished() {
        return;
    }

    step_ars(commands, holes, materials, font, rewrites, free_patterns);
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
