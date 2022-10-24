#![feature(iter_intersperse)]
#![feature(if_let_guard)]

use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fs::File;
use std::io::BufReader;

use bevy::input::ButtonState;
use bevy::input::{
    keyboard::KeyCode,
    mouse::{MouseButtonInput, MouseMotion, MouseScrollUnit, MouseWheel},
    Input,
};
use bevy::prelude::*;
use bevy::render::camera::Camera;
use bevy::text::Text2dSize;
use bevy::window::CursorMoved;
use bevy_egui::{egui, EguiContext, EguiPlugin};
use bevy_inspector_egui::WorldInspectorPlugin;
use serde::{Deserialize, Serialize};

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        // .add_plugin(WorldInspectorPlugin::new())
        .add_plugin(EguiPlugin)
        .add_startup_system(setup)
        .add_event::<CompileEvent>()
        .add_startup_stage("ars_setup", SystemStage::single(spawn_initial_state))
        .add_system(compile_input)
        .add_system(listener_prompt)
        .add_system(keyboard_input_system)
        .add_system(attract_matching_patterns_and_rewrites)
        .add_system(update_listener)
        .add_system(ars)
        // .add_system(persistence)
        .add_system(ars_layout)
        .add_system(propagate_bboxes)
        .add_system(ars_term_bg)
        .add_system(rewrite_layout)
        .add_system(print_mouse_events_system)
        .add_system(focus_system)
        .add_system(kinematics_system)
        .run();
}

fn rewrite_rewrite() -> Rewrite {
    Rewrite(
        Pattern::parse("?pattern -> ?rewrite"),
        Pattern::parse("<defined>"),
    )
}

fn fork_rewrite() -> Rewrite {
    Rewrite(
        Pattern::parse("fork ?left ?right"),
        Pattern::parse("<?left and ?right as individual entities>"),
    )
}

fn spawn_rewrite(
    image: &Image,
    pos: Vec2,
    commands: &mut Commands,
    materials: &Materials,
    font: &ARSFont,
    rewrite: Rewrite,
) {
    commands
        .spawn()
        .insert_bundle(SpatialBundle::default())
        .insert_bundle((
            Ars,
            rewrite.clone(),
            Kinematics {
                pos,
                ..Default::default()
            },
            BBox::default(),
            Force(Vec2::ZERO),
        ))
        .with_children(|parent| {
            parent
                .spawn()
                .insert_bundle(Text2dBundle {
                    text: Text::from_section(
                        rewrite.0.pprint(image),
                        TextStyle {
                            font: font.0.clone(),
                            font_size: 24.0,
                            color: Color::WHITE,
                        },
                    )
                    .with_alignment(TextAlignment {
                        vertical: VerticalAlign::Top,
                        horizontal: HorizontalAlign::Left,
                    }),
                    ..Default::default()
                })
                .insert(TextWithBG)
                .insert(FollowParent)
                .insert(TopPattern)
                .insert(BBox::default())
                .with_children(|parent| {
                    parent
                        .spawn()
                        .insert_bundle(SpriteBundle {
                            sprite: Sprite {
                                color: materials.rewrite_color,
                                ..Default::default()
                            },
                            ..Default::default()
                        })
                        .insert(PatternBG)
                        .insert(FollowParent)
                        .insert(BBox::default());
                });
            parent
                .spawn()
                .insert_bundle(SpriteBundle {
                    sprite: Sprite {
                        color: materials.surfboard_line_color,
                        ..Default::default()
                    },
                    ..Default::default()
                })
                .insert(SurfboardLine)
                .insert(FollowParent)
                .insert(BBox::default());
            parent
                .spawn()
                .insert_bundle(Text2dBundle {
                    text: Text::from_section(
                        rewrite.1.pprint(image),
                        TextStyle {
                            font: font.0.clone(),
                            font_size: 24.0,
                            color: Color::WHITE,
                        },
                    )
                    .with_alignment(TextAlignment {
                        vertical: VerticalAlign::Top,
                        horizontal: HorizontalAlign::Left,
                    }),
                    ..Default::default()
                })
                .insert(TextWithBG)
                .insert(FollowParent)
                .insert(BottomPattern)
                .insert(BBox::default())
                .with_children(|parent| {
                    parent
                        .spawn()
                        .insert_bundle(SpriteBundle {
                            sprite: Sprite {
                                color: materials.rewrite_color,
                                ..Default::default()
                            },
                            ..Default::default()
                        })
                        .insert(PatternBG)
                        .insert(FollowParent)
                        .insert(BBox::default());
                });
        });
}

fn spawn_pattern(
    image: &Image,
    pos: Vec2,
    commands: &mut Commands,
    materials: &Materials,
    font: &ARSFont,
    pattern: Pattern,
) {
    commands
        .spawn()
        .insert_bundle(SpatialBundle::default())
        .insert_bundle(Text2dBundle {
            text: Text::from_section(
                pattern.pprint(image),
                TextStyle {
                    font: font.0.clone(),
                    font_size: 24.0,
                    color: Color::WHITE,
                },
            )
            .with_alignment(TextAlignment {
                vertical: VerticalAlign::Top,
                horizontal: HorizontalAlign::Left,
            }),
            ..Default::default()
        })
        .with_children(|parent| {
            parent
                .spawn()
                .insert_bundle(SpriteBundle {
                    sprite: Sprite {
                        color: materials.pattern_color,
                        ..Default::default()
                    },
                    ..Default::default()
                })
                .insert(PatternBG)
                .insert(BBox::default());
        })
        .insert(TextWithBG)
        .insert(Ars)
        .insert(pattern)
        .insert(Kinematics {
            pos,
            ..Default::default()
        })
        .insert(BBox::default())
        .insert(Force(Vec2::ZERO));
}

struct ARSFont(Handle<Font>);

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn().insert_bundle(Camera2dBundle::default());
    commands.insert_resource(Image::default());
    commands.insert_resource(ARSTimer(Timer::from_seconds(0.02, true)));
    let mut rewrite_timer = Timer::from_seconds(0.5, true);
    rewrite_timer.pause();
    commands.insert_resource(RewriteTimer(rewrite_timer));
    commands.insert_resource(PersistenceTimer(Timer::from_seconds(5.0, true)));
    commands.insert_resource(ListenerState::default());
    commands.insert_resource(Pointer::default());
    commands.insert_resource(Materials {
        pattern_color: Color::rgba(0.0, 0.0, 0.0, 0.3),
        rewrite_color: Color::rgba(1.0, 0.8, 0.1, 0.1),
        surfboard_line_color: Color::rgb(0.0, 0.0, 0.0),
    });
    commands.insert_resource(ARSFont(asset_server.load("fonts/iosevka-medium.ttf")));
}

fn spawn_initial_state(
    mut commands: Commands,
    mut image: ResMut<Image>,
    materials: Res<Materials>,
    font: Res<ARSFont>,
) {
    spawn_rewrite(
        &image,
        Kinematics::random().pos,
        &mut commands,
        &materials,
        &font,
        fork_rewrite(),
    );
    spawn_rewrite(
        &image,
        Kinematics::random().pos,
        &mut commands,
        &materials,
        &font,
        rewrite_rewrite(),
    );

    if let Ok(reader) = File::open("image.nimic").map(BufReader::new) {
        let (pats, rewrites, loaded_image): (Vec<Pattern>, Vec<Rewrite>, Image) =
            bincode::deserialize_from(reader).unwrap();

        image.update(loaded_image);

        for pat in pats {
            spawn_pattern(
                &image,
                Kinematics::random().pos * 10.0,
                &mut commands,
                &materials,
                &font,
                pat,
            );
        }

        for rewrite in rewrites {
            spawn_rewrite(
                &image,
                Kinematics::random().pos * 10.0,
                &mut commands,
                &materials,
                &font,
                rewrite,
            );
        }
    }
}

fn ars_layout(
    timer: Res<ARSTimer>,
    bboxes: Query<(Entity, &BBox), With<Ars>>,
    mut kinematics: Query<&mut Kinematics, With<Ars>>,
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

            // let buffer = 5.0;
            // let bbox_1 = bbox_1.buffer(buffer);
            // let bbox_2 = bbox_2.buffer(buffer);

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
                .unwrap()
                * 0.5;

            if let Ok(mut e_kin) = kinematics.get_mut(e_1) {
                e_kin.vel += push_vec + Kinematics::random().vel * 0.1;
            }
            if let Ok(mut e_kin) = kinematics.get_mut(e_2) {
                e_kin.vel -= push_vec + Kinematics::random().vel * 0.1;
            }
        }
    }
}

fn kinematics_system(
    time: Res<Time>,
    mut timer: ResMut<ARSTimer>,
    mut kinematics: Query<(Entity, &mut Kinematics)>,
    mut forces: Query<(Entity, &mut Force)>,
    mut positions: Query<(&BBox, &mut GlobalTransform)>,
) {
    if !timer.0.tick(time.delta()).just_finished() {
        return;
    }

    for (e, mut f) in forces.iter_mut() {
        let (_, mut k) = kinematics.get_mut(e).unwrap();
        k.vel += f.0;
        f.0 = Vec2::ZERO;
    }

    for (e, mut k) in kinematics.iter_mut() {
        k.vel *= 0.9;
        k.pos = k.pos + k.vel * time.delta_seconds();

        if let Ok((bbox, mut transform)) = positions.get_mut(e) {
            transform.translation_mut().x = k.pos.x + bbox.width() * 0.5;
            transform.translation_mut().y = k.pos.y + bbox.height() * 0.5;
        }
    }
}

fn ars_term_bg(
    timer: Res<ARSTimer>,
    text_size_q: Query<(&Text2dSize, &Children), With<TextWithBG>>,
    mut sprites: Query<&mut Transform, With<PatternBG>>,
) {
    if !timer.0.just_finished() {
        return;
    }

    for (text_size, children) in text_size_q.iter() {
        for child in children.iter() {
            if let Ok(mut trans) = sprites.get_mut(*child) {
                let margin = 0.0;
                trans.translation.x = text_size.size.x * 0.5;
                trans.translation.y = -text_size.size.y * 0.5;
                trans.scale = Vec3::new(text_size.size.x + margin, text_size.size.y + margin, 0.0);
            }
        }
    }
}

fn rewrite_layout(
    timer: Res<ARSTimer>,
    terms: Query<&Children, With<Rewrite>>,
    top_patterns: Query<&Text2dSize, With<TopPattern>>,
    bottom_patterns: Query<&Text2dSize, With<BottomPattern>>,
    mut trans_q: ParamSet<(
        Query<&mut Transform, With<TopPattern>>,
        Query<&mut Transform, With<BottomPattern>>,
        Query<&mut Transform, With<SurfboardLine>>,
    )>,
) {
    if !timer.0.just_finished() {
        return;
    }

    for children in terms.iter() {
        let mut top_height = None;
        let mut top_width = None;
        let mut bottom_width = None;
        for child in children.iter() {
            if let Ok(text_size) = top_patterns.get(*child) {
                top_height = Some(text_size.size.y);
                top_width = Some(text_size.size.x);
            }
            if let Ok(text_size) = bottom_patterns.get(*child) {
                bottom_width = Some(text_size.size.x);
            }
        }
        if let (Some(top_h), Some(top_w), Some(bottom_w)) = (top_height, top_width, bottom_width) {
            let rewrite_w = top_w.max(bottom_w);
            let buffer = 5.0;
            let surfboard_w = rewrite_w + 20.0;
            for child in children.iter() {
                if let Ok(mut trans) = trans_q.p2().get_mut(*child) {
                    trans.scale = Vec3::new(surfboard_w, 1.5, 0.0);
                    trans.translation.y = -top_h - buffer;
                    trans.translation.x = rewrite_w * 0.5;
                }
                if let Ok(mut trans) = trans_q.p1().get_mut(*child) {
                    trans.translation.y = -(top_h + buffer * 2.0);
                    trans.translation.x = (rewrite_w - bottom_w) * 0.5;
                }
                if let Ok(mut trans) = trans_q.p0().get_mut(*child) {
                    trans.translation.x = (rewrite_w - top_w) * 0.5;
                }
            }
        }
    }
}

fn propagate_bboxes(
    timer: Res<ARSTimer>,
    mut bboxes: Query<&mut BBox>,
    entities: Query<Entity, With<BBox>>,
    transforms: Query<&Transform, With<BBox>>,
    parents: Query<(Entity, &Children), With<BBox>>,
    global_transforms: Query<&GlobalTransform, With<BBox>>,
    roots: Query<Entity, Without<Parent>>,
) {
    if !timer.0.just_finished() {
        return;
    }
    for e in entities.iter() {
        if let (Ok(mut bbox), Ok(trans)) = (bboxes.get_mut(e), transforms.get(e)) {
            bbox.upper_right = trans.scale.truncate() * 0.5 + trans.translation.truncate();
            bbox.lower_left = -trans.scale.truncate() * 0.5 + trans.translation.truncate();
            assert!(bbox.height() >= 0.0, "{:?}", bbox);
            assert!(bbox.width() >= 0.0, "{:?}", bbox);
        }
    }

    for (parent, children) in parents.iter() {
        let mut parent_bbox = BBox::default();

        for child in children.iter() {
            if let Ok(child_bbox) = bboxes.get_mut(*child) {
                parent_bbox.upper_right = child_bbox.upper_right.max(parent_bbox.upper_right);
                parent_bbox.lower_left = child_bbox.lower_left.min(parent_bbox.lower_left);
                assert!(parent_bbox.height() >= 0.0, "{:?}", parent_bbox);
                assert!(parent_bbox.width() >= 0.0, "{:?}", parent_bbox);
            } else {
                println!("skipping child {:?}", child);
            }
        }

        let mut global_translation = if roots.get(parent).is_ok() {
            global_transforms
                .get(parent)
                .map(|t| *t)
                .ok()
                .unwrap_or_default()
                .translation_mut()
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

fn print_mouse_events_system(
    mut pointer: ResMut<Pointer>,
    mut camera_query: Query<(&Camera, &mut GlobalTransform)>,
    mut mouse_button_input_events: EventReader<MouseButtonInput>,
    mut mouse_motion_events: EventReader<MouseMotion>,
    mut cursor_moved_events: EventReader<CursorMoved>,
    mut mouse_wheel_events: EventReader<MouseWheel>,
    holdable_entities: Query<(Entity, &BBox), With<Ars>>,
) {
    for event in mouse_button_input_events.iter() {
        match event {
            MouseButtonInput {
                button: MouseButton::Left,
                state,
            } => {
                pointer.drag_start = pointer.pos;
                pointer.down = state == &ButtonState::Pressed;
            }
            event => println!("{:?}", event),
        }
    }

    for _event in mouse_motion_events.iter() {}

    for event in cursor_moved_events.iter() {
        pointer.pos = event.position;
    }

    for event in mouse_wheel_events.iter() {
        match event {
            MouseWheel {
                unit: MouseScrollUnit::Line | MouseScrollUnit::Pixel,
                x,
                y,
            } => {
                if let Some((_, mut cam_trans)) = camera_query.iter_mut().next() {
                    cam_trans.translation_mut().x += x;
                    cam_trans.translation_mut().y += y;
                }
            }
        }
    }

    if !pointer.down {
        pointer.holding = None;
    }
    if let Some((camera, cam_trans)) = camera_query.iter_mut().next() {
        if pointer.down && pointer.holding.is_none() {
            for (entity, bbox) in holdable_entities.iter() {
                if let (Some(upper_right), Some(lower_left)) = (
                    camera.world_to_viewport(&cam_trans, bbox.upper_right.extend(0.)),
                    camera.world_to_viewport(&cam_trans, bbox.lower_left.extend(0.)),
                ) {
                    let bbox_on_screen = BBox {
                        upper_right,
                        lower_left,
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
    timer: Res<ARSTimer>,
    pointer: Res<Pointer>,
    mut terms: Query<(&BBox, &mut Kinematics), With<Ars>>,
    camera_query: Query<(&Camera, &GlobalTransform)>,
) {
    if !timer.0.just_finished() {
        return;
    }

    if let (Some((camera, cam_trans)), Some(holding_entity)) =
        (camera_query.iter().next(), pointer.holding)
    {
        if let Ok((bbox, mut kin)) = terms.get_mut(holding_entity) {
            if let (Some(upper_right), Some(lower_left)) = (
                camera.world_to_viewport(cam_trans, bbox.upper_right.extend(0.)),
                camera.world_to_viewport(cam_trans, bbox.lower_left.extend(0.)),
            ) {
                let bbox_on_screen = BBox {
                    upper_right,
                    lower_left,
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
    image: Res<Image>,
    rewrites: Query<&Rewrite, With<Ars>>,
    free_patterns: Query<&Pattern, With<Ars>>,
) {
    if !timer.0.tick(time.delta()).just_finished() {
        return;
    }

    let image_file = "image.nimic";
    let image_file_tmp = "image.nimic.tmp";
    {
        let tmp = File::create(image_file_tmp).unwrap();

        // TODO: we may not need cloned here if Serialize is implemented for &T
        let pats: Vec<Pattern> = free_patterns.iter().cloned().collect();
        let rewrites: Vec<Rewrite> = rewrites
            .iter()
            .cloned()
            .filter(|r| !r.is_primitive())
            .collect();

        if let Err(e) = bincode::serialize_into(tmp, &(pats, rewrites, image.clone())) {
            println!("Failed save image: {:?}", e);
            return;
        }
    }

    std::fs::rename(image_file_tmp, image_file).unwrap();
    println!("Saved image");
}

#[derive(Component, Default, Debug)]
struct ListenerState {
    command: String,
    history: Vec<Pattern>,
}

struct CompileEvent;
#[derive(Component)]
struct ARSTimer(Timer);
#[derive(Component)]
struct RewriteTimer(Timer);
#[derive(Component)]
struct PersistenceTimer(Timer);
#[derive(Component)]
struct Ars;
#[derive(Component)]
struct PatternBG;
#[derive(Component)]
struct SurfboardLine;
#[derive(Component)]
struct TopPattern;
#[derive(Component)]
struct BottomPattern;
#[derive(Component)]
struct FollowParent;
#[derive(Component)]
struct TextWithBG;

#[derive(Default, Debug, Component)]
struct Pointer {
    down: bool,
    pos: Vec2,
    drag_start: Vec2,
    holding: Option<Entity>,
}
#[derive(Default, Debug, Component)]
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

    fn double_left(&self) -> Vec2 {
        self.center() + Vec2::new(-self.width() * 0.5, 0.0) * 2.0
    }

    fn contains(&self, p: Vec2) -> bool {
        p.x <= self.upper_right.x
            && p.x >= self.lower_left.x
            && p.y <= self.upper_right.y
            && p.y >= self.lower_left.y
    }

    fn overlaps(&self, other: &Self) -> bool {
        let delta = other.center() - self.center();

        delta.x.abs() < (self.width() + other.width()) * 0.5
            && delta.y.abs() < (self.height() + other.height()) * 0.5
    }
}

#[derive(Component)]
struct Materials {
    pattern_color: Color,
    rewrite_color: Color,
    surfboard_line_color: Color,
}

#[derive(Debug, Default, Clone, Component)]
struct Kinematics {
    pos: Vec2,
    vel: Vec2,
}

#[derive(Debug, Default, Clone, Copy, Component)]
struct Force(Vec2);

impl Kinematics {
    fn random() -> Self {
        use rand::Rng;
        let mut thread_rng = rand::thread_rng();

        Self {
            pos: Vec2::new(
                thread_rng.gen_range(-1.0..1.0),
                thread_rng.gen_range(-1.0..1.0),
            ) * 4.,
            vel: Vec2::new(
                thread_rng.gen_range(-1.0..1.0),
                thread_rng.gen_range(-1.0..1.0),
            ) * 0.1,
        }
    }
}

#[derive(Default, Debug, Clone, Serialize, Deserialize, Component)]
struct Image {
    next_hole: u64,
    next_reference: u64,
    patterns: Vec<Pattern>,
    references: BTreeMap<u64, Pattern>,
}

impl Image {
    fn update(&mut self, other: Self) {
        self.next_hole = other.next_hole.max(self.next_hole);
        self.next_reference = other.next_reference.max(self.next_reference);
        self.references.extend(other.references);
    }

    fn alloc_hole(&mut self) -> String {
        let hole = self.next_hole;
        self.next_hole += 1;
        format!("{}", hole)
    }

    fn alloc_ref(&mut self) -> u64 {
        let reference = self.next_reference;
        self.next_reference += 1;
        reference
    }

    fn store_ref(&mut self, reference: u64, pat: Pattern) {
        self.references.insert(reference, pat);
    }

    fn deref(&self, reference: u64) -> Option<&Pattern> {
        self.references.get(&reference)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Component)]
struct Rewrite(Pattern, Pattern);

impl Rewrite {
    fn is_primitive(&self) -> bool {
        self == &rewrite_rewrite() || self == &fork_rewrite()
    }

    fn pprint(&self, image: &Image) -> String {
        format!("[{} -> {}]", self.0.pprint(image), self.1.pprint(image))
    }
}

impl std::fmt::Display for Rewrite {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} -> {}]", self.0, self.1)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, PartialOrd, Ord, Serialize, Deserialize, Component)]
enum Pattern {
    Sym(String),       // ==> concrete value
    Hole(String),      // ==> ?var
    Seq(Vec<Pattern>), // ==> [ x y ?v ]
    Ref(u64),          // ==> ~(1132)  -- reference to another pattern
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
            Self::Ref(r) => write!(f, "~({})", r),
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
            .filter(|t| {
                !matches!(t,
                    Self::Sym(s) if s.chars().all(char::is_whitespace)
                )
            })
            .collect()
    }
}

impl From<Vec<Pattern>> for Pattern {
    fn from(seq: Vec<Pattern>) -> Self {
        Self::Seq(seq)
    }
}

impl From<&str> for Pattern {
    fn from(s: &str) -> Self {
        Self::parse(s)
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

    fn pprint(&self, image: &Image) -> String {
        self.pprint_indented(image, 0, false, &mut Default::default())
    }

    fn pprint_indented(
        &self,
        image: &Image,
        indent_level: usize,
        parent_wrapped: bool,
        expanded_refs: &mut BTreeMap<u64, usize>,
    ) -> String {
        let indent: String = " ".to_string().repeat(indent_level);
        match self {
            Self::Sym(s) => s.to_string(),
            Self::Hole(h) => format!("?{}", h),
            Self::Seq(seq) => {
                let mut cumulative_complexity = 0;
                let mut wrapped = false;
                let mut pprint = format!("{}[", if parent_wrapped { &indent } else { "" });
                for pat in seq.iter().intersperse(&Self::Sym(" ".into())) {
                    cumulative_complexity += pat.complexity();
                    if cumulative_complexity > 15 {
                        wrapped = true;
                        if pat != &Self::Sym(" ".into()) {
                            pprint = format!("{}\n{}", pprint, indent);
                        }
                    }
                    pprint = format!(
                        "{}{}",
                        pprint,
                        pat.pprint_indented(
                            image,
                            indent_level + usize::from(wrapped),
                            wrapped,
                            &mut expanded_refs.clone()
                        )
                    );
                }
                pprint = format!("{}]", pprint);
                pprint
            }
            Self::Ref(r) => {
                let times_expanded = expanded_refs.entry(*r).or_default();
                *times_expanded += 1;
                if *times_expanded > 2 {
                    "...".to_string()
                } else {
                    image.deref(*r).unwrap().pprint_indented(
                        image,
                        indent_level,
                        parent_wrapped,
                        expanded_refs,
                    )
                }
            }
        }
    }

    fn unify(&self, other: &Self, image: &mut Image) -> Result<BTreeMap<String, Self>, ()> {
        let mut bindings =
            self.unify_rec(other, Default::default(), image, &mut Default::default())?;
        let mut roots = self.holes(image);
        roots.extend(other.holes(image));

        let cycle_breaks: BTreeMap<_, _> = self
            .cycles(&bindings)
            .into_iter()
            .map(|cyclic_hole| (cyclic_hole, image.alloc_ref()))
            .collect();

        for (cyclic_hole, r) in cycle_breaks.iter() {
            let ref_pat = Pattern::Ref(*r);
            for (_, pat) in bindings.iter_mut() {
                pat.replace_hole(cyclic_hole, ref_pat.clone());
            }
        }
        for (cyclic_hole, r) in cycle_breaks {
            let cyclic_pat = bindings.remove(&cyclic_hole).unwrap();
            bindings.insert(cyclic_hole, Pattern::Ref(r));
            image.store_ref(r, cyclic_pat);
        }

        self.inline_bindings(&roots, &mut bindings, image);

        Ok(bindings)
    }

    fn unify_rec(
        &self,
        other: &Self,
        mut bindings: BTreeMap<String, Self>,
        image: &mut Image,
        ref_memo: &mut BTreeSet<(Self, Self)>,
    ) -> Result<BTreeMap<String, Self>, ()> {
        let unified = match (self, other) {
            (Self::Sym(a), Self::Sym(b)) => {
                if a == b {
                    Ok(bindings)
                } else {
                    Err(())
                }
            }
            (Self::Seq(seq_a), Self::Seq(seq_b)) => {
                if seq_a.len() != seq_b.len() {
                    Err(())
                } else {
                    for (a, b) in seq_a.iter().zip(seq_b.iter()) {
                        bindings = a.unify_rec(b, bindings, image, ref_memo)?;
                    }
                    Ok(bindings)
                }
            }
            (Self::Hole(a), Self::Hole(b)) => {
                if a == b {
                    Ok(bindings)
                } else {
                    match (bindings.remove(a), bindings.remove(b)) {
                        (None, None) => {
                            let a_b_hole = image.alloc_hole();
                            bindings.insert(a.to_string(), Pattern::Hole(a_b_hole.clone()));
                            bindings.insert(b.to_string(), Pattern::Hole(a_b_hole));
                            Ok(bindings)
                        }
                        (Some(pat), None) | (None, Some(pat)) => {
                            let a_b_hole = image.alloc_hole();
                            bindings.insert(a.to_string(), Pattern::Hole(a_b_hole.clone()));
                            bindings.insert(b.to_string(), Pattern::Hole(a_b_hole.clone()));
                            bindings.insert(a_b_hole, pat);
                            Ok(bindings)
                        }
                        (Some(a_pat), Some(b_pat)) => {
                            bindings.insert(a.to_string(), a_pat.clone());
                            bindings.insert(b.to_string(), b_pat.clone());
                            a_pat.unify_rec(&b_pat, bindings, image, ref_memo)
                        }
                    }
                }
            }
            (Self::Hole(h), pat) | (pat, Self::Hole(h)) => match bindings.get(h) {
                Some(prev_match) => prev_match.clone().unify_rec(pat, bindings, image, ref_memo),
                None => {
                    bindings.insert(h.clone(), pat.clone());
                    Ok(bindings)
                }
            },
            (Self::Ref(r), pat) | (pat, Self::Ref(r)) => {
                if ref_memo.contains(&(
                    Self::Ref(*r).min(pat.clone()),
                    Self::Ref(*r).max(pat.clone()),
                )) {
                    Ok(bindings)
                } else {
                    ref_memo.insert((
                        Self::Ref(*r).min(pat.clone()),
                        Self::Ref(*r).max(pat.clone()),
                    ));
                    let r_pat = image.deref(*r).unwrap().clone();
                    pat.unify_rec(&r_pat, bindings, image, ref_memo)
                }
            }
            _ => Err(()),
        };
        unified
    }

    fn inline_bindings(
        &self,
        roots: &BTreeSet<String>,
        bindings: &mut BTreeMap<String, Self>,
        image: &mut Image,
    ) {
        loop {
            let mut changed = false;

            for (hole, pat) in bindings.clone().into_iter() {
                let no_common_holes = pat
                    .holes_no_deref()
                    .intersection(&bindings.keys().cloned().collect())
                    .next()
                    .is_none();
                if no_common_holes {
                    // this pattern has no holes which are bindings.
                    match pat {
                        Pattern::Hole(pat_hole) if !roots.contains(&pat_hole) => {
                            for (_, other_pat) in bindings.iter_mut() {
                                changed |=
                                    other_pat.replace_hole(&pat_hole, Pattern::Hole(hole.clone()))
                            }
                        }
                        pat => {
                            for (_, other_pat) in bindings.iter_mut() {
                                changed |= other_pat.replace_hole(&hole, pat.clone())
                            }
                        }
                    }
                }
            }

            for (_, pat) in bindings.iter_mut() {
                changed |= pat.inline_indirect_refs(image)
            }

            for (r, mut pat) in image.references.clone().into_iter() {
                if pat.inline_indirect_refs(image) {
                    changed = true;
                    image.store_ref(r, pat);
                }
            }

            if !changed {
                break;
            }
        }

        let non_root_bindings = &bindings.keys().cloned().collect::<BTreeSet<_>>() - roots;
        for non_root_binding in non_root_bindings {
            if bindings
                .iter()
                .all(|(_, p)| !p.holes(image).contains(&non_root_binding))
            {
                bindings.remove(&non_root_binding);
            }
        }
    }

    fn inline_indirect_refs(&mut self, image: &Image) -> bool {
        match self {
            Self::Ref(r) => match image.deref(*r) {
                Some(Self::Ref(s)) => {
                    *r = *s;
                    true
                }
                _ => false,
            },
            Self::Seq(seq) => seq.iter_mut().any(|p| p.inline_indirect_refs(image)),
            _ => false,
        }
    }

    /// Returns all holes who directly or indirectly refer to themselves
    fn cycles(&self, bindings: &BTreeMap<String, Self>) -> BTreeSet<String> {
        let mut reachable: BTreeMap<String, BTreeSet<String>> = Default::default();
        for (hole, pattern) in bindings.iter() {
            let pattern_holes = pattern.holes_no_deref();
            reachable
                .entry(hole.clone())
                .or_default()
                .extend(pattern_holes);
        }
        let mut reachable_set_changed = true;
        while reachable_set_changed {
            let mut to_expand = Vec::new();
            for (parent_hole, child_holes) in reachable.iter() {
                for child in child_holes.iter() {
                    if let Some(child_child_holes) = reachable.get(child) {
                        if !(child_child_holes - child_holes).is_empty() {
                            to_expand.push((parent_hole.to_string(), child.to_string()))
                        }
                    }
                }
            }
            reachable_set_changed = !to_expand.is_empty();
            for (par, child) in to_expand {
                if let Some(holes) = reachable.get(&child).cloned() {
                    reachable.entry(par).or_default().extend(holes)
                }
            }
        }

        reachable
            .into_iter()
            .filter(|(h, refs)| refs.contains(h))
            .map(|(h, _)| h)
            .collect()
    }

    fn holes_no_deref(&self) -> BTreeSet<String> {
        let mut holes = Default::default();
        self.holes_rec_no_deref(&mut holes);
        holes
    }

    fn holes_rec_no_deref(&self, holes: &mut BTreeSet<String>) {
        match self {
            Self::Hole(hole) => {
                holes.insert(hole.clone());
            }
            Self::Seq(seq) => seq.iter().for_each(|p| p.holes_rec_no_deref(holes)),
            _ => (),
        }
    }

    fn holes(&self, image: &Image) -> BTreeSet<String> {
        let mut holes = Default::default();
        self.holes_rec(&mut holes, image, &mut Default::default());
        holes
    }

    fn holes_rec(&self, holes: &mut BTreeSet<String>, image: &Image, memo: &mut BTreeSet<u64>) {
        match self {
            Self::Hole(hole) => {
                holes.insert(hole.clone());
            }
            Self::Seq(seq) => seq.iter().for_each(|p| p.holes_rec(holes, image, memo)),
            Self::Ref(r) if !memo.contains(r) => {
                memo.insert(*r);
                image.deref(*r).unwrap().holes_rec(holes, image, memo);
            }
            _ => (),
        }
    }

    fn replace_holes(&mut self, holes: BTreeMap<String, Self>) -> bool {
        let mut changed = false;
        for (hole, pat) in holes {
            changed |= self.replace_hole(&hole, pat);
        }
        changed
    }

    fn replace_hole(&mut self, hole: &str, pat: Self) -> bool {
        match self {
            Self::Hole(h) if h == hole => {
                *self = pat;
                true
            }
            Self::Seq(seq) => seq
                .iter_mut()
                .map(|p| p.replace_hole(hole, pat.clone()))
                .fold(false, |changed, replace_res| changed | replace_res),
            _ => false,
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
            Self::Sym(_) => 1,
            Self::Hole(_) => 2,
            Self::Seq(seq) => seq.iter().map(Self::complexity).sum::<usize>() + 1,
            Self::Ref(_) => 9,
        }
    }
}

fn listener_prompt(
    mut commands: Commands,
    mut compile_events: EventWriter<CompileEvent>,
    mut rewrite_timer: ResMut<RewriteTimer>,
    image: ResMut<Image>,
    materials: Res<Materials>,
    font: Res<ARSFont>,
    rewrites: Query<(Entity, &Rewrite, &BBox), With<Ars>>,
    free_patterns: Query<(Entity, &Pattern, &BBox), With<Ars>>,
    mut listener_state: ResMut<ListenerState>,
    mut egui_context: ResMut<EguiContext>,
) {
    let ctx = egui_context.ctx_mut();
    let mut fonts = egui::FontDefinitions::default();
    // fonts.family_and_size.insert(
    //     egui::TextStyle::Monospace,
    //     (egui::FontFamily::Monospace, 24.0),
    // );
    // fonts.
    // ctx.set_fonts(fonts);

    egui::Window::new("Log Book").vscroll(true).show(ctx, |ui| {
        for prev_pat in listener_state.history.clone().iter().rev() {
            let pprinted_pat = prev_pat.pprint(&image);
            if ui.button(&pprinted_pat).clicked() {
                listener_state.command = pprinted_pat;
            }
        }
    });
    egui::Window::new("Listener").show(ctx, |ui| {
        ui.add(
            egui::TextEdit::multiline(&mut listener_state.command).font(egui::FontId {
                size: 24.,
                family: egui::FontFamily::Monospace,
            }),
        );
        ui.horizontal(|ui| {
            if ui.button("compile").clicked() && !listener_state.command.is_empty() {
                compile_events.send(CompileEvent);
            }
            if ui.button("format").clicked() {
                listener_state.command = Pattern::parse(&listener_state.command).pprint(&image);
            }
            if ui.button("clear").clicked() {
                for (e, r, _) in rewrites.iter() {
                    if !r.is_primitive() {
                        commands.entity(e).despawn_recursive();
                    }
                }

                for (e, _, _) in free_patterns.iter() {
                    commands.entity(e).despawn_recursive();
                }
            }
            if rewrite_timer.0.paused() {
                if ui.button("unpause").clicked() {
                    rewrite_timer.0.unpause()
                }
            } else if ui.button("pause").clicked() {
                rewrite_timer.0.pause()
            }

            if ui.button("step").clicked() {
                step_ars(commands, image, materials, font, rewrites, free_patterns);
            }
        });
    });
}

fn update_listener(
    mut listener_state: ResMut<ListenerState>,
    image: Res<Image>,
    pointer: Res<Pointer>,
    rewrites: Query<&Rewrite>,
    patterns: Query<&Pattern>,
) {
    if let Some(holding_entity) = pointer.holding {
        if let Ok(rewrite) = rewrites.get(holding_entity) {
            listener_state.command = rewrite.pprint(&image)
        } else if let Ok(pat) = patterns.get(holding_entity) {
            listener_state.command = pat.pprint(&image)
        };
    }
}
fn compile_input(
    mut commands: Commands,
    mut compile_reader: EventReader<CompileEvent>,
    image: Res<Image>,
    materials: Res<Materials>,
    font: Res<ARSFont>,
    mut listener_state: ResMut<ListenerState>,
    camera: Query<&GlobalTransform, With<Camera>>,
) {
    if compile_reader.iter().next().is_some() && !listener_state.command.trim().is_empty() {
        let pattern = Pattern::parse(&listener_state.command);
        if let Some(p_idx) = listener_state.history.iter().position(|p| p == &pattern) {
            listener_state.history.remove(p_idx);
        }
        listener_state.history.push(pattern.clone());

        let camera_pos = camera
            .iter()
            .next()
            .map(|gt| gt.translation())
            .unwrap_or_default()
            .truncate();

        spawn_pattern(
            &image,
            Kinematics::random().pos * 10.0 + camera_pos,
            &mut commands,
            &materials,
            &font,
            pattern,
        );
        listener_state.command = Default::default();
    }
}

fn keyboard_input_system(
    mut compile_events: EventWriter<CompileEvent>,
    keyboard_input: Res<Input<KeyCode>>,
) {
    if (keyboard_input.pressed(KeyCode::LControl) || keyboard_input.pressed(KeyCode::RControl))
        && keyboard_input.pressed(KeyCode::Return)
    {
        compile_events.send(CompileEvent);
    }
}

fn attract_matching_patterns_and_rewrites(
    mut image: ResMut<Image>,
    timer: Res<ARSTimer>,
    rewrites: Query<(Entity, &Rewrite, &Kinematics, &BBox)>,
    patterns: Query<(Entity, &Pattern, &Kinematics, &BBox)>,
    mut forces: Query<(Entity, &mut Force)>,
) {
    if !timer.0.just_finished() {
        return;
    }

    let force = 2000.;
    for (r_e, rewrite, rewrite_kin, rewrite_bbox) in rewrites.iter() {
        if rewrite.is_primitive() {
            continue;
        }
        for (p_e, pattern, pattern_kin, pattern_bbox) in patterns.iter() {
            let delta = rewrite_bbox.center() - pattern_bbox.center();
            let dist = delta.length();
            if dist > 1e-6 {
                let force_vec = if rewrite.0.unify(pattern, &mut image).is_ok() {
                    delta / dist.max(100.0).powf(2.0)
                        * (force + (rewrite.0.complexity() as f32) * 10.0)
                } else if dist < 400. {
                    -delta / (dist + 1.5).powf(2.0) * 1000.
                } else {
                    Default::default()
                };
                let (_, mut f) = forces.get_mut(r_e).unwrap();
                f.0 -= force_vec;
                let (_, mut f) = forces.get_mut(p_e).unwrap();
                f.0 += force_vec;
            }
        }
    }
}

fn step_ars(
    mut commands: Commands,
    mut image: ResMut<Image>,
    materials: Res<Materials>,
    font: Res<ARSFont>,
    rewrites: Query<(Entity, &Rewrite, &BBox), With<Ars>>,
    free_patterns: Query<(Entity, &Pattern, &BBox), With<Ars>>,
) {
    let mut spent_rewrites: BTreeSet<Entity> = Default::default();

    for (pattern_entity, pattern, pattern_bbox) in free_patterns.iter() {
        let mut candidate_rewrites: Vec<(Rewrite, BTreeMap<String, Pattern>, Entity)> =
            Default::default();

        let pattern_holes: BTreeMap<_, _> = pattern
            .holes(&image)
            .into_iter()
            .map(|h| (h, image.alloc_hole()))
            .collect();
        let pattern_renamed = pattern.clone().rename_holes(pattern_holes.clone());

        for (rewrite_entity, rewrite, rewrite_bbox) in rewrites.iter() {
            if spent_rewrites.contains(&rewrite_entity) {
                continue;
            }

            if !rewrite_bbox.overlaps(pattern_bbox) && !rewrite.is_primitive() {
                continue;
            }

            if let Ok(bindings) = rewrite.0.unify(&pattern_renamed, &mut image) {
                let inverted_pattern_holes = pattern_holes
                    .clone()
                    .into_iter()
                    .map(|(a, b)| (b, a))
                    .collect::<BTreeMap<_, _>>();
                let bindings = bindings
                    .into_iter()
                    .map(|(b, pat)| {
                        let old_hole = inverted_pattern_holes.get(&b).cloned().unwrap_or(b);
                        let old_pat = pat.rename_holes(inverted_pattern_holes.clone());
                        (old_hole, old_pat)
                    })
                    .collect();
                candidate_rewrites.push((rewrite.clone(), bindings, rewrite_entity));
            }
        }
        candidate_rewrites
            .sort_by(|(r_a, _, _), (r_b, _, _)| r_a.0.complexity().cmp(&r_b.0.complexity()));

        if let Some((rewrite, bindings, rewrite_entity)) = candidate_rewrites.pop() {
            commands.entity(pattern_entity).despawn_recursive();
            if !rewrite.is_primitive() {
                spent_rewrites.insert(rewrite_entity);
            }

            if rewrite == rewrite_rewrite() {
                if let (Some(pattern), Some(rewrite)) = (
                    bindings.get("pattern").cloned(),
                    bindings.get("rewrite").cloned(),
                ) {
                    let spawn_position = pattern_bbox.double_left();
                    spawn_rewrite(
                        &image,
                        spawn_position,
                        &mut commands,
                        &materials,
                        &font,
                        Rewrite(pattern, rewrite),
                    );
                }
            } else if rewrite == fork_rewrite() {
                if let (Some(left), Some(right)) = (
                    bindings.get("left").cloned(),
                    bindings.get("right").cloned(),
                ) {
                    let spawn_position = pattern_bbox.double_left();

                    spawn_pattern(
                        &image,
                        spawn_position + Vec2::new(-0.1, 0.0),
                        &mut commands,
                        &materials,
                        &font,
                        left,
                    );
                    spawn_pattern(
                        &image,
                        spawn_position + Vec2::new(0.1, 0.0),
                        &mut commands,
                        &materials,
                        &font,
                        right,
                    );
                }
            } else {
                let spawn_position = pattern_bbox.double_left();
                let mut rewritten_pattern = rewrite.1.clone();
                rewritten_pattern.replace_holes(bindings);
                commands.entity(rewrite_entity).despawn_recursive();
                spawn_pattern(
                    &image,
                    spawn_position,
                    &mut commands,
                    &materials,
                    &font,
                    rewritten_pattern,
                );
            }
        }
    }
}

fn ars(
    time: Res<Time>,
    mut timer: ResMut<RewriteTimer>,
    image: ResMut<Image>,
    commands: Commands,
    materials: Res<Materials>,
    font: Res<ARSFont>,
    rewrites: Query<(Entity, &Rewrite, &BBox), With<Ars>>,
    free_patterns: Query<(Entity, &Pattern, &BBox), With<Ars>>,
) {
    if !timer.0.tick(time.delta()).just_finished() {
        return;
    }

    step_ars(commands, image, materials, font, rewrites, free_patterns);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unify_simple() {
        assert_eq!(
            Pattern::parse("a").unify(&Pattern::parse("b"), &mut Default::default()),
            Err(())
        );
        assert_eq!(
            Pattern::parse("a").unify(&Pattern::parse("a"), &mut Default::default()),
            Ok(Default::default())
        );

        assert_eq!(
            Pattern::parse("[a b]").unify(&Pattern::parse("[a b]"), &mut Default::default()),
            Ok(Default::default())
        );

        assert_eq!(
            Pattern::parse("[a [x y]]")
                .unify(&Pattern::parse("[a [x y]]"), &mut Default::default()),
            Ok(Default::default())
        );

        assert_eq!(
            Pattern::parse("[a [x z]]")
                .unify(&Pattern::parse("[a [x y]]"), &mut Default::default()),
            Err(())
        );

        assert_eq!(
            Pattern::parse("?x").unify(&Pattern::parse("1"), &mut Default::default()),
            Ok(vec![("x".into(), "1".into())].into_iter().collect())
        );

        assert_eq!(
            Pattern::parse("1").unify(&Pattern::parse("?x"), &mut Default::default()),
            Ok(vec![("x".into(), "1".into())].into_iter().collect())
        );

        assert_eq!(
            Pattern::parse("[?x 1]").unify(&Pattern::parse("[2 ?y]"), &mut Default::default()),
            Ok(vec![("x".into(), "2".into()), ("y".into(), "1".into())]
                .into_iter()
                .collect())
        );

        assert_eq!(
            Pattern::parse("[1 1]").unify(&Pattern::parse("[?x ?x]"), &mut Default::default()),
            Ok(vec![("x".into(), "1".into())].into_iter().collect())
        );

        assert_eq!(
            Pattern::parse("[1 2]").unify(&Pattern::parse("[?x ?x]"), &mut Default::default()),
            Err(())
        );

        assert_eq!(
            Pattern::parse("[?x 1]").unify(&Pattern::parse("[1 ?x]"), &mut Default::default()),
            Ok(vec![("x".into(), "1".into())].into_iter().collect())
        );

        assert_eq!(
            Pattern::parse("[?x 1]").unify(&Pattern::parse("[?x ?x]"), &mut Default::default()),
            Ok(vec![("x".into(), "1".into())].into_iter().collect())
        );

        assert_eq!(
            Pattern::parse("[?x 2]").unify(&Pattern::parse("[1 ?x]"), &mut Default::default()),
            Err(())
        );
    }

    #[test]
    fn unify_is_transitive() {
        assert_eq!(
            Pattern::parse("[?x ?x]").unify(&Pattern::parse("[?y 1]"), &mut Default::default()),
            Ok(vec![("x".into(), "1".into()), ("y".into(), "1".into())]
                .into_iter()
                .collect())
        );

        assert_eq!(
            Pattern::parse("[?x ?x]").unify(&Pattern::parse("[1 ?y]"), &mut Default::default()),
            Ok(vec![("x".into(), "1".into()), ("y".into(), "1".into())]
                .into_iter()
                .collect())
        );

        assert_eq!(
            Pattern::parse("[?x ?y ?x]")
                .unify(&Pattern::parse("[?z ?z 1]"), &mut Default::default()),
            Ok(vec![
                ("x".into(), "1".into()),
                ("y".into(), "1".into()),
                ("z".into(), "1".into())
            ]
            .into_iter()
            .collect())
        );
    }

    #[test]
    fn unify_recursive() {
        assert_eq!(
            Pattern::parse("[?x ?x]")
                .unify(&"[?self [x -> ?self]]".into(), &mut Default::default()),
            Ok(vec![
                ("x".into(), Pattern::Ref(0)),
                ("self".into(), Pattern::Ref(0))
            ]
            .into_iter()
            .collect())
        );
    }

    #[test]
    fn unify_corecursive() {
        assert_eq!(
            Pattern::parse("[?x ?y ?x ?y]").unify(
                &"[?self_a ?self_b [x -> ?self_b] [y -> ?self_a]]".into(),
                &mut Default::default()
            ),
            Ok(vec![
                ("x".into(), Pattern::Ref(0)),
                ("y".into(), Pattern::Ref(1)),
                ("self_a".into(), Pattern::Ref(0)),
                ("self_b".into(), Pattern::Ref(1))
            ]
            .into_iter()
            .collect())
        );
    }

    #[test]
    fn unify_through_ref() {
        let mut image = Default::default();
        let unified_bindings = Pattern::parse("[?x [?z ?x]]")
            .unify(&"[?y ?y]".into(), &mut image)
            .unwrap();

        assert_eq!(
            unified_bindings,
            vec![("x".into(), Pattern::Ref(0)), ("y".into(), Pattern::Ref(0)),]
                .into_iter()
                .collect()
        );

        assert_eq!(
            image.deref(0),
            Some(&vec!["?z".into(), Pattern::Ref(0)].into())
        );

        let mut unified = Pattern::parse("[?x [?z ?x]]");
        unified.replace_holes(unified_bindings);

        assert_eq!(
            unified,
            vec![Pattern::Ref(0), vec!["?z".into(), Pattern::Ref(0)].into()].into()
        );

        assert_eq!(
            image.deref(0),
            Some(&vec!["?z".into(), Pattern::Ref(0)].into())
        );

        assert_eq!(
            unified.unify(&"[?x [1 ?y]]".into(), &mut image),
            Ok(vec![
                ("x".into(), Pattern::Ref(0)),
                ("y".into(), Pattern::Ref(0)),
                ("z".into(), "1".into())
            ]
            .into_iter()
            .collect())
        );

        assert_eq!(
            image.deref(0),
            Some(&vec!["?z".into(), Pattern::Ref(0)].into())
        );

        assert_eq!(
            unified.unify(&"[[1 ?b] ?b]".into(), &mut image),
            Ok(
                vec![("b".into(), Pattern::Ref(0)), ("z".into(), "1".into())]
                    .into_iter()
                    .collect()
            )
        );
    }

    #[test]
    fn holes() {
        let image = Image::default();
        assert_eq!(
            Pattern::parse("[?x [?y 1]").holes(&image),
            vec!["x".into(), "y".into()].into_iter().collect()
        );
    }

    #[test]
    fn holes_through_ref() {
        let mut image = Image::default();
        let a = image.alloc_ref();
        let b = image.alloc_ref();
        image.store_ref(a, vec!["?x".into(), Pattern::Ref(b)].into());
        image.store_ref(b, vec!["?z".into(), Pattern::Ref(a)].into());
        assert_eq!(
            Pattern::Seq(vec!["?y".into(), Pattern::Ref(a)]).holes(&image),
            vec!["y".into(), "x".into(), "z".into()]
                .into_iter()
                .collect()
        );
    }
}
