#![feature(iter_intersperse)]

use std::collections::{BTreeMap, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader, Write};

use bevy::prelude::*;
use bevy_egui::{egui, EguiContext, EguiPlugin};

/// This example illustrates the various features of Bevy UI.
fn main() {
    App::build()
        .add_plugins(DefaultPlugins)
        .add_plugin(EguiPlugin)
        .add_resource(ARSTimer(Timer::from_seconds(1.0, true)))
        .add_resource(PersistenceTimer(Timer::from_seconds(5.0, true)))
        .add_resource(ListenerState::default())
        .add_startup_system(setup.system())
        .add_system(listener_prompt.system())
        .add_system(ars_ui.system())
        .add_system(ars.system())
        .add_system(persistence.system())
        .run();
}

fn macro_reduction() -> Reduction {
    Reduction(
        Pattern::parse("macro ?pattern ?rewrite".into()),
        Pattern::parse("<defined>"),
    )
}

fn fork_reduction() -> Reduction {
    Reduction(
        Pattern::parse("fork ?left ?right".into()),
        Pattern::parse("<left and right as individual entities>"),
    )
}

fn setup(commands: &mut Commands) {
    commands.spawn((ARS, macro_reduction()));
    commands.spawn((ARS, fork_reduction()));

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
    reductions: Query<&Reduction, With<ARS>>,
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

        for reduction in reductions.iter() {
            if reduction == &macro_reduction() || reduction == &fork_reduction() {
                continue;
            }

            tmp.write_all(format!("macro {} {}\n", reduction.0, reduction.1).as_bytes())
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

#[derive(Clone, PartialEq, Eq)]
struct Reduction(Pattern, Pattern);

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
            (Self::Hole(a_hole), Self::Hole(b_hole)) => Ok(vec![
                (a_hole.clone(), Self::Hole(b_hole.clone())),
                (b_hole.clone(), Self::Hole(a_hole.clone())),
            ]),
            (Self::Hole(hole), pat) | (pat, Self::Hole(hole)) => {
                Ok(vec![(hole.clone(), pat.clone())])
            }
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
    mut listener_state: ResMut<ListenerState>,
    mut egui_context: ResMut<EguiContext>,
) {
    let ctx = &mut egui_context.ctx;
    egui::Window::new("Listener").show(ctx, |ui| {
        let listener_resp = ui.text_edit_singleline(&mut listener_state.command);
        if (ui.button("eval").clicked || listener_resp.lost_kb_focus)
            && !listener_state.command.is_empty()
        {
            commands.spawn((ARS, Pattern::parse(&listener_state.command)));
            listener_state.command = Default::default();
        }
    });
}

fn ars(
    time: Res<Time>,
    mut timer: ResMut<ARSTimer>,
    commands: &mut Commands,
    reductions: Query<(Entity, &Reduction), With<ARS>>,
    free_patterns: Query<(Entity, &Pattern), With<ARS>>,
) {
    if !timer.0.tick(time.delta_seconds()).just_finished() {
        return;
    }

    for (pattern_entity, pattern) in free_patterns.iter() {
        for (reduction_entity, reduction) in reductions.iter() {
            if let Ok(bindings) = pattern.bind(&reduction.0) {
                println!("Bindings {:?}", bindings);
                commands.despawn(pattern_entity);

                if reduction == &macro_reduction() {
                    let bindings_map: BTreeMap<_, _> = bindings.iter().cloned().collect();
                    if bindings_map.len() != bindings.len() {
                        panic!("Unification is not supported yet");
                    }

                    if let (Some(pattern), Some(rewrite)) = (
                        bindings_map.get("pattern").cloned(),
                        bindings_map.get("rewrite").cloned(),
                    ) {
                        if pattern == rewrite {
                            println!("Dropping identity reduction");
                            continue;
                        }
                        commands.spawn((ARS, Reduction(pattern, rewrite)));
                    }
                } else if reduction == &fork_reduction() {
                    let bindings_map: BTreeMap<_, _> = bindings.iter().cloned().collect();
                    if bindings_map.len() != bindings.len() {
                        panic!("Unification is not supported yet");
                    }

                    if let (Some(left), Some(right)) = (
                        bindings_map.get("left").cloned(),
                        bindings_map.get("right").cloned(),
                    ) {
                        commands.spawn((ARS, left));
                        commands.spawn((ARS, right));
                    }
                } else {
                    commands.spawn((ARS, reduction.1.apply(bindings)));
                    commands.despawn(reduction_entity);
                }
            }
        }
    }
}

fn ars_ui(
    mut egui_context: ResMut<EguiContext>,
    patterns: Query<&Pattern, With<ARS>>,
    reductions: Query<&Reduction, With<ARS>>,
) {
    let ctx = &mut egui_context.ctx;
    for (i, pattern) in patterns.iter().enumerate() {
        egui::Window::new(format!("{}. {}", i, pattern))
            .title_bar(false)
            .show(ctx, |ui| {
                ui.label(format!("{}", pattern));
            });
    }

    for (i, reduction) in reductions.iter().enumerate() {
        egui::Window::new(format!("{}. {} => {}", i, reduction.0, reduction.1))
            .title_bar(false)
            .resizable(false)
            .show(ctx, |ui| {
                ui.vertical(|ui| {
                    ui.monospace(format!("{}", reduction.0));
                    ui.separator();
                    ui.monospace(format!("{}", reduction.1));
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
