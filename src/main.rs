mod actions;
mod relations;
mod render;
mod world;
use std::collections::HashSet;

use actions::{Action, Id, Locator, Position};
use macroquad::prelude::*;
use render::{GeoWorldRenderer, MacroQuadRenderer, BACKGROUND};
use world::GeoWorld;

use crate::{actions::apply_drag, render::inside_bounds};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum GeoMode {
    Ink,
    MetaInk,
}

#[derive(Debug)]
pub enum Selection {
    Grouping(Position, Position, HashSet<Id>),
    Set(HashSet<Id>),
    Drag(Position, Vec<Locator>),
    Line(Id, Position, Position),
    NoSelect,
}

#[macroquad::main("Geometer")]
async fn main() {
    let mut geoworld = GeoWorld::new();
    let mut selection: Selection = Selection::NoSelect;
    let mut renderer = MacroQuadRenderer::new();

    // this is a bit hacky, but it adds a particular semantic to the AddPoint action which makes
    // the interface a little nicer.
    let mut add_point_momentum = false;

    while !is_key_pressed(KeyCode::Escape) {
        clear_background(BACKGROUND);

        // cache window information
        let mouse_down = is_mouse_button_down(MouseButton::Left);
        let mouse_released = is_mouse_button_released(MouseButton::Left);
        let mouse_pos = mouse_position();
        let shift_down = is_key_down(KeyCode::LeftShift) || is_key_down(KeyCode::RightShift);
        let delete_press = is_key_pressed(KeyCode::Backspace);
        let point_at_mouse = if mouse_down || mouse_released {
            geoworld.get_point_at(mouse_pos) // kind of expensive
        } else {
            None
        };

        if is_mouse_button_pressed(MouseButton::Right) {
            renderer.toggle_mode();
        }

        println!("selection: {:?}", selection);

        // user input -> action
        use Selection::*;
        let action = match selection {
            Grouping(start_pos, _, mut v) if mouse_down => {
                v.clear();
                for (id, pos) in geoworld.points.iter() {
                    if inside_bounds(pos, start_pos, mouse_pos) {
                        v.insert(*id);
                    }
                }
                selection = Grouping(start_pos, mouse_pos, v);
                Action::NoAction
            }
            Grouping(.., v) if mouse_released => {
                selection = if v.is_empty() { NoSelect } else { Set(v) };
                Action::NoAction
            }
            Set(_)
                if mouse_down && shift_down && point_at_mouse.is_some() && !add_point_momentum =>
            {
                let id = point_at_mouse.unwrap();
                selection = Line(id, *geoworld.points.get(&id).unwrap(), mouse_pos);
                Action::NoAction
            }
            Set(v) if mouse_down && point_at_mouse.is_some() => {
                if v.contains(&point_at_mouse.unwrap()) {
                    selection = Drag(
                        mouse_pos,
                        v.into_iter().map(|id| geoworld.locate(id)).collect(),
                    )
                } else {
                    selection = NoSelect
                }
                Action::NoAction
            }
            Set(mut v) if mouse_down && shift_down => {
                let id = geoworld.idgen();
                v.insert(id);
                selection = Set(v);
                Action::AddPoint(id, mouse_pos)
            }
            Set(mut v) if mouse_down => {
                v.clear();
                selection = Selection::Grouping(mouse_pos, mouse_pos, v);
                Action::NoAction
            }
            Set(v) if delete_press => {
                println!("deleted?");
                selection = NoSelect;
                Action::Seq(
                    v.iter()
                        .map(|&id| Action::DeletePoint(geoworld.locate(id)))
                        .collect(),
                )
            }
            Set(_) if mouse_released => {
                selection = NoSelect;
                Action::NoAction
            }
            Drag(drag_start, v) if mouse_down => {
                let moves = v
                    .iter()
                    .filter_map(|&loc| {
                        if let (_, Some(pos)) = loc {
                            let newpos = apply_drag(pos, drag_start, mouse_pos);
                            return Some(Action::MovePoint(loc, newpos));
                        }
                        None
                    })
                    .collect();
                selection = Drag(drag_start, v);
                Action::Seq(moves)
            }
            Drag(_, v) if mouse_released => {
                selection = Set(v.into_iter().map(|(id, _)| id).collect());
                Action::NoAction
            }
            Line(id, line_start, _) if mouse_down => {
                selection = Line(id, line_start, mouse_pos);
                Action::NoAction
            }
            Line(src, ..) if mouse_released && point_at_mouse.is_some_and(|dst| src != dst) => {
                selection = NoSelect;
                Action::AddLine(src, point_at_mouse.unwrap())
            }
            Line(..) if mouse_released => {
                selection = NoSelect;
                Action::NoAction
            }
            NoSelect if mouse_down && shift_down && point_at_mouse.is_some() => {
                // draw line
                let id = point_at_mouse.unwrap();
                selection = Line(id, *geoworld.points.get(&id).unwrap(), mouse_pos);
                Action::NoAction
            }
            NoSelect if mouse_down && shift_down => {
                let id = geoworld.idgen();
                selection = Set(HashSet::from([id]));
                Action::AddPoint(id, mouse_pos)
            }
            NoSelect if mouse_down && point_at_mouse.is_some() => {
                selection = Set(HashSet::from([point_at_mouse.unwrap()]));
                Action::NoAction
            }
            NoSelect if mouse_down => {
                selection = Grouping(mouse_pos, mouse_pos, HashSet::new());
                Action::NoAction
            }
            _ => Action::NoAction,
        };

        let action = match action {
            Action::AddPoint(..) if renderer.geomode == GeoMode::MetaInk => Action::NoAction,
            Action::AddLine(src, dst) if renderer.geomode == GeoMode::MetaInk => {
                Action::Seq(vec![Action::AddMetaLine(src, dst)])
            }
            action => action,
        };

        add_point_momentum = matches!(action, Action::AddPoint(_, _));

        // evaluation
        geoworld.eval(action);
        renderer.draw(&geoworld, &selection);
        next_frame().await
    }
}
