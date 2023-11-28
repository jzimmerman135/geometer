use std::collections::HashSet;

use macroquad::{
    color::Color,
    shapes::{draw_circle, draw_circle_lines, draw_line, draw_rectangle, draw_rectangle_lines},
    text::draw_text,
};

use crate::{
    actions::{Id, Position},
    world::GeoWorld,
    GeoMode, Selection,
};

pub trait GeoWorldRenderer {
    fn draw(&self, world: &GeoWorld, selected: &Selection);
    fn contacts_point(pos: Position, loc: Position) -> bool;
    fn new() -> Self;
    fn toggle_mode(&mut self);
}

pub struct MacroQuadRenderer {
    pub geomode: GeoMode,
}

pub const BACKGROUND: Color = Color {
    r: 0.929,
    g: 0.941,
    b: 0.985,
    a: 1.,
};

pub const HIGHLIGHT: Color = Color {
    r: 0.362,
    g: 0.739,
    b: 0.928,
    a: 0.3,
};

pub const HIGHLIGHT_BRIGHT: Color = Color {
    r: 0.362,
    g: 0.739,
    b: 0.928,
    a: 0.6,
};

pub const BLUE: Color = Color {
    r: 0.262,
    g: 0.439,
    b: 0.928,
    a: 1.,
};

pub const YELLOW: Color = Color {
    r: 226. / 255.,
    g: 169. / 255.,
    b: 89. / 255.,
    a: 1.,
};

impl GeoMode {
    fn color(&self) -> Color {
        match self {
            GeoMode::Ink => BLUE,
            GeoMode::MetaInk => YELLOW,
        }
    }
}

pub const POINT_RADIUS: f32 = 10.;
impl GeoWorldRenderer for MacroQuadRenderer {
    fn toggle_mode(&mut self) {
        self.geomode = match self.geomode {
            GeoMode::Ink => GeoMode::MetaInk,
            GeoMode::MetaInk => GeoMode::Ink,
        }
    }

    fn draw(&self, world: &GeoWorld, selected: &Selection) {
        let dummy_selected_ids: Option<HashSet<Id>>;

        let selected_ids = match selected {
            Selection::Set(ps) => Some(ps),
            Selection::Grouping((x1, y1), (x2, y2), ps) => {
                let w = absmin(x2 - x1, 1.);
                let h = absmin(y2 - y1, 1.);
                draw_rectangle_lines(*x1, *y1, w, h, 2.0, HIGHLIGHT_BRIGHT);
                draw_rectangle(*x1, *y1, w, h, HIGHLIGHT);
                Some(ps)
            }
            Selection::Drag(_, vs) => {
                dummy_selected_ids = Some(vs.iter().map(|(id, _)| *id).collect::<HashSet<i64>>());
                dummy_selected_ids.as_ref()
            }
            Selection::Line(id, start, end) => {
                dummy_selected_ids = Some(HashSet::from([*id]));
                draw_line(start.0, start.1, end.0, end.1, 2., self.geomode.color());
                dummy_selected_ids.as_ref()
            }
            Selection::NoSelect => None,
        };

        draw_mode_box(GeoMode::Ink, GeoMode::Ink == self.geomode);
        draw_mode_box(GeoMode::MetaInk, GeoMode::MetaInk == self.geomode);

        for ((id1, id2), mode) in world.lines.iter() {
            if let (Some((x1, y1)), Some((x2, y2))) = (world.points.get(id1), world.points.get(id2))
            {
                draw_line(*x1, *y1, *x2, *y2, 2., mode.color());
            }
        }

        for (id, point) in world.points.iter() {
            if selected_ids.map_or(false, |selected| selected.contains(id)) {
                draw_circle(point.0, point.1, POINT_RADIUS * 1.5, HIGHLIGHT);
                draw_circle_lines(point.0, point.1, POINT_RADIUS * 1.5, 2.0, HIGHLIGHT_BRIGHT);
            }
            draw_circle(
                point.0,
                point.1,
                POINT_RADIUS,
                world.point_modes.get(id).unwrap().color(),
            )
        }
    }

    fn contacts_point(pos: Position, loc: Position) -> bool {
        (pos.0 - loc.0).abs() < POINT_RADIUS * 1.5 && (pos.1 - loc.1).abs() < POINT_RADIUS * 1.5
    }

    fn new() -> Self {
        MacroQuadRenderer {
            geomode: GeoMode::Ink,
        }
    }
}

fn draw_mode_box(mode: GeoMode, selected: bool) {
    let ch = 30.;
    let (x, y, w, h, text, color) = match mode {
        GeoMode::Ink => (
            ch * 0.4,
            ch * 0.4,
            ch * 2.,
            ch * 1.1,
            "ink",
            Color { a: 1.0, ..BLUE },
        ),
        GeoMode::MetaInk => (
            ch * 0.4 + ch * 2.5,
            ch * 0.4,
            ch * 4.,
            ch * 1.1,
            "meta-ink",
            Color { a: 1.0, ..YELLOW },
        ),
    };

    draw_rectangle(x, y, w, h, color);
    draw_text(text, x + ch * 0.2, y + ch * 0.8, ch, BACKGROUND);
    if selected {
        draw_rectangle_lines(x - 5., y - 5., w + 10., h + 10., 4.0, color);
    }
}

/// `coord` is [-1., 1.]
/// `dim` is non-negative, usually either `screen_width()` or `screen_height()`
pub fn _ndc_to_screen(coord: f32, dim: f32) -> f32 {
    dim * ((coord + 1.0) / 2.0)
}

pub fn inside_bounds(p: &Position, x: Position, y: Position) -> bool {
    let bl_x = x.0.min(y.0) - POINT_RADIUS;
    let bl_y = x.1.min(y.1) - POINT_RADIUS;
    let tl_x = x.0.max(y.0) + POINT_RADIUS;
    let tl_y = x.1.max(y.1) + POINT_RADIUS;
    bl_x < p.0 && p.0 < tl_x && bl_y < p.1 && p.1 < tl_y
}

pub fn absmin(x: f32, f: f32) -> f32 {
    if x.abs() < f {
        f
    } else {
        x
    }
}
