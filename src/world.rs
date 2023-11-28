use std::collections::HashMap;

use crate::{
    actions::{Action, Id, Locator, Position},
    render::{GeoWorldRenderer, MacroQuadRenderer},
    GeoMode,
};

pub struct GeoWorld {
    idcount: Id,
    pub points: HashMap<Id, Position>,
    pub point_modes: HashMap<Id, GeoMode>,
    pub lines: HashMap<(Id, Id), GeoMode>,
}

impl GeoWorld {
    pub fn new() -> Self {
        Self {
            idcount: 0,
            points: HashMap::new(),
            point_modes: HashMap::new(),
            lines: HashMap::new(),
        }
    }

    pub fn locate(&self, id: Id) -> Locator {
        (id, self.points.get(&id).copied())
    }

    pub fn idgen(&mut self) -> Id {
        let old_idcount = self.idcount;
        self.idcount = old_idcount + 1;
        old_idcount
    }

    pub fn new_point(&mut self, id: Id, pos: Position, mode: GeoMode) {
        self.points.insert(id, pos);
        self.point_modes.insert(id, mode);
    }

    pub fn move_point(&mut self, id: Id, pos: Position) {
        if self.points.contains_key(&id) {
            self.points.insert(id, pos);
        }
    }

    pub fn delete_point(&mut self, id: Id) {
        self.points.remove(&id);
    }

    pub fn new_line(&mut self, start: Id, end: Id, mode: GeoMode) {
        self.lines.insert((start, end), mode);
    }

    pub fn get_point_at(&self, pos: Position) -> Option<Id> {
        for (&id, &point) in self.points.iter() {
            if MacroQuadRenderer::contacts_point(pos, point) {
                return Some(id);
            }
        }
        None
    }

    pub fn eval(&mut self, action: Action) {
        match action {
            Action::AddPoint(id, pos) => self.new_point(id, pos, GeoMode::Ink),
            Action::AddLine(src, dst) => self.new_line(src, dst, GeoMode::Ink),
            Action::DeletePoint((id, _)) => self.delete_point(id),
            Action::Seq(actions) => {
                for action in actions {
                    self.eval(action);
                }
            }
            Action::MovePoint((id, _), pos) => self.move_point(id, pos),
            Action::AddMetaPoint(id, pos) => self.new_point(id, pos, GeoMode::MetaInk),
            Action::AddMetaLine(src, dst) => {
                self.new_line(src, dst, GeoMode::MetaInk);
            }
            Action::AddMetaCombinator(..) | Action::NoAction => {}
        }
    }
}
