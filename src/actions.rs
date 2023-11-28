use crate::relations::Combinator;

pub type Position = (f32, f32);
pub type Id = i64;
pub type Locator = (Id, Option<Position>);

pub enum Action {
    AddPoint(Id, Position),
    DeletePoint(Locator),
    MovePoint(Locator, Position),
    AddLine(Id, Id),
    AddMetaPoint(Id, Position),
    AddMetaCombinator(Id, Combinator, Position),
    AddMetaLine(Id, Id),
    Seq(Vec<Action>),
    NoAction,
}

#[allow(unused)]
impl Action {
    pub fn invert(&self) -> Action {
        match self {
            Action::AddPoint(id, pos) => Action::DeletePoint((*id, Some(*pos))),
            Action::DeletePoint((id, Some(pos))) => Action::AddPoint(*id, *pos),
            Action::Seq(actions) => Action::Seq(actions.iter().map(Self::invert).collect()),
            _ => Action::NoAction,
        }
    }
}

pub fn apply_drag(pos: Position, drag_start: Position, drag_end: Position) -> Position {
    let drag_shift = (drag_end.0 - drag_start.0, drag_end.1 - drag_start.1);
    (pos.0 + drag_shift.0, pos.1 + drag_shift.1)
}
