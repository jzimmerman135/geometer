# Outline

## UI Controls

- Right-click to switch mode

#### Select-mode (pseudo-mode, active when no selection)

- Click on anything to select
  - Drag to move it
- Drag over a region to select it

#### Visual-mode (pseudo-mode, active when selection)

- Click to begin move
  - Stays when mouse released, reverts to select-mode

#### Ink-mode

- Shift-click on a blank location adds an ink point
- Shift-click on an ink point begins a line
  - If mouse is released on another ink point, it stays
  - If mouse is released elsewhere, it disappears

#### Meta-mode

- Shift-click on a blank location adds a combinator
- Shift-click on a point begins a meta-line

  - If mouse is released on another meta point, it stays
  - If mouse is released on another ink point, it stays
  - If mouse is released elsewhere, it stays
    and creates a meta ink point

## Geoworld

A data structure for geometry

Contains:

- Points { Id -> Position, Color }
- Lines { Id -> (Id, Id), Color }
