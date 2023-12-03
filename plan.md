# Outline

## TODOS

- Get ports and equality working (parallel and perpendicular lines)

## UI Controls

## Actions

- Right-click to switch mode

#### Select-state

- Click on anything to select it
  - Drag to move it
- Drag over a region to select all things in that regions
  - Drag to move them

#### InkMode Insert state

- Shift-click on a blank location adds an ink point, and selects it
- Shift-click on an ink point begins a line

  - If mouse is released on another ink point, it stays
  - If mouse is released elsewhere, it disappears

#### MetaMode Insert state

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

# Implementation

```
Input UI:
In InkMode,
Selected is Nothing,
DragStart is None,
LeftMouse is down,
LeftMouse is over nothing.

Actions:
--> Id is Idgen ()
--> AddPoint (Id, MousePos)
--> HighlightPoint [Id]

Output UI:
--> DragStart is MousePos
--> Selected is [Id]
```

```
Input UI:
In InkMode,
Selected is Some,
DragStart is None,
LeftMouse is down,
LeftMouse is over nothing.

Actions:
--> Id is Idgen ()
--> AddPoint (Id, MousePos)
--> HighlightPoint [Id :: Some]

Output UI:
--> DragStart is MousePos
--> Selected is [Id :: Some]
```
