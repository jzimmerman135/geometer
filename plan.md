# Outline

## UI Controls

#### Actions

- Right-click to switch mode

- Click on anything to select it
  - Drag to move it
- Drag over a region to select all things in that regions

  - Drag to move them

- Shift-click on a blank location adds an ink point
- Shift-click on an ink point begins a line

  - If mouse is released on another ink point, it stays
  - If mouse is released elsewhere, it disappears

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
