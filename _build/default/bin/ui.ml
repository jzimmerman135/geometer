open Base
open Raylib
module Vec2 = Raylib.Vector2

let empty =
  {
    mousepos = (get_mouse_x (), get_mouse_y ());
    selected = NoSelection;
    mode = InkMode;
    input = Nothing;
  }

let contacts_point pred points pos =
  let contact_radius = Int.of_float point_highlight_size in
  let makes_contact (x1, y1) (x2, y2) =
    abs (x1 - x2) < contact_radius && abs (y1 - y2) < contact_radius
  in
  let at (_, pos') = makes_contact pos pos' in
  IdMap.to_seq points |> Seq.find (fun pt -> pred pt && at pt)

let contacts_any_point_in = contacts_point (fun _ -> true)

let poll ui world =
  let mousepos = (get_mouse_x (), get_mouse_y ()) in
  let mode =
    let m_key_pressed = is_key_pressed Key.M in
    match ui.mode with
    (* M key toggles meta-meta mode *)
    | MetaMetaMode m when m_key_pressed -> m
    | m when m_key_pressed -> MetaMetaMode m
    (* No M key and no right click, do nothing *)
    | m when not (is_mouse_button_pressed MouseButton.Right) -> m
    (* Right mouse clicked, this toggles ink and meta-ink *)
    | InkMode -> MetaInkMode
    | MetaInkMode -> InkMode
    | MetaMetaMode InkMode -> MetaMetaMode MetaInkMode
    | MetaMetaMode MetaInkMode -> MetaMetaMode InkMode
    | MetaMetaMode (MetaMetaMode _) -> raise (Failure "meta-meta (meta-meta)")
  in
  let input =
    let contacts_any_point = contacts_any_point_in world.points in
    let mousedown = is_mouse_button_down MouseButton.Left in
    let mousereleased = is_mouse_button_released MouseButton.Left in
    let shiftdown = is_key_down Key.Left_shift || is_key_down Key.Right_shift in
    match (ui.input, contacts_any_point mousepos) with
    (* Keypresses *)
    | _, _ when is_key_pressed Key.Backspace -> Delete
    | _, _ when is_key_pressed Key.Left -> Undo
    | _, _ when is_key_pressed Key.Right -> Redo
    (* Stop drag when mouse released *)
    | DraggingFrom origin, _ when mousereleased ->
        DragReleasedFrom (origin, mousepos)
    (* If mouse is not down *)
    | _, _ when not mousedown -> Nothing
    (* Left mouse is down, but wasn't previously *)
    | Nothing, Some pt when shiftdown -> Clicked (Shift, Point pt)
    | Nothing, None when shiftdown -> Clicked (Shift, EmptySpace mousepos)
    | Nothing, Some pt -> Clicked (Plain, Point pt)
    | Nothing, None -> Clicked (Plain, EmptySpace mousepos)
    (* Left mouse is down and was previously dragging *)
    | DraggingFrom place, _ | Clicked place, _ -> DraggingFrom place
    (* Otherwise back to nothing *)
    | RightClicked, _
    | Delete, _
    | Undo, _
    | Redo, _
    | DragReleasedFrom (_, _), _ ->
        Nothing
  in
  { ui with mousepos; mode; input }

let action world ui : ui * uiaction =
  let movept (diffx, diffy) (id, (px, py)) = (id, (px + diffx, py + diffy)) in
  let diffpt (fromx, fromy) (tox, toy) = (tox - fromx, toy - fromy) in
  let shiftpt start stop : point -> point = diffpt start stop |> movept in
  let genmoves (pts : point list) (start : position) (stop : position) =
    let genmove ((id, _) as pt) =
      MovePoint (id, shiftpt start stop pt |> snd)
    in
    List.map genmove pts
  in
  let selected, action =
    match (ui.input, ui.selected) with
    (* deleting points *)
    | Delete, Selected pts ->
        let deletept pt = DeletePoint pt in
        (NoSelection, Seq (List.map deletept pts))
    (* start active selection drag *)
    | Clicked (Plain, EmptySpace _), NoSelection
    | Clicked (Plain, EmptySpace _), Selected _ ->
        (ActivelySelecting [], NoAction)
    (* maintiain active selection drag *)
    | DraggingFrom (Plain, EmptySpace pos), ActivelySelecting _ ->
        ( ActivelySelecting (World.points_in_rect world pos ui.mousepos),
          NoAction )
    (* release from active selection drag *)
    | DragReleasedFrom _, ActivelySelecting (_ :: _ as xs) ->
        (Selected xs, NoAction)
    | DragReleasedFrom _, ActivelySelecting [] -> (NoSelection, NoAction)
    (* released while moving selection *)
    | DragReleasedFrom ((Plain, Point (_, startpos)), endpos), Selected pts ->
        ( Selected (List.map (shiftpt startpos endpos) pts),
          Seq (genmoves pts startpos endpos) )
    (* released while drawing a line *)
    | DragReleasedFrom ((Shift, Point (startid, _)), endpos), _ -> (
        match contacts_any_point_in world.points endpos with
        | Some ((endid, _) as endpt) when startid <> endid ->
            let id = World.idgen () in
            (Selected [ endpt ], AddLine (Ink, id, (startid, endid)))
        | _ -> (NoSelection, NoAction))
    (* selecting a point *)
    | Clicked (Plain, Point pt), NoSelection -> (Selected [ pt ], NoAction)
    | Clicked (Plain, Point pt), Selected pts when not (List.mem pt pts) ->
        (Selected [ pt ], NoAction)
    (* adding a point *)
    | Clicked (Shift, EmptySpace pos), (NoSelection as sel)
    | Clicked (Shift, EmptySpace pos), (Selected _ as sel) -> (
        let pts = match sel with Selected pts -> pts | _ -> [] in
        let id = World.idgen () in
        match ui.mode with
        | MetaMetaMode InkMode | InkMode ->
            (Selected ((id, pos) :: pts), AddPoint (Ink, (id, pos)))
        | _ -> (NoSelection, NoAction))
    (* moving a point *)
    | DraggingFrom (Plain, Point (id, startpos)), Selected pts
      when List.mem_assoc id pts ->
        (Selected pts, Seq (genmoves pts startpos ui.mousepos))
    (* start drawing a line *)
    | Clicked (Shift, Point pt), Selected _ -> (Selected [ pt ], NoAction)
    | Clicked (Shift, Point pt), NoSelection -> (Selected [ pt ], NoAction)
    (* no interaction *)
    | _, Selected pts -> (Selected pts, NoAction)
    | _ -> (NoSelection, NoAction)
  in
  ({ ui with selected }, action)
