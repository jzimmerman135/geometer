open Base
module Vec2 = Raylib.Vector2
open Raylib

let empty =
  {
    mousepos = (get_mouse_x (), get_mouse_y ());
    selected = NoSelection;
    mode = InkMode;
    animation = 0.0;
    input = Nothing;
    combinators =
      (let combs =
         [ ("add", 2); ("sin", 1); ("time", 0); ("mul", 2); ("gate", 1) ]
       in
       List.combine combs (Render.combinator_menu_dims combs));
  }

let contacts_point pred points pos =
  let contact_radius = Int.of_float point_highlight_size in
  let makes_contact (x1, y1) (x2, y2) =
    abs (x1 - x2) < contact_radius && abs (y1 - y2) < contact_radius
  in
  let at (_, pos') = makes_contact pos pos' in
  IdMap.to_seq points |> Seq.find (fun pt -> pred pt && at pt)

let contacts_any_point_in = contacts_point (fun _ -> true)

let contacts_rect_rough leeway (x', y') (x, y, w, h) =
  x - (leeway * 10) < x'
  && x' < x + w + (leeway * 10)
  && y - leeway < y'
  && y' < y + h + leeway

let contacts_rect = contacts_rect_rough 4

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
  let animation =
    match ui.animation with
    | 0. when ui.mode = mode -> 0.
    | n when n > 1. -> 0.
    | n -> n +. 0.01
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
  { ui with mousepos; mode; input; animation }

let action world ui : ui * uiaction =
  let addsel (id, pos) selected =
    if List.mem_assoc id selected then selected else (id, pos) :: selected
  in
  let movept (diffx, diffy) (id, (px, py)) = (id, (px + diffx, py + diffy)) in
  let diffpos (fromx, fromy) (tox, toy) = (tox - fromx, toy - fromy) in
  let shiftpt start stop : point -> point = diffpos start stop |> movept in
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
    (* maintiain active selection drag *)
    | DraggingFrom (Plain, EmptySpace pos), ActivelySelecting _ ->
        ( ActivelySelecting (World.points_in_rect world pos ui.mousepos),
          NoAction )
    (* maintain combinator menu drag *)
    | DraggingFrom (Shift, EmptySpace pos), CombinatorMenuSelection _ ->
        let dragoff = diffpos pos ui.mousepos in
        let rec findi i = function
          | (_, rect) :: _ when contacts_rect dragoff rect -> i
          | _ :: rs -> findi (i + 1) rs
          | [] -> -1
        in
        (CombinatorMenuSelection (findi 0 ui.combinators), NoAction)
    (* release from active selection drag *)
    | DragReleasedFrom _, ActivelySelecting (_ :: _ as xs) ->
        (Selected xs, NoAction)
    | DragReleasedFrom _, ActivelySelecting [] -> (NoSelection, NoAction)
    (* released while moving selection *)
    | DragReleasedFrom ((Plain, Point (_, startpos)), endpos), Selected pts ->
        ( Selected (List.map (shiftpt startpos endpos) pts),
          Seq (genmoves pts startpos endpos) )
    (* released while picking a combinator from menu, but no menu item picked *)
    | DragReleasedFrom ((Shift, EmptySpace _), _), CombinatorMenuSelection -1 ->
        (NoSelection, NoAction)
    (* released while picking a combinator from menu *)
    | DragReleasedFrom ((Shift, EmptySpace pos), _), CombinatorMenuSelection _
      ->
        let newid = World.idgen () in
        (NoSelection, AddPoint (MetaInk, (newid, pos)))
    (* released while drawing a line *)
    | DragReleasedFrom ((Shift, Point ((startid, _) as pt)), endpos), sel ->
        let selpts = get_selected sel in
        let findstuff id = IdMap.find id world.colors in
        let rec mkline point_at_mouse mode =
          match (mode, point_at_mouse) with
          | MetaMetaMode mode', _ -> mkline point_at_mouse mode'
          (* ensure line end was released on top of another point *)
          | MetaInkMode, None ->
              let ptid, lineid = (World.idgen (), World.idgen ()) in
              let newpt = (ptid, ui.mousepos) in
              ( Selected [ newpt ],
                Seq
                  [
                    AddPoint (MetaInk, newpt);
                    AddLine (MetaInk, lineid, (startid, ptid));
                  ] )
          | InkMode, Some (endid, pos) when startid <> endid -> (
              let id = World.idgen () in
              match (findstuff startid, findstuff endid) with
              | Ink, Ink ->
                  ( Selected (addsel (endid, pos) selpts),
                    AddLine (Ink, id, (startid, endid)) )
              | _ -> (NoSelection, NoAction))
          (* shift is multiselect *)
          | _, _ when selpts <> [] -> (Selected (addsel pt selpts), NoAction)
          | _, _ -> (NoSelection, NoAction)
        in
        mkline (contacts_any_point_in world.points endpos) ui.mode
    (* start active selection drag *)
    | Clicked (Plain, EmptySpace _), NoSelection
    | Clicked (Plain, EmptySpace _), Selected _ ->
        (ActivelySelecting [], NoAction)
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
        (* ink mode *)
        | MetaMetaMode InkMode | InkMode ->
            (Selected ((id, pos) :: pts), AddPoint (Ink, (id, pos)))
        (* meta-ink mode *)
        | MetaMetaMode MetaInkMode | MetaInkMode ->
            (CombinatorMenuSelection (-1), NoAction)
        | _ -> (NoSelection, NoAction))
    (* moving a point *)
    | DraggingFrom (Plain, Point (id, startpos)), Selected pts
      when List.mem_assoc id pts ->
        (Selected pts, Seq (genmoves pts startpos ui.mousepos))
    (* shift click a point to add to selection, also starts drawing a line *)
    | Clicked (Shift, Point (id, pos)), Selected pts
      when not (List.mem_assoc id pts) ->
        (Selected ((id, pos) :: pts), NoAction)
    | Clicked (Shift, Point pt), NoSelection -> (Selected [ pt ], NoAction)
    (* no interaction *)
    | _, Selected pts -> (Selected pts, NoAction)
    | _ -> (NoSelection, NoAction)
  in
  ({ ui with selected }, action)
