open Base
module Vec2 = Raylib.Vector2
open Raylib

let empty =
  {
    mousepos = (get_mouse_x (), get_mouse_y ());
    selected = NoSelection;
    mode = InkMode;
    animation = None;
    input = Nothing;
    accessors =
      (let ports = [ "size"; "x"; "y"; "r"; "g"; "b" ] in
       List.map (fun x -> (x, 0)) ports
       |> Render.combinator_menu_dims |> List.combine ports);
    combinators =
      (let combs =
         [
           ("add", 2);
           ("sin", 1);
           ("time", 0);
           ("mul", 2);
           ("gate", 1);
           ("val", 1);
         ]
       in
       Render.combinator_menu_dims combs |> List.combine combs);
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
  x - (leeway * 2) < x'
  && x' < x + w + (leeway * 10)
  && y - leeway < y'
  && y' < y + h + leeway

let contacts_rect = contacts_rect_rough 4

(* Update the UI state's input, animation, mode and mousepos field *)
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
    | None when ui.mode <> mode -> Some 0.
    | Some n when n >= 1. -> None
    | Some n -> Some (n +. 0.01)
    | a -> a
  in
  let input =
    let contacts_any_point = contacts_any_point_in world.points in
    let mousedown = is_mouse_button_down MouseButton.Left in
    let mousereleased = is_mouse_button_released MouseButton.Left in
    let shiftdown = is_key_down Key.Left_shift || is_key_down Key.Right_shift in
    match (ui.input, contacts_any_point mousepos) with
    (* Recognize and prioritize special keypresses *)
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

(* Update the UI's selection field and provide an action to modify the world.
 * Must be done immediately after the UI is polled.
 *)
let action world ui : ui * uiaction =
  let add_to_selection (id, pos) selected =
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
  let rec get_action = function
    (* Deleting points *)
    | Delete, Selected pts ->
        let deletept pt = DeletePoint pt in
        (NoSelection, Seq (List.map deletept pts))
    (* Maintiain active selection drag *)
    | DraggingFrom (Plain, EmptySpace pos), ActivelySelecting _ ->
        ( ActivelySelecting (World.points_in_rect world pos ui.mousepos),
          NoAction )
    (* Maintain combinator menu drag *)
    | DraggingFrom (Shift, EmptySpace pos), CombinatorMenuSelection _ ->
        let dragoff = diffpos pos ui.mousepos in
        let rec findi i = function
          | (_, rect) :: _ when contacts_rect dragoff rect -> i
          | _ :: rs -> findi (i + 1) rs
          | [] -> -1
        in
        (CombinatorMenuSelection (findi 0 ui.combinators), NoAction)
    (* Release from active selection drag *)
    | DragReleasedFrom _, ActivelySelecting (_ :: _ as xs) ->
        (Selected xs, NoAction)
    | DragReleasedFrom _, ActivelySelecting [] -> (NoSelection, NoAction)
    (* Released while moving selection *)
    | DragReleasedFrom ((Plain, Point (_, startpos)), endpos), Selected pts ->
        ( Selected (List.map (shiftpt startpos endpos) pts),
          Seq (genmoves pts startpos endpos) )
    (* Released while picking a combinator from menu, but no menu item picked *)
    | DragReleasedFrom ((Shift, EmptySpace _), _), CombinatorMenuSelection -1 ->
        (NoSelection, NoAction)
    (* Released while picking a combinator from menu *)
    | ( DragReleasedFrom ((Shift, EmptySpace startpos), _),
        CombinatorMenuSelection i ) ->
        let newid = World.idgen () in
        let (name, nports), (x, y, _, _) = List.nth ui.combinators i in
        let rec genportids n =
          if n > 0 then World.idgen () :: genportids (n - 1) else []
        in
        ( NoSelection,
          AddCombinator
            (newid, (name, diffpos (-x, -y - 12) startpos, genportids nports))
        )
    (* Released while drawing a line *)
    | DragReleasedFrom ((Shift, Point ((startid, _) as pt)), endpos), sel ->
        let selpts = get_selected sel in
        let findstuff id = IdMap.find id world.stuffmap in
        let rec mkline point_at_mouse mode =
          let startptstuff = findstuff startid in
          match (mode, point_at_mouse) with
          (* Do the submode action for metameta *)
          | MetaMetaMode mode', _ ->
              mkline point_at_mouse mode'
              (* Drawing metaline from inkpoint creates a port at the end *)
          | MetaInkMode, None when startptstuff = Ink ->
              (PortMenu (pt, endpos, -1), NoAction)
          (* Drawing metaline, ensure metaline end was not released on top of another point *)
          | MetaInkMode, None when startptstuff = MetaInk ->
              let ptid, lineid = (World.idgen (), World.idgen ()) in
              let newpt = (ptid, ui.mousepos) in
              ( Selected [ newpt ],
                Seq
                  [
                    AddPoint (MetaInk, newpt);
                    AddLine (MetaInk, lineid, (startid, ptid));
                  ] )
          (* Ensure line end was released on top of another point *)
          | InkMode, Some (endid, pos) when startid <> endid -> (
              let id = World.idgen () in
              match (findstuff startid, findstuff endid) with
              | Ink, Ink ->
                  ( Selected (add_to_selection (endid, pos) selpts),
                    AddLine (Ink, id, (startid, endid)) )
              | _ -> (NoSelection, NoAction))
          (* Shift click on a point is multiselect *)
          | _, _ when selpts <> [] ->
              (Selected (add_to_selection pt selpts), NoAction)
          | _, _ -> (NoSelection, NoAction)
        in
        mkline (contacts_any_point_in world.points endpos) ui.mode
    (* Start active selection drag *)
    | Clicked (Plain, EmptySpace _), NoSelection
    | Clicked (Plain, EmptySpace _), Selected _ ->
        (ActivelySelecting [], NoAction)
    (* Selecting a point *)
    | Clicked (Plain, Point pt), NoSelection -> (Selected [ pt ], NoAction)
    | Clicked (Plain, Point pt), Selected pts when not (List.mem pt pts) ->
        (Selected [ pt ], NoAction)
    (* Adding a point *)
    | Clicked (Shift, EmptySpace pos), (NoSelection as sel)
    | Clicked (Shift, EmptySpace pos), (Selected _ as sel) -> (
        let pts = match sel with Selected pts -> pts | _ -> [] in
        let id = World.idgen () in
        match ui.mode with
        (* Add a point when in Ink mode *)
        | MetaMetaMode InkMode | InkMode ->
            (Selected ((id, pos) :: pts), AddPoint (Ink, (id, pos)))
        (* Offer a combinator when in Meta-ink mode *)
        | MetaMetaMode MetaInkMode | MetaInkMode ->
            (CombinatorMenuSelection (-1), NoAction)
        | _ -> (NoSelection, NoAction))
    (* Moving a point *)
    | DraggingFrom (Plain, Point (id, startpos)), Selected pts
      when List.mem_assoc id pts ->
        (Selected pts, Seq (genmoves pts startpos ui.mousepos))
    (* Shift click a point to add to selection, also starts drawing a line *)
    | Clicked (Shift, Point (id, pos)), Selected pts
      when not (List.mem_assoc id pts) ->
        (Selected ((id, pos) :: pts), NoAction)
    | Clicked (Shift, Point pt), NoSelection -> (Selected [ pt ], NoAction)
    (* Port menu is active, but user wants to do something else. Let them cook *)
    | Clicked c, PortMenu (_, _, -1) -> get_action (Clicked c, NoSelection)
    (* Made a selection on port menu, create a port *)
    | Clicked _, PortMenu ((fromid, _), menupos, _i) ->
        let ptid, lineid = (World.idgen (), World.idgen ()) in
        let newpt = (ptid, menupos) in
        ( Selected [ newpt ],
          Seq
            [
              AddPoint (MetaInk, newpt);
              AddLine (MetaInk, lineid, (fromid, ptid));
            ] )
    (* Port menu is up *)
    | Nothing, PortMenu (frompt_pos, menupos, _) ->
        let dragoff = diffpos menupos ui.mousepos in
        let rec findi i = function
          | (_, rect) :: _ when contacts_rect dragoff rect -> i
          | _ :: rs -> findi (i + 1) rs
          | [] -> -1
        in
        let i' = findi 0 ui.accessors in
        (PortMenu (frompt_pos, menupos, i'), NoAction)
    (* No interaction *)
    | _, Selected pts -> (Selected pts, NoAction)
    | _ -> (NoSelection, NoAction)
  in
  let selected, action = get_action (ui.input, ui.selected) in
  ({ ui with selected }, action)
