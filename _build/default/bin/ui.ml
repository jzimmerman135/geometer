open Base
open Raylib
module Vec2 = Raylib.Vector2

let get_mouse_pos () = (get_mouse_x (), get_mouse_y ())

let empty =
  {
    mouse_pos = get_mouse_pos ();
    selected = [];
    mode = InkMode;
    mousedrag_startpos = None;
    is_metameta_mode = false;
  }

let refresh ({ mode; selected; mousedrag_startpos; _ } as ui) world :
    ui * uiaction =
  (* utils *)
  let getids pts = List.map fst pts in
  let movept (startx, starty) (stopx, stopy) (id, (px, py)) =
    (id, (px + stopx - startx, py + stopy - starty))
  in
  let genmoves (pts : point list) (start : position) (stop : position) =
    let genmove ((id, pos) as pt) =
      let _, newpos = movept start stop pt in
      MovePoint (id, pos, newpos)
    in
    List.map genmove pts
  in
  let contacts_point pred pos =
    let point_size = Int.of_float point_size in
    let makes_contact (x1, y1) (x2, y2) =
      abs (x1 - x2) < point_size && abs (y1 - y2) < point_size
    in
    let at (_, pos') = makes_contact pos pos' in
    IdMap.to_seq world.points |> Seq.find (fun pt -> pred pt && at pt)
  in
  let contacts_any_point = contacts_point (fun _ -> true) in

  (* polling user *)
  let mouse_pos = get_mouse_pos () in
  let shift_down = is_key_down Key.Left_shift || is_key_down Key.Right_shift in
  let mouse_down = is_mouse_button_down MouseButton.Left in
  let mouse_released = is_mouse_button_released MouseButton.Left in
  let point_under_mouse = contacts_any_point mouse_pos in
  if is_key_down Key.U then
    print_endline
      ("point_under_mouse: "
      ^
      match point_under_mouse with
      | Some (id, pos) -> Int.to_string id ^ " " ^ pos_to_string pos
      | None -> "none");

  (* pre-update UI *)
  let mode =
    match mode with
    | mode when not (is_mouse_button_pressed MouseButton.Right) -> mode
    | InkMode -> MetaInkMode
    | MetaInkMode -> InkMode
  in
  let ui = { ui with mouse_pos; mode } in

  (* let contacts_inkpoint world = *)
  (*   contacts_point (fun (id, _) -> IdMap.find_opt id world.colors = Some Ink) *)
  (* in *)
  (* let contacts_metapoint world = *)
  (*   contacts_point (fun (id, _) -> *)
  (*       IdMap.find_opt id world.colors = Some MetaInk) *)
  (* in *)
  let selected, action =
    match (mode, selected, mousedrag_startpos, point_under_mouse) with
    | InkMode, [], _, None when shift_down && mouse_down ->
        let id = World.idgen () in
        ( [ (id, mouse_pos) ],
          Seq [ AddPoint (id, mouse_pos); HighlightAll [ id ] ] )
    (* move case *)
    | _, (_ :: _ as pts), None, None when mouse_down ->
        (pts, Seq [ HighlightAll (getids pts) ])
    (* endmove case *)
    | _, (_ :: _ as pts), Some dragstart, _ when mouse_released ->
        ( List.map (movept dragstart mouse_pos) pts,
          Seq (HighlightAll (getids pts) :: genmoves pts dragstart mouse_pos) )
    (* Just keep selected *)
    | _, (_ :: _ as pts), _, _ -> (pts, HighlightAll (getids pts))
    (* base case *)
    | _, [], _, _ -> ([], NoAction)
  in

  (* post-update ui *)
  let mousedrag_startpos =
    match mousedrag_startpos with
    | None when mouse_down -> Some mouse_pos
    | Some _ when mouse_released -> None
    | drag -> drag
  in
  ({ ui with selected; mousedrag_startpos }, action)
