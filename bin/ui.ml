open Base
open Raylib
module Vec2 = Raylib.Vector2

let get_mouse_pos () = (get_mouse_x (), get_mouse_y ())

let empty =
  {
    mouse_pos = get_mouse_pos ();
    state = NoSelection;
    mode = InkMode;
    mousedrag_startpos = None;
    is_metameta_mode = false;
  }

let refresh ({ mode; state; mousedrag_startpos; _ } as ui) world : ui * uiaction
    =
  let mouse_pos = get_mouse_pos () in
  let ui = { ui with mouse_pos } in

  let contacts_point pred pos =
    let point_size = Int.of_float point_size in
    let makes_contact (x1, y1) (x2, y2) =
      abs (x1 - x2) < point_size && abs (y1 - y2) < point_size
    in
    let at (_, pos') = makes_contact pos pos' in
    IdMap.to_seq world.points |> Seq.find (fun pt -> pred pt && at pt)
  in

  let contacts_any_point = contacts_point (fun _ -> true) in
  (* let contacts_inkpoint world = *)
  (*   contacts_point (fun (id, _) -> IdMap.find_opt id world.colors = Some Ink) *)
  (* in *)
  (* let contacts_metapoint world = *)
  (*   contacts_point (fun (id, _) -> *)
  (*       IdMap.find_opt id world.colors = Some MetaInk) *)
  (* in *)
  (* let get_selected : state -> id list = function *)
  (*   | Selected xs -> xs *)
  (*   | _ -> [] *)
  (* in *)
  let shift_down = is_key_down Key.Left_shift || is_key_down Key.Right_shift in
  let mouse_down = is_mouse_button_down MouseButton.Left in
  let point_under_mouse = contacts_any_point mouse_pos in
  let mode' =
    match mode with
    | mode when not (is_mouse_button_pressed MouseButton.Right) -> mode
    | InkMode -> MetaInkMode
    | MetaInkMode -> InkMode
  in
  let ui = { ui with mode = mode' } in
  match (mode', state, mousedrag_startpos, point_under_mouse) with
  | _ when is_mouse_button_pressed MouseButton.Right -> (ui, NoAction)
  | InkMode, NoSelection, _, _ when shift_down && mouse_down ->
      let id = World.idgen () in
      let state' = Selected [ id ] in
      ( { ui with state = state' },
        Seq [ AddPoint (id, mouse_pos); HighlightAll [ id ] ] )
  (* move case *)
  (* | _, Selected ids, _, Some (id, _) when List.mem id ids -> *)
  (*     (ui, Seq [ HighlightAll ids ]) *)
  (* Just keep selected *)
  | _, Selected ids, _, _ -> (ui, HighlightAll ids)
  (* base case *)
  | _ -> (ui, NoAction)
