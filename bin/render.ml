open Raylib
open Base

let mode_is_ink = function InkMode -> true | _ -> false
let mode_is_metaink = function MetaInkMode -> true | _ -> false
let stuffcolor : stuff -> color = function Ink -> blue | MetaInk -> red
let draw_inkpoint (x, y) = draw_circle x y point_size blue
let draw_metainkpoint (x, y) = draw_circle x y point_size red

let draw_highlightpoint (x, y) =
  draw_circle_lines x y (point_size +. 6.0) highlight_em;
  draw_circle x y (point_size +. 6.0) (highlight 0x44)

let draw_ui { mouse_pos; mode; is_metameta_mode; _ } =
  let draw_uibox highlighted color text w x =
    draw_rectangle x 10 w 30 color;
    draw_text text (x + 10) 12 30 background;
    let offset = 5 in
    if highlighted mode then
      draw_rectangle (x + offset) 42 (w - (offset * 2)) 3 color
  in
  draw_uibox mode_is_ink blue "ink" 80 10;
  draw_uibox mode_is_metaink red "meta-ink" 150 90;
  if is_metameta_mode then
    draw_uibox (fun _ -> true) green "meta-meta" 180 (get_screen_width () - 190);
  ignore mouse_pos
(* match mode with *)
(* | InkMode -> draw_inkpoint mouse_pos *)
(* | MetaInkMode -> draw_metainkpoint mouse_pos *)

let rec draw_action world : uiaction -> unit = function
  | Seq actions -> List.iter (draw_action world) actions
  | HighlightAll ids ->
      let positions =
        List.filter_map (fun id -> IdMap.find_opt id world.points) ids
      in
      if positions = [] then exit 1;
      List.iter draw_highlightpoint positions
  | _ -> ()

let draw_world (world : world) : unit =
  let draw_point id pos =
    match IdMap.find id world.colors with
    | Ink -> draw_inkpoint pos
    | MetaInk -> draw_metainkpoint pos
  in
  IdMap.iter draw_point world.points
