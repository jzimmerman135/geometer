open Raylib
open Base

let ( >>> ) f g x = g (f x)

(* points in a selection refer to the original position
   they were at but for rendering we want the "current" position *)
let pseudoshift_ui world ({ selected; _ } as ui) =
  let shift (id, pos) : point =
    ( id,
      match IdMap.find_opt id world.points with
      | Some newpos -> newpos
      | None -> pos )
  in
  let selected' =
    match selected with
    | ActivelySelecting pts -> ActivelySelecting (List.map shift pts)
    | Selected pts -> Selected (List.map shift pts)
    | x -> x
  in
  { ui with selected = selected' }

let draw_inkline (p1 : position) (p2 : position) =
  draw_line_ex (vec p1) (vec p2) 3.0 blue

let draw_metainkline (p1 : position) (p2 : position) =
  draw_line_ex (vec p1) (vec p2) 3.0 red

let draw_inkpoint (x, y) = draw_circle x y point_size blue
let draw_metainkpoint (x, y) = draw_circle x y point_size red

let combinator_dims (x, y) text outports =
  let w = max 36 (12 * (String.length text + 2)) in
  let h = 24 in
  let w' = w + ((outports + 1) * 24) - 12 in
  (x, y - 12, w', h)

let draw_combinator color (x, y, w, h) text outports =
  draw_rectangle_lines x y w h green;
  let rec draw_outports x' y' outports' =
    let offset = 12 + (outports' * 24) in
    if outports' < outports then (
      draw_outports x' y' (outports' + 1);
      draw_circle (x' + offset) (y' + 12) 12. color;
      draw_circle (x' + offset) (y' + 12) 8. background)
  in
  draw_rectangle (x + 12) y (w - 24) h color;
  draw_outports x y 0;
  draw_text text (x + (outports * 24) + 3) y 24 text_color;
  draw_circle (x + w - 12) (y + 12) 12. color;
  draw_circle (x + w - 12) (y + 12) 8. background;
  (x, y, w, h)

let combinator_menu_dims (x, y) (combs : (string * int) list) :
    (int * int * int * int) list =
  let gap = 36 in
  let off = 48 in
  let n_items = List.length combs in
  let n_items' = Float.of_int n_items in
  let radius = 48. in
  let pi = 3.14 in
  let combinator_menuitem_dims index name outports =
    let rad = (Float.of_int index *. (pi /. (n_items' -. 1.))) -. (pi /. 2.) in
    let xdiff, _ydiff =
      (Int.of_float (radius *. cos rad), Int.of_float (radius *. 2. *. sin rad))
    in
    let ydiff = gap * index in
    combinator_dims (x + xdiff + off, y + ydiff) name outports
  in
  List.mapi
    (fun index (name, ps) -> combinator_menuitem_dims index name ps)
    combs

let draw_combinator_menu (x, y) emphasis_idx : (int * int * int * int) list =
  let gap = 36 in
  let off = 48 in
  let combs =
    [ ("sin", 1); ("hello", 3); ("tan", 2); ("add", 3); ("mul", 2); ("cos", 1) ]
  in
  let n_items = List.length combs in
  let n_items' = Float.of_int n_items in
  let y = y - (gap * n_items / 2) + 24 in
  let draw_combinator_menuitem color index name outports =
    let radius = 48. in
    let pi = 3.14 in
    let rad = (Float.of_int index *. (pi /. (n_items' -. 1.))) -. (pi /. 2.) in
    let xdiff, _ydiff =
      (Int.of_float (radius *. cos rad), Int.of_float (radius *. 2. *. sin rad))
    in
    let ydiff = gap * index in
    let dims = combinator_dims (x + xdiff + off, y + ydiff) name outports in
    draw_combinator color dims name outports
  in
  List.fold_left
    (fun (index, res) (name, ports) ->
      let color = if index == emphasis_idx then red else highlight 0x44 in
      let res' = draw_combinator_menuitem color index name ports :: res in
      (index + 1, res'))
    (0, []) combs
  |> snd

let draw_ui ui =
  let draw_mode mode =
    let draw_uibox should_underline color text w x =
      draw_rectangle x 10 w 30 color;
      draw_text text (x + 10) 12 30 background;
      let offset = 5 in
      if should_underline mode then
        draw_rectangle (x + offset) 42 (w - (offset * 2)) 3 color
    in
    draw_uibox is_mode_ink blue "ink" 80 10;
    draw_uibox is_mode_metaink red "meta-ink" 150 90;
    match mode with
    | MetaMetaMode _ ->
        draw_uibox
          (fun _ -> true)
          green "meta-meta" 180
          (get_screen_width () - 190)
    | _ -> ()
  in
  let rec draw_selection : selection -> unit =
    let draw_point_highlight ((x, y) : position) =
      draw_circle_lines x y point_highlight_size highlight_em;
      draw_circle x y point_highlight_size (highlight 0x33)
    in
    function
    | Selected pts -> List.iter (snd >>> draw_point_highlight) pts
    | ActivelySelecting pts -> (
        match ui.input with
        | Clicked (Plain, EmptySpace (startx, starty))
        | DraggingFrom (Plain, EmptySpace (startx, starty)) ->
            let mousex, mousey = ui.mousepos in
            let diffx, diffy = (abs (mousex - startx), abs (mousey - starty)) in
            let minx, miny = (min mousex startx, min mousey starty) in
            draw_rectangle minx miny diffx diffy (highlight 0x22);
            draw_rectangle_lines minx miny diffx diffy (highlight 0x44);
            draw_selection (Selected pts)
        | _ ->
            raise
              (Failure
                 "ActivelySelecting but not DraggingFromEmptySpace or \
                  ClickedEmptySpace"))
    | CombinatorMenuSelection i -> (
        match ui.input with
        | DraggingFrom (_, EmptySpace pos) ->
            let a, b, c, d = combinator_dims ui.mousepos "hey" 3 in
            draw_rectangle_lines a b c d blue;

            List.iter ignore
              (* (fun (x, y, z, w) -> draw_rectangle_lines x y (z - x) (w - y) red) *)
              (draw_combinator_menu pos i)
        | _ -> ())
    | NoSelection -> ()
  in
  let draw_action : user_action -> unit = function
    | DraggingFrom (Shift, Point (_, pos)) when is_mode_ink ui.mode ->
        draw_inkline pos ui.mousepos
    | DraggingFrom (Shift, Point (_, pos)) when is_mode_metaink ui.mode ->
        draw_metainkline pos ui.mousepos
    | _ -> ()
  in
  draw_action ui.input;
  draw_mode ui.mode;
  draw_selection ui.selected

let draw_world (world : world) : unit =
  let draw_point id pos =
    match IdMap.find id world.colors with
    | Ink -> draw_inkpoint pos
    | MetaInk -> draw_metainkpoint pos
  in
  let draw_connector id (startpt, endpt) =
    let startpos = IdMap.find startpt world.points in
    let endpos = IdMap.find endpt world.points in
    match IdMap.find id world.colors with
    | Ink -> draw_inkline startpos endpos
    | MetaInk -> draw_metainkline startpos endpos
  in
  IdMap.iter draw_connector world.lines;
  IdMap.iter draw_point world.points
