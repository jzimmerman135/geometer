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

let combinator_menu_dims (combs : (string * int) list) :
    (int * int * int * int) list =
  (* builds menu around 0, 0 *)
  let x, y = (0, 0) in
  let gap = 30 in
  let off = -10 in
  let n_items = List.length combs in
  let n_items' = Float.of_int n_items in
  let y' = y - (gap * n_items / 2) + 24 in
  let radius = 24. in
  let pi = 3.14 in
  let combinator_menuitem_dims index name outports =
    let rad = (Float.of_int index *. (pi /. (n_items' -. 1.))) -. (pi /. 2.) in
    let xdiff, _ydiff =
      (Int.of_float (radius *. cos rad), Int.of_float (radius *. 2. *. sin rad))
    in
    let ydiff = gap * index in
    combinator_dims (x + xdiff + off, y' + ydiff) name outports
  in
  List.mapi
    (fun index (name, ps) -> combinator_menuitem_dims index name ps)
    combs

let draw_combinator color (x, y, w, h) text outports =
  let r = h / 2 in
  let r' = Float.of_int (h / 2) in
  let draw_half_circle x y =
    draw_circle_sector (vec (x, y)) 12. 180. 360. 10 color
  in
  let rec draw_outports x' y' outports' =
    let offset = 12 + (outports' * 24) in
    if outports' < outports then (
      draw_outports x' y' (outports' + 1);
      draw_circle (x' + offset) (y' + r) r' color;
      draw_circle (x' + offset) (y' + r) (point_size +. 2.) background)
  in
  let text_offset = if outports > 0 then outports * 24 else r in
  draw_rectangle (x + r) y (w - 24) h background;
  draw_circle (x + w - r) (y + r) r' background;
  draw_circle (x + r) (y + r) r' background;
  draw_rectangle (x + r) y (w - 24) h color;
  if outports = 0 then draw_half_circle (x + r) (y + r) else draw_outports x y 0;
  draw_text text (x + text_offset + 3) y 24 text_color;
  draw_circle (x + w - r) (y + r) r' color;
  draw_circle (x + w - r) (y + r) 8. background

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
  let draw_combinator_menu combs (x, y) _emphasis_idx =
    List.iteri
      (fun index ((name, ports), dims) ->
        let col = if index = _emphasis_idx then red else highlight 0x44 in
        let x', y', w, h = dims in
        draw_combinator col (x + x', y + y', w, h) name ports)
      combs
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
            draw_combinator_menu ui.combinators pos i
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
