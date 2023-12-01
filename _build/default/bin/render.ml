open Raylib
open Base

let ( >>> ) f g x = g (f x)

(* points in a selection refer to the original position
   they were at but for rendering we want the "current" position *)
let pseudoshift_ui world ({ selected; _ } as ui) =
  let shift (id, _) : point = (id, IdMap.find id world.points) in
  let selected' =
    match selected with
    | ActivelySelecting pts -> ActivelySelecting (List.map shift pts)
    | Selected pts -> Selected (List.map shift pts)
    | x -> x
  in
  { ui with selected = selected' }

let draw_inkline (p1 : position) (p2 : position) =
  draw_line_ex (vec p1) (vec p2) 4.0 blue

let draw_metainkline (p1 : position) (p2 : position) =
  draw_line_ex (vec p1) (vec p2) 4.0 blue

let draw_ui ui =
  let draw_mode mode =
    let draw_uibox should_underline color text w x =
      draw_rectangle x 10 w 30 color;
      draw_text text (x + 10) 12 30 background;
      let offset = 5 in
      if should_underline mode then
        draw_rectangle (x + offset) 42 (w - (offset * 2)) 3 color
    in
    let is_mode_ink = function
      | InkMode | MetaMetaMode InkMode -> true
      | _ -> false
    in
    let is_mode_metaink = function
      | MetaInkMode | MetaMetaMode MetaInkMode -> true
      | _ -> false
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
    | NoSelection -> ()
  in
  let draw_action : user_action -> unit = function
    | DraggingFrom (Shift, Point (_, pos)) -> draw_inkline pos ui.mousepos
    | _ -> ()
  in
  draw_action ui.input;
  draw_mode ui.mode;
  draw_selection ui.selected

let draw_world (world : world) : unit =
  let draw_inkpoint (x, y) = draw_circle x y point_size blue in
  let draw_metainkpoint (x, y) = draw_circle x y point_size red in
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
  IdMap.iter draw_point world.points;
  IdMap.iter draw_connector world.lines
