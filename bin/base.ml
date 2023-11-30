open Raylib
module IdMap = Map.Make (Int)
module IdSet = Set.Make (Int)

type color = Raylib.Color.t

let background : Color.t = Color.create 0xfe 0xf9 0xef 0xff
let blue : Color.t = Color.create 0x35 0x5C 0x7D 0xff
let red : Color.t = Color.create 0xf6 0x72 0x80 0xff
let green : Color.t = Color.create 0x6A 0x9C 0x89 0xff
let highlight = Color.create 20 208 222
let highlight_em = Color.create 20 130 140 0xff
let point_size = 6.0
let point_highlight_size = 14.0

(* WORLD *)

type stuff = Ink | MetaInk
type id = int

type linemap = lineends IdMap.t
and lineends = id * id

type pointmap = position IdMap.t
and position = int * int

type stuffmap = stuff IdMap.t
type world = { points : pointmap; lines : linemap; colors : stuffmap }

(* UI *)

type mode = InkMode | MetaInkMode | MetaMetaMode of mode

and selection =
  | NoSelection
  | Selected of point list
  | ActivelySelecting of point list

and point = id * position

type user_action =
  | Clicked of clickorigin
  | DraggingFrom of clickorigin
  | DragReleasedFrom of clickorigin * position
  | RightClicked
  | Delete
  | Undo
  | Redo
  | Nothing

and clickorigin = whichclick * place
and whichclick = Plain | Shift
and place = EmptySpace of position | Point of point

type ui = {
  mousepos : int * int;
  selected : selection;
  mode : mode;
  input : user_action;
}

type uiaction =
  | AddPoint of point
  | MovePoint of id * position
  | AddLine of id * id
  | DeletePoint of point
  (* | AddMetaPoint of id * position *)
  (* | AddMetaLine of id * id *)
  | Seq of uiaction list
  | NoAction

let is_shift f = f Key.Left_shift || f Key.Right_shift
let pos_to_string (x, y) = "(" ^ Int.to_string x ^ ", " ^ Int.to_string y ^ ")"

let point_to_string (id, pos) =
  "id: " ^ Int.to_string id ^ "pos: " ^ pos_to_string pos

let ui_to_string { selected; mode; mousepos; input } =
  let open String in
  let clkstr = function Plain -> "" | Shift -> "shift " in
  let plcstr = function
    | Point pt -> point_to_string pt
    | EmptySpace pos -> pos_to_string pos
  in
  let input_str =
    match input with
    | Nothing -> "nothing"
    | RightClicked -> "right click"
    | Clicked (clk, place) -> clkstr clk ^ "clicked" ^ plcstr place
    | DraggingFrom (cl, place) -> clkstr cl ^ "dragging from " ^ plcstr place
    | Delete -> "delete"
    | Undo -> "undo"
    | Redo -> "redo"
    | DragReleasedFrom ((cl, place), stop) ->
        clkstr cl
        ^ concat " "
            [
              plcstr place;
              "drag released from";
              plcstr place;
              "to";
              pos_to_string stop;
            ]
  in
  let mode_str =
    match mode with
    | InkMode -> "ink"
    | MetaInkMode -> "meta-ink"
    | MetaMetaMode InkMode -> "ink (meta-meta)"
    | MetaMetaMode MetaInkMode -> "meta-ink (meta-meta)"
    | MetaMetaMode (MetaMetaMode _) ->
        raise (Failure "unreacheable meta-meta (meta-meta)")
  in
  let mousepos_str = pos_to_string mousepos in
  let selected_str =
    match selected with
    | NoSelection -> "none"
    | Selected pts ->
        "selected: [" ^ concat ", " (List.map point_to_string pts) ^ "]"
    | ActivelySelecting pts ->
        "actively selecting: ["
        ^ concat ", " (List.map point_to_string pts)
        ^ "]"
  in
  concat "\n"
    [
      "input: " ^ input_str;
      "mode: " ^ mode_str;
      "mousepos: " ^ mousepos_str;
      "selected: " ^ selected_str;
    ]
