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
let point_size = 10.0

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

type mode = InkMode | MetaInkMode

type selected = point list
and point = id * position

type ui = {
  mouse_pos : int * int;
  selected : selected;
  mode : mode;
  is_metameta_mode : bool;
  mousedrag_startpos : position option;
}

type uiaction =
  | HighlightAll of id list
  | HighlightQuad of position * position
  | AddPoint of id * position
  | MovePoint of id * position * position
  (* | AddLine of id * id *)
  (* | AddMetaPoint of id * position *)
  (* | AddMetaLine of id * id *)
  | Seq of uiaction list
  | NoAction

let is_shift f = f Key.Left_shift || f Key.Right_shift
let pos_to_string (x, y) = "(" ^ Int.to_string x ^ ", " ^ Int.to_string y ^ ")"

let point_to_string (id, pos) =
  "id: " ^ Int.to_string id ^ "pos: " ^ pos_to_string pos

let ui_to_string
    { selected; mode; is_metameta_mode; mouse_pos; mousedrag_startpos } =
  let state_string =
    match selected with
    | [] -> "No selection"
    | pts ->
        "Selected [" ^ String.concat "," (List.map point_to_string pts) ^ "]"
  in
  let mode_string =
    match mode with InkMode -> "ink" | MetaInkMode -> "meta-ink"
  in
  let mousedrag_string =
    match mousedrag_startpos with
    | Some pos -> pos_to_string pos
    | None -> "No drag"
  in
  String.concat "\n"
    [
      "state: " ^ state_string;
      "mode: " ^ mode_string;
      "mousepos: " ^ pos_to_string mouse_pos;
      "mousedrag_startpos: " ^ mousedrag_string;
      "meta-meta: " ^ Bool.to_string is_metameta_mode;
      "shift_down: " ^ Bool.to_string (is_shift is_key_down);
      "mouse_down: " ^ Bool.to_string (is_mouse_button_down MouseButton.Left);
    ]
