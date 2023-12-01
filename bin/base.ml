module IdMap = Map.Make (Int)
module IdSet = Set.Make (Int)
module Color = Raylib.Color
module Vector2 = Raylib.Vector2
module Key = Raylib.Key

type color = Raylib.Color.t

let background : Color.t = Color.create 0xfe 0xf9 0xef 0xff
let text_color_base : int -> Color.t = Color.create 0x35 0x5C 0x6D
let blue_base : int -> Color.t = Color.create 0x35 0x5C 0x7D
let red_base : int -> Color.t = Color.create 0xf6 0x72 0x80
let blue = blue_base 0xdd
let red = red_base 0xaa
let text_color = text_color_base 0xaa
let green : Color.t = Color.create 0x6A 0x9C 0x89 0xdd
let highlight = Color.create 20 208 222
let highlight_em = Color.create 20 130 140 0xff
let point_size = 6.0
let point_highlight_size = 12.0
let line_width = 3.0
let line_highlight_width = 5.0

(* WORLD *)

type stuff = Ink | MetaInk
type position = int * int
type id = int
type point = id * position
type lineends = id * id

type world = {
  points : position IdMap.t;
  lines : (id * id) IdMap.t;
  colors : stuff IdMap.t;
  combinators : (string * position) IdMap.t;
}

type relationship = Port of string * id

type uiaction =
  | AddPoint of stuff * point
  | AddLine of stuff * id * (id * id)
  | MovePoint of id * position
  | DeletePoint of point
  | Seq of uiaction list
  | NoAction

(* UI *)

type mode = InkMode | MetaInkMode | MetaMetaMode of mode

and selection =
  | NoSelection
  | Selected of point list
  | ActivelySelecting of point list
  | CombinatorMenuSelection of int

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

type combinator = (string * int) * rect
and rect = int * int * int * int

type ui = {
  combinators : combinator list;
  mousepos : int * int;
  selected : selection;
  mode : mode;
  animation : float;
  input : user_action;
}

(* UTILS *)

let vec ((x, y) : position) = Vector2.create (Float.of_int x) (Float.of_int y)
let is_shift f = f Key.Left_shift || f Key.Right_shift
let is_mode_ink = function InkMode | MetaMetaMode InkMode -> true | _ -> false

let is_mode_metaink = function
  | MetaInkMode | MetaMetaMode MetaInkMode -> true
  | _ -> false

let get_selected = function
  | ActivelySelecting pts | Selected pts -> pts
  | CombinatorMenuSelection _ | NoSelection -> []

(* STRINGIFIERS *)

let rect_to_string (x, y, w, h) =
  String.concat " "
    [
      "x: " ^ Int.to_string x;
      "y: " ^ Int.to_string y;
      "w: " ^ Int.to_string w;
      "h: " ^ Int.to_string h;
    ]

let pos_to_string (x, y) = "(" ^ Int.to_string x ^ ", " ^ Int.to_string y ^ ")"

let line_to_string id (startid, endid) =
  let istr = Int.to_string in
  String.concat " "
    [ "Line:"; istr id; "Start:"; istr startid; "End:"; istr endid ]

let point_to_string (id, pos) =
  "id: " ^ Int.to_string id ^ " pos: " ^ pos_to_string pos

let ui_to_string { selected; mode; mousepos; input; animation; _ } =
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
    | CombinatorMenuSelection _ -> "combinator menu"
  in
  concat "\n"
    [
      "input: " ^ input_str;
      "mode: " ^ mode_str;
      "animation: " ^ Float.to_string animation;
      "mousepos: " ^ mousepos_str;
      "selected: " ^ selected_str;
    ]
