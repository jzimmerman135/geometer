open Base

(* global identifier count *)
let idcount = ref (-1)

let idgen () =
  idcount := !idcount + 1;
  !idcount

let empty =
  {
    points = IdMap.empty;
    lines = IdMap.empty;
    colors = IdMap.empty;
    combinators = IdMap.empty;
  }

let add_point id pos ({ points; colors; _ } as world) =
  {
    world with
    points = IdMap.add id pos points;
    colors = IdMap.add id Ink colors;
  }

let add_metapoint id pos ({ points; colors; _ } as world) =
  {
    world with
    points = IdMap.add id pos points;
    colors = IdMap.add id MetaInk colors;
  }

let remove_point id ({ points; colors; lines; _ } as world) =
  {
    world with
    points = IdMap.remove id points;
    colors = IdMap.remove id colors;
    lines =
      IdMap.filter
        (fun _ (startid, endid) -> startid <> id && endid <> id)
        lines;
  }

let add_line_internal stuff id (ends : lineends) ({ lines; colors; _ } as world)
    =
  let swap (e1, e2) = (e2, e1) in
  if IdMap.exists (fun _ ends' -> ends = ends' || swap ends = ends') lines then
    world
  else
    {
      world with
      lines = IdMap.add id ends lines;
      colors = IdMap.add id stuff colors;
    }

let add_line = add_line_internal Ink

let remove_line id ({ lines; colors; _ } as world) =
  { world with lines = IdMap.remove id lines; colors = IdMap.remove id colors }

let add_metaline = add_line_internal MetaInk

let add_combinator id c ({ combinators; _ } as world : world) =
  { world with combinators = IdMap.add id c combinators }

let points_in_rect world (x1, y1) (x2, y2) : point list =
  let ptsize = Int.of_float point_size in
  let minx, miny = (min x1 x2 - ptsize, min y1 y2 - ptsize) in
  let maxx, maxy = (max x1 x2 + ptsize, max y1 y2 + ptsize) in
  let inbounds _ (x, y) = minx < x && x < maxx && miny < y && y < maxy in
  IdMap.filter inbounds world.points |> IdMap.to_seq |> List.of_seq

let rec update world (action : uiaction) : world =
  match action with
  | Seq actions -> List.fold_left update world actions
  | AddPoint (Ink, (id, pos)) -> add_point id pos world
  | AddLine (Ink, id, ends) -> add_line id ends world
  | AddPoint (MetaInk, (id, pos)) -> add_metapoint id pos world
  | AddLine (MetaInk, id, ends) -> add_metaline id ends world
  | MovePoint (id, newpos) when IdMap.mem id world.points ->
      { world with points = IdMap.add id newpos world.points }
  | DeletePoint (id, _) -> remove_point id world
  | AddCombinator (id, (name, pos, inports)) ->
      add_combinator id (name, pos, inports, idgen ()) world
  | _ -> world
