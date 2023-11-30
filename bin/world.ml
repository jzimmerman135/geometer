open Base

let idcount = ref (-1)

let idgen () =
  idcount := !idcount + 1;
  !idcount

let empty = { points = IdMap.empty; lines = IdMap.empty; colors = IdMap.empty }

let add_point id pos ({ points; colors; _ } as world) =
  print_endline
    (String.concat " "
       [ "adding point with id:"; Int.to_string id; "pos:"; pos_to_string pos ]);
  {
    world with
    points = IdMap.add id pos points;
    colors = IdMap.add id Ink colors;
  }

let add_line id (ends : lineends) ({ points; colors; _ } as world) =
  {
    world with
    lines = IdMap.add id ends points;
    colors = IdMap.add id Ink colors;
  }

let add_metaline id (ends : lineends) ({ points; colors; _ } as world) =
  {
    world with
    lines = IdMap.add id ends points;
    colors = IdMap.add id MetaInk colors;
  }

let rec update world (action : uiaction) : world =
  match action with
  | Seq actions -> List.fold_left update world actions
  | AddPoint (id, pos) -> add_point id pos world
  | MovePoint (id, _, newpos) when IdMap.mem id world.points ->
      add_point id newpos world
  | _ -> world
