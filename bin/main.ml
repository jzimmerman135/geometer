open Base

let setup () =
  Raylib.init_window 900 506 "Geometer";
  Raylib.set_target_fps 400;
  (World.empty, Ui.empty)

let rec loop (world, ui) =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background background;
    draw_text
      ("FPS: " ^ Int.to_string (Raylib.get_fps ()))
      10
      (get_screen_height () - 20)
      20 Color.lightgray;
    if is_key_down Key.U then print_endline (ui_to_string ui);
    let ui', action = Ui.poll ui world |> Ui.action world in
    let world' = World.update world action in
    Render.pseudoshift_ui world' ui' |> Render.draw_ui;
    Render.draw_world world';
    end_drawing ();
    loop (world', ui')

let () = setup () |> loop
