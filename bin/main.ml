open Base

let setup () =
  Raylib.init_window 900 506 "Geometer";
  Raylib.set_target_fps 120

let appinit = (World.empty, Ui.empty)

let rec initloop (world, ui) =
  let continue =
    let open Raylib in
    List.exists is_key_down [ Key.Space; Key.M ]
    || List.exists is_mouse_button_down [ MouseButton.Left; MouseButton.Right ]
  in
  match continue with
  | true -> 0
  | false when Raylib.window_should_close () -> 1
  | _ ->
      let open Raylib in
      begin_drawing ();
      if !Render.background_pts = [] then
        Render.background_pts :=
          Render.gen_background_pts
            (get_screen_width () / 100)
            (get_screen_height () / 100);
      Render.draw_background ();
      draw_text
        ("FPS: " ^ Int.to_string (Raylib.get_fps ()))
        10
        (get_screen_height () - 20)
        20 Color.lightgray;
      Render.draw_controls ();
      if is_key_down Key.U then print_endline (ui_to_string ui);
      Render.draw_world world;
      Render.pseudoshift_ui world ui |> Render.draw_ui;
      end_drawing ();
      initloop (world, ui)

let rec loop (world, ui) =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    Render.draw_background ();
    Render.draw_fps ();
    if is_key_down Key.U then print_endline (ui_to_string ui);
    if is_key_down Key.H then Render.draw_controls ();
    let ui', action = Ui.poll ui world |> Ui.action world in
    let world' = World.update world action in
    Render.draw_world world';
    Render.pseudoshift_ui world' ui' |> Render.draw_ui;
    end_drawing ();
    loop (world', ui')

let () =
  setup ();
  if initloop appinit = 0 then loop appinit else Raylib.close_window ()
