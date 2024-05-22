open Pruebaraylib.Game

let screen = 800
let lenght = 80

let setup () =
  Raylib.init_window screen screen "GameOfLife";
  Raylib.set_target_fps 5;
  lenght |> Game.init_world |> Game.alive_random
;;

let rec loop (state : Game.world) =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
    let open Raylib in
    begin_drawing ();
    clear_background Color.black;
    draw_text (string_of_int state.generation) 10 10 20 Color.green;
    draw_text (string_of_int state.alive) 10 30 20 Color.green;
    for i = 0 to lenght - 1 do
      for j = 0 to lenght - 1 do
        match state.cells.(i).(j) with
        | Alive -> draw_rectangle (i * 10) (j * 10) 10 10 Color.white
        | Dead -> ()
      done
    done;
    end_drawing ();
    state |> Game.update |> loop
;;

let () = setup () |> loop
