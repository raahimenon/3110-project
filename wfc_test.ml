open Entity
open Player
open Room
open Gamestate
open Animations
open Combat
open Item

let wfc_test () =
  print_endline "window init";
  let window = Window.create_window "3110 Project" (GameVars.width * (int_of_float GameVars.tile_size)) (GameVars.height * (int_of_float GameVars.tile_size)) in
  print_endline "input init";
  let f = Floor (Animations.load_image "./sprites/room/floor.bmp" (Window.get_renderer window)) in
  let w = Wall (Animations.load_image "./sprites/room/wall.bmp" (Window.get_renderer window)) in
  let input =
    [|
      [| w ; f ; w ; w |];
      [| w ; f ; w ; w |];
      [| w ; f ; f ; f |];
      [| w ; f ; f ; f |];
    |] in

  print_endline "begin wfc";
  Save.save
    (Room_gen.generate_room (0) (input) (2) (10) (10) (0))
    ("wfc_test"^(string_of_int (Random.int 1000000))^".json");
  print_endline "wfc complete";


  let () = print_string "Select a file to load:"  in
  Sys.readdir "saves" |> Array.iter (fun f -> (print_string ("\n\t"^f)));

  let load_file = read_line () in
  let default_room = (Load.load load_file window) in

  let () = Window.clear window in
  let () = Window.render window in
  Gamestate.game_loop 
    {running = true;
     current_room = default_room;
     window = window;
     input = [];
    } (Window.get_time ());
  Window.wait 5;
  Window.exit_window window

let () = wfc_test ()