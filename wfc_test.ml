open Entity
open Player
open Room
open Gamestate
open Animations
open Combat
open Item

let wfc_test () = 
  Random.self_init ();
  let random_seed = Random.bits () in

  let () = print_string "WHEE" in

  let window = Window.create_window "3110 Project" (GameVars.width * (int_of_float GameVars.tile_size)) (GameVars.height * (int_of_float GameVars.tile_size)) in

  let () = Window.clear window in
  let () = Window.render window in
  Gamestate.game_loop 
    {running = true;
     current_room = Room_gen.simple_gen random_seed window;
     window = window;
     input = [];
    } (Window.get_time ());
  Window.wait 5;
  Window.exit_window window

let () = wfc_test ()