open Entity
open Player
open Room
open Gamestate
open Animations
open Combat

let default_player = make_player "link" 0

let default_room = Load.load "test_save.json"


let main () = 
  let window = Window.create_window "3110 Project" (GameVars.width * (int_of_float GameVars.tile_size)) (GameVars.height * (int_of_float GameVars.tile_size)) in
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

let () = main ()