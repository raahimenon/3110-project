open Entity
open Player
open Room
open Gamestate
open Animations
open Combat
open Item

let main () = 
  let window = Window.create_window "3110 Project" (GameVars.width * (int_of_float GameVars.tile_size)) (GameVars.height * (int_of_float GameVars.tile_size)) in

  let default_item = make_item "link" 0 window in

  let default_room = {(Load.load "test_save.json" window)  with items =[default_item]} in

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