open Entity
open Player
open Room
open Gamestate
open Animations
open Combat
open Item

let main () = 

  let () = print_string "JSON File to Load in Saves Folder (If unsure, type '0.json'):"  in
  let load_file = read_line () in

  let window = Window.create_window "3110 Project" (GameVars.width * (int_of_float GameVars.tile_size)) (GameVars.height * (int_of_float GameVars.tile_size)) in

  let default_item = make_item "blue-rupee" 1 window 2 3 in
  let default_item2 = make_item "blue-rupee" 2 window 5 8 in

  let default_room = {(Load.load load_file window)  with items =[default_item; default_item2]} in

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