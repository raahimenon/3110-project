open Entity
open Player
open Room
open Gamestate
open Animations
open Combat
open Item
open Enemy

let main () = 

  let () = print_string "JSON File to Load in Saves Folder (If unsure, type '0.json'):"  in
  let load_file = read_line () in

  let window = Window.create_window "3110 Project" (GameVars.width * (int_of_float GameVars.tile_size)) (GameVars.height * (int_of_float GameVars.tile_size)) in

  (* let default_enemy = make_enemy "link" 3 window in  *)

  (* let default_room = {(Load.load load_file window)  with items =[default_item; default_item2]; enemies = [default_enemy;]} in *)

  let () = Window.clear window in
  let () = Window.render window in
  let time = Window.get_time () in
  Gamestate.game_loop 
    {running = true;
     current_room = Load.load load_file window;
     window = window;
     input = [];
     icons = Animations.load_icons (Window.get_renderer window);
     last_anim_frame = time
    } time;
  Window.wait 5;
  Window.exit_window window

let () = main ()