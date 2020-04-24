open Graphics
open Entity
open Player
open Room
open Gamestate
open Animations
open Combat

let default_player = make_player "link" 0

let default_room = 
  {player = default_player; 
   enemies =[];
   items =[];
   tiles = (Room.Floor (Animations.load_image "./sprites/link/down/walk/link_down_walk_1.bmp")) |> Array.make_matrix 2 2 }


let main () = 
  let window = Window.create_window "3110 Project" 320 320 in
  let () = Window.clear window in
  let () = Window.render window in
  Gamestate.game_loop 
    {running = true;
     current_room = default_room;
     window = window;
     input = None
    };
  Window.wait 5.;
  Window.exit_window window

let () = main ()