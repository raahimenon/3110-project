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
   tiles = (Room.Floor (Animations.load_image "player.txt")) |> Array.make_matrix 2 2 }


let main = open_graph " 600x600"; Gamestate.game_loop 
    {running = true; current_room= default_room} (Unix.gettimeofday ());
  close_graph

let () = main ()