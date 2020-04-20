open Graphics
open Entity
open Player
open Room
open Gamestate
open Animations
open Combat

let default_player = {
  animations= [];
  curr_anim = "";
  curr_frame_num= 0;
  size = 0,0;
  name = 0,0;
  frame = Animations.load_image "player.txt";
  pos = {x=0;y=0};
  id = 0;
  max_health = 0;
  health = 0;
  state = 0;
  unique_stats = Combat {attack =0; defense =0; movement_speed =0;};
}

let default_room = 
  {player = default_player; 
   tiles = (Room.Floor (Animations.load_image "player.txt")) |> Array.make_matrix 2 2 }


let main = open_graph " 480x270"; Gamestate.game_loop 
    {running = true; current_room= default_room} (Unix.gettimeofday ());
  close_graph

let () = main ()