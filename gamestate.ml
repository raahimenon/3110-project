open Graphics
open Room
open Player
open Entity
open GameVars

type state =
  {
    running: bool;
    current_room: Room.t
  }

let player_updater (st:state) player:Player.t = 
  let player = {player with curr_frame_num = Animations.next_frame player.curr_frame_num player.curr_anim} in
  if not (Graphics.key_pressed ()) then player else match Graphics.read_key () with
    |'w' -> {player with pos = {player.pos with y = player.pos.y +. 1.}}
    |'a' -> {player with pos = {player.pos with x = player.pos.x -. 1.}}
    |'s' -> {player with pos = {player.pos with y = player.pos.y -. 1.}}
    |'d' -> {player with pos = {player.pos with x = player.pos.x +. 1.}}
    |'p' -> print_endline (string_of_int player.curr_frame_num); player
    |'q' -> exit 0
    |_-> player


let rec game_loop st time = 
  let curr_time = Unix.gettimeofday () in
  let delta = curr_time -. time in 
  if not st.running then () 
  else if delta > spf then
    let event = wait_next_event [Poll] in
    let st = { running = not (event.key = Char.chr 27);
               current_room = 
                 update_room st.current_room (player_updater st)} 
    in
    Graphics.clear_graph ();
    Room.draw_room st.current_room; 
    game_loop st curr_time
  else game_loop st time  