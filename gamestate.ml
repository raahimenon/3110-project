open Graphics
open Room
open Player
open Entity


type state =
  {
    running: bool;
    current_room: Room.t
  }

let player_updater (st:state) (input:Graphics.status) player:Player.t = 
  if not (Graphics.key_pressed ()) then player else match Graphics.read_key () with
    |'w' -> {player with pos = {player.pos with y = player.pos.y +10}}
    |'a' -> {player with pos = {player.pos with x = player.pos.x -10}}
    |'s' -> {player with pos = {player.pos with y = player.pos.y -10}}
    |'d' -> {player with pos = {player.pos with x = player.pos.x +10}}
    |_-> player


let rec game_loop st time = let newtime = Unix.gettimeofday () in
  let delta = newtime -. time in 
  if not st.running then () else 
    let event = wait_next_event [Poll] in
    let st = { running = not (event.key = Char.chr 27);
               current_room = 
                 Room.update_room st.current_room (player_updater st event)} 
    in
    Graphics.clear_graph ();
    Room.draw_room st.current_room; 
    if st.running then
      let () = if delta < 0.016 then Unix.sleepf (0.016-.delta) else () in
      game_loop st newtime else ()

