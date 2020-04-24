open Graphics
open Room
open Player
open Item
open Entity
open GameVars
open Enemy

type state =
  {
    running : bool;
    current_room : Room.t;
    window : Window.window;
    input : Window.input
  }


let player_updater (st : state) (player: Player.t) : Player.t = 
  let player = {player with curr_frame_num = Animations.next_frame player.curr_frame_num player.curr_anim} in
  if st.input = None then 
    if player.state <> Idle then {player with curr_anim = (get_anim player player.direction "idle"); curr_frame_num = 0; state = Idle} else player
  else match st.input |> Option.get with
    | esc when esc = Window.esc -> exit 0
    | q when q = Window.q -> exit 0
    | w when w = Window.w -> {player with direction = Up; pos = {player.pos with y = player.pos.y -. 2./.tile_size}; curr_anim = (get_anim player Up "walk"); state = Move Up}
    | a when a = Window.a -> {player with direction = Left; pos = {player.pos with x = player.pos.x -. 2./.tile_size}; curr_anim = (get_anim player Left "walk"); state = Move Left}
    | s when s = Window.s -> {player with direction = Down; pos = {player.pos with y = player.pos.y +. 2./.tile_size}; curr_anim = (get_anim player Down "walk"); state = Move Down}
    | d when d = Window.d -> {player with direction = Right; pos = {player.pos with x = player.pos.x +. 2./.tile_size}; curr_anim = (get_anim player Right "walk"); state = Move Right}
    |_-> player

let enemy_updater (st:state) enemy:Enemy.t = failwith "unimplemented"

let item_updater (st:state) item:Item.t = failwith "unimplemented"

let room_updater (st:state) room:Room.t = 
  {room with player = room.player |> player_updater st;
             enemies = room.enemies |> List.map (enemy_updater st);
             items= room.items |> List.map (item_updater st)}


let rec game_loop st = 
  Window.clear st.window;
  Room.draw_room st.window st.current_room; 
  Window.render st.window;
  let input = Window.input_query () in
  let st = { running = if input = None then true else not (input |> Option.get = Window.esc);
             current_room = 
               st.current_room |> room_updater st;
             window = st.window;
             input = input } in
  Window.wait spf;
  game_loop st