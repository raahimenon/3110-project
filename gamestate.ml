open Graphics
open Room
open Player
open Item
open Entity
open GameVars
open Enemy
open Vector

type state =
  {
    running : bool;
    current_room : Room.t;
    window : Window.window;
    input : Window.input
  }

let change_state (player:Player.t) st = 
  match st with
  |Move dir -> 
    {player with state = Move dir; direction = dir; 
                 curr_anim = (get_anim player dir "walk");
                 tile_destination = dir |> vec_of_dir |> to_int |> add_ints player.curr_tile;
                 reach_dest = false;}
  |Idle -> 
    {player with curr_anim = (get_anim player player.direction "idle"); 
                 curr_frame_num = 0; state = Idle; reach_dest = true;
                 pos = player.pos |> floor;
                 curr_tile = player.pos |> to_int;
                 tile_destination = player.curr_tile;}
  |_-> player

let check_if_pos_reached player = 
  let (a,b) = subtract (player.tile_destination |> from_int) player.pos  in 
  match player.direction with
  | Up -> b>0.
  | Down -> b<0.
  | Left  -> a>0.
  | Right -> a<0.

let player_move (player : Player.t)  = 
  let newpos = player.direction |> vec_of_dir |> scale_vec speed |> add player.pos 
  in 
  let player = {player with pos = newpos; curr_tile = to_int newpos;} in
  if not (check_if_pos_reached player) 
  then player else
    {player with reach_dest = true;
                 pos = (fun x -> print_endline ( print x); x)  
                     (player.tile_destination |> from_int);
                 curr_tile = player.tile_destination;}


let rec read_input input to_read = match input with
  |h::t -> if List.mem h to_read then Some h else read_input t to_read
  |[] -> None

let player_updater (st:state) (player:Player.t) = 
  let player = {player with curr_frame_num = Animations.next_frame player.curr_frame_num player.curr_anim} in
  let player = if not player.reach_dest then player else 
      begin
        match (read_input st.input [Window.w; Window.a; Window.s; Window.d;]) with
        | Some k when k = Window.w -> change_state player (Move Up)
        | Some k when k = Window.a -> change_state player (Move Left)
        | Some k when k = Window.s -> change_state player (Move Down)
        | Some _ -> change_state player (Move Right)
        | None -> change_state player (Idle)
      end in

  (*let int_pos = (int_of_float player.pos.x),(int_of_float player.pos.y) in 
    let (intx,inty) = int_pos in *)
  (*let player = 
    if (player.state <> Idle)&&(st.input = None) then change_state player Idle 
    else player
    in *)
  match player.state with 
  |Move dir -> player_move player
  |Idle -> player
  |_ -> print_endline "why is this happening" ; player

(*
let player_updater (st : state) (player: Player.t) : Player.t = 
  let player = {player with curr_frame_num = Animations.next_frame player.curr_frame_num player.curr_anim} in
  if st.input = None then 
    if player.state <> Idle then {player with curr_anim = (get_anim player player.direction "idle"); curr_frame_num = 0; state = Idle} else player
  else 
    let (x,y) = player.pos in 
    match st.input |> Option.get with
    | esc when esc = Window.esc -> exit 0
    | q when q = Window.q -> exit 0
    | w when w = Window.w -> {player with direction = Up; pos = (x,y -. 2./.tile_size); curr_anim = (get_anim player Up "walk"); state = Move Up}
    | a when a = Window.a -> {player with direction = Left; pos = (x-. 2./.tile_size,y); curr_anim = (get_anim player Left "walk"); state = Move Left}
    | s when s = Window.s -> {player with direction = Down; pos = (x,y+. 2./.tile_size); curr_anim = (get_anim player Down "walk"); state = Move Down}
    | d when d = Window.d -> {player with direction = Right; pos = (x+. 2./.tile_size,y); curr_anim = (get_anim player Right "walk"); state = Move Right}
    |_-> player
*)
(** [enemy_updater st enemy] returns a new [Enemy.t] which represents the
    changes to [enemy] imposed by [st]. Raises [Failure] if any adjacent enemy
    or player is found to have [Buff] stats rather than [Combat] stats. *)
let enemy_updater (st:state) (enemy:Enemy.t) : Enemy.t option =
  let p = st.current_room.player in
  let d = match enemy.unique_stats with
    | Combat stats -> stats.defense
    | other -> failwith "encountered enemy with malformed attributes"
  in let hp_change = 
       (* Identify nearby enemies attacking [enemy] *)
       st.current_room.enemies |> List.filter (fun e ->
           match e.state with
           | Attack dir ->
             (match dir, Vector.subtract e.pos enemy.pos  with
              | Up, (0., -1.) | Down,( 0., 1.) | Right,(-1., 0.) | Left,( 1., 0.)
                -> true
              | other -> false)
           | other -> false)
       (* Tally damage done by enemies, accounting for defense *)
       |> (List.fold_left (fun (prev : int) (e : Enemy.t) : int -> 
           match e.unique_stats with
           | Combat stats -> prev + max 0 (stats.attack - d)
           | other -> failwith "encountered enemy with malformed attributes") 0)

  (* Add any damage done by the player *)
  in let hp_change =
       hp_change + 
       (* Check if the player is trying to attack *)
       match p.state with
       | Attack dir ->
         (* Check if the player is looking at this enemy *)
         (match dir, add p.pos (enemy.pos|> scale_vec (-1.))  with
          | Up, (0., -1.) | Down, (0., 1.) | Right, (-1., 0.) | Left, (1., 0.)
            -> (match p.unique_stats with
                | Combat stats -> stats.attack
                | other -> failwith "encountered player with malformed attributes")
          | other -> 0)
       | other -> 0
  in

  (* Get new state *)
  (* TODO: ACTUAL LOGIC PARSING *)
  let new_state = Idle in

  (* Apply damage and new state to enemy *)
  if (min enemy.max_health (enemy.health - hp_change) > 0)
  then Some {
      enemy with 
      health = min enemy.max_health (enemy.health - hp_change);
      state = new_state
    }
  else None

let item_updater (st:state) (item:Item.t) =
  let p = st.current_room.player in
  (* Check if the item is in the inventory *)
  match item.pos with
  | Inventory -> item
  | Position pos ->
    (* Check if the player is trying to interact *)
    (match p.state with
     | Interact dir ->
       (* Check if the player is looking at this item *)
       let (x,y) = p.pos in 
       (match dir, x -. pos.x, y -. pos.y with
        | Up, 0., -1. | Down, 0., 1. | Right, -1., 0. | Left, 1., 0.
          -> {item with pos = Inventory}
        | other -> item)
     | other -> item)

let room_updater (st:state) room:Room.t = 
  let _ = match read_input st.input [Window.q] with |Some _ ->  exit 0 |None -> () in  
  {room with player = room.player |> player_updater st;
             enemies = room.enemies |> List.filter_map (enemy_updater st);
             items = room.items |> List.map (item_updater st)}


let rec game_loop st time = 
  let curr_time = Window.get_time () in
  let delta = curr_time - time in 
  let delay = spf_in_milli - delta in
  begin if (delay) > 0 then Window.wait (delay) else print_endline ("lag" ^ string_of_int delay)
  end;
  let curr_time = Window.get_time() in
  Window.clear st.window;
  Room.draw_room st.window st.current_room; 
  Window.render st.window;
  let input = Window.input_query st.input in
  let st = { running = if not (List.mem Window.esc input) then true else false;
             current_room = 
               st.current_room |> room_updater st;
             window = st.window;
             input = input } in
  game_loop st curr_time