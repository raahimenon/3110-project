open Room
open Item
open Entity
open GameVars
open Enemy
open Player
open Vector

(** [state] is the current game state *)
type state =
  {
    running : bool;
    current_room : Room.t;
    window : Window.window;
    input : Window.input
  }

(** [change_state p st] is the player [p] updated by the current state [st] *)
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
                 pos = player.pos (*|> floor*);
                 curr_tile = player.pos |> to_int;
                 tile_destination = player.curr_tile;}
  |Interact (dir,time) -> 
    {player with curr_anim = (get_anim player player.direction "idle"); 
                 curr_frame_num = 0; state = Interact (dir,time); 
                 reach_dest = false;
                 pos = player.pos (*|> floor*);
                 curr_tile = player.pos |> to_int;
                 tile_destination = player.curr_tile;}

  |_-> player
(** [check_if_pos_reached p] checks if a moving player has reached the tile
    they are attempting to get to (in which case they can stop moving) *)
let check_if_pos_reached player = 
  let (a,b) = subtract (player.tile_destination |> from_int) player.pos  in 
  match player.direction with
  | Up -> b>0.
  | Down -> b<0.
  | Left  -> a>0.
  | Right -> a<0.

(** [collision_player t rm] checks if there is a collidable object at tile [t]*)
let collision_player tile rm = Room.entity_at_tile rm tile

(** [player_move p rm] is the player [p] in room [rm] after moving *)
let player_move (player : Player.t) rm = 
  (*if (collision_player player.tile_destination rm)
    then {player with reach_dest = true; tile_destination = player.curr_tile;} 
    else *)
  let newpos = player.direction |> vec_of_dir |> scale_vec speed |> add player.pos 
  in 
  let newplayer = {player with pos = newpos; curr_tile = to_int newpos;} in 
  if Room.collision_with_player  rm newplayer <> None then player else newplayer

(*if not (check_if_pos_reached player) 
  then player else
  {player with reach_dest = true;
               pos = (*(fun x -> print_endline ( print x); x)  *)
                 (player.tile_destination |> from_int);
               curr_tile = player.tile_destination;}*)

let rec read_input input to_read = match input with
  |h::t -> if List.mem h to_read then Some h else read_input t to_read
  |[] -> None

let dir_from_keys key = match key with 
  | t when t = Window.w -> Up
  | t when t =  Window.a -> Left
  | t when t =  Window.s -> Down
  | t when t =  Window.d -> Right
  | _ -> failwith "unbound key input"

let change_state_input player input = 
  match (read_input input [Window.w; Window.a; Window.s; Window.d;]) with
  | Some k -> let dir = dir_from_keys k in change_state player (Move dir)
  | None -> begin match (read_input input [Window.e;]) with 
      | Some k -> change_state player (Interact (player.direction,Window.get_time () ))
      | None -> change_state player (Idle) end 


let player_updater (st:state) (player:Player.t) = 
  let player = {player with curr_frame_num = Animations.next_frame player.curr_frame_num player.curr_anim} in
  let player = (*if not player.reach_dest then player else *)
    begin
      change_state_input player st.input
    end in
  match player.state with 
  |Move dir -> player_move player st.current_room
  |Idle -> player
  |Interact (dir, time) -> if (Window.get_time () - time) > 500 then {player with reach_dest =true } else player
  |_ -> print_endline "why is this happening" ; player

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
       st.current_room.enemies |> List.filter (fun (e:Enemy.t) ->
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
      state = Idle
    }
  else None

let item_updater (st:state) (item:Item.t) =
  (*let item = {item with curr_frame_num = Animations.next_frame item.curr_frame_num item.curr_anim} in*)
  let p = st.current_room.player in
  (* Check if the item is in the inventory *)
  match item.pos with
  | Inventory -> item
  | Position (x,y) ->
    (* Check if the player is trying to interact *)
    (match p.state with
     | Interact (dir,_) 
       (* Check if the player is looking at this item *)
       when Vector.greater (0.7,0.5) (((Vector.subtract (Vector.add p.pos (vec_of_dir dir)) (x,y)))|> Vector.abs) -> {item with pos = Inventory}

     (* let (x,y) = p.pos in 
        (match dir, x -. pos.x, y -. pos.y with
        | Up, 0., -1. | Down, 0., 1. | Right, -1., 0. | Left, 1., 0.
         -> *)
     | other -> item)

let room_updater (st:state) room:Room.t = 
  let _ = match read_input st.input [Window.q; Window.esc] with 
    |Some _ -> 
      let numsaves = Sys.readdir "saves" |> Array.length |> string_of_int in
      Save.save room (numsaves);
      exit 0 
    |None -> () in  
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
  let st = { running = if not (List.mem Window.esc input) || not (List.mem Window.q input) then true else false;
             current_room = 
               st.current_room |> room_updater st;
             window = st.window;
             input = input } in
  game_loop st curr_time