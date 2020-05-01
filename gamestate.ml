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
    input : Window.input list
  }

let rec apply_buffs (player:Player.t) (buffs:Buff.buff_type list) =
  match buffs with
  | [] -> player
  | h::t -> begin 
      match h with
      | Buff.Max_health mh -> {player with max_health = player.max_health + mh}
      | Buff.Health h -> {player with health = player.health + h}
      | _ -> player
    end

(** [change_state p st] is the player [p] updated by the current state [st] *)
let change_state (player:Player.t) pst st = 
  match pst with
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
  |Interact (dir,time) -> 
    {player with curr_anim = (get_anim player player.direction "idle"); 
                 curr_frame_num = 0; state = Interact (dir,time); 
                 reach_dest = false;
                 pos = player.pos |> floor;
                 curr_tile = player.pos |> to_int;
                 tile_destination = player.curr_tile;}
  |Use_Item (dir, time) ->
    let player = 
      {player with curr_anim = (get_anim player player.direction "idle"); 
                   curr_frame_num = 0; state = Use_Item (dir,time); 
                   reach_dest = false;
                   pos = player.pos |> floor;
                   curr_tile = player.pos |> to_int;
                   tile_destination = player.curr_tile;} in
    begin match get_item_slot st.current_room player.inventory_slot with
      | Some i -> begin match i.unique_stats with
          |Buff b -> apply_buffs player b.effect
          | _ -> player
        end
      | None -> player end

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
  if (collision_player player.tile_destination rm)
  then {player with reach_dest =true; tile_destination = player.curr_tile;} 
  else 
    let newpos = player.direction |> vec_of_dir |> scale_vec speed |> add player.pos 
    in 
    let player = {player with pos = newpos; curr_tile = to_int newpos;} in
    if not (check_if_pos_reached player) 
    then player else
      {player with reach_dest = true;
                   pos = (*(fun x -> print_endline ( print x); x)  *)
                     (player.tile_destination |> from_int);
                   curr_tile = player.tile_destination;}

let rec read_input input to_read = match input with
  |h::t -> if List.mem h to_read then Some h else read_input t to_read
  |[] -> None

let rec read_mouse input =
  match List.find_opt (function |Window.MWheel _ -> true |_ -> false) input with
  | Some (MWheel t) -> (-t)
  | _ -> 0

let dir_from_keys key = match key with 
  | t when t = Window.w -> Up
  | t when t =  Window.a -> Left
  | t when t =  Window.s -> Down
  | t when t =  Window.d -> Right
  | Window.MWheel t when t > 0 -> Up
  | Window.MWheel t when t < 0 -> Down
  | _ -> failwith "unbound key input"

let change_state_input player input = 
  match (read_input input [Window.w; Window.a; Window.s; Window.d; Window.rclick; Window.lclick]) with
  | Some r when r = Window.rclick -> change_state player (Interact (player.direction,Window.get_time () ))
  | Some l when l = Window.lclick -> change_state player (Use_Item (player.direction,Window.get_time () ))
  | Some k -> let dir = dir_from_keys k in change_state player (Move dir)
  | None -> change_state player Idle


let player_updater (st:state) (player:Player.t) = 
  let player = 
    {player with 
     health = player.health - 1;
     curr_frame_num = Animations.next_frame player.curr_frame_num player.curr_anim;
     inventory_slot = let next_slot = player.inventory_slot + read_mouse st.input in
       if next_slot >= 0 && next_slot < GameVars.inventory_size then next_slot else player.inventory_slot
    } in
  let player = if not player.reach_dest then player else 
      begin
        change_state_input player st.input st
      end in
  match player.state with 
  |Move dir -> player_move player st.current_room
  |Idle -> player
  |Interact (dir, time) -> if (Window.get_time () - time) > 500 then {player with reach_dest =true } else player
  |Use_Item (dir, time) -> if (Window.get_time () - time) > 500 then {player with reach_dest =true } else player
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

let item_updater (st:state) (item:Item.t) : Item.t option =
  let p = st.current_room.player in
  (* Check if the item is in the inventory *)
  match item.pos with
  | Inventory {index}-> begin match p.state with
      | Use_Item (dir, _) -> 
        if item.in_use then Some item else
        if p.inventory_slot = index then
          begin match item.unique_stats with 
            | Buff {max_durability; durability; effect} -> 
              print_endline (string_of_int durability);
              if durability - 1 > 0 then
                Some {item with unique_stats = Buff{max_durability; durability = durability - 1; effect}; in_use = true}
              else (print_endline "BROKEN"; None) 
            | _ -> Some item end
        else Some item 
      | other -> Some {item with in_use = false} end
  | Position {x;y} -> let item = {item with in_use = false} in
    (* Check if the player is trying to interact *)
    (match p.state with
     | Interact (dir,_) 
       (* Check if the player is looking at this item *)
       when (Vector.vec_of_dir dir) = subtract (x,y) p.pos -> 
       Some {item with pos = begin match get_unused_inventory st.current_room with Some i -> Inventory {index = i} |None -> item.pos end }
     (*) let (x,y) = p.pos in 
       (match dir, x -. pos.x, y -. pos.y with
       | Up, 0., -1. | Down, 0., 1. | Right, -1., 0. | Left, 1., 0.
         -> *)
     | other -> Some item)

let room_updater (st:state) room : Room.t = 
  let _ = match read_input st.input [Window.q; Window.esc] with 
    | Some esc when esc = Window.esc -> 
      let numsaves = Sys.readdir "saves" |> Array.length |> string_of_int in
      Save.save room (numsaves);
      exit 0 
    | Some _ ->
      Window.exit_window (st.window);
      exit 0
    | None -> () in  
  {room with player = room.player |> player_updater st;
             enemies = room.enemies |> List.filter_map (enemy_updater st);
             items = room.items |> List.map (item_updater st) |> List.filter (fun i -> i <> None) |> List.map (Option.get)}

let draw_hud (st : state) = 
  let player = st.current_room.player in
  let ratio = float_of_int player.health /. (float_of_int player.max_health) in
  Window.draw_hud_box st.window st.current_room.player.inventory_slot;
  Window.draw_rect_col st.window 
    (255. *. (1. -. ratio) |> int_of_float, 255. *. ratio |> int_of_float, 0)
    (float_of_int GameVars.width -. 1. -. GameVars.hud_bezel_tile,1.)
    (1., (float_of_int GameVars.height -. 2.)*.ratio);
  List.map (fun (i : Item.t) -> match i.pos with Inventory _ -> Item.draw st.window st.current_room.player.pos i |_ -> ()) st.current_room.items

let rec game_loop st time = 
  let curr_time = Window.get_time () in
  let delta = curr_time - time in 
  let delay = spf_in_milli - delta in
  begin if (delay) > 0 then Window.wait (delay) else print_endline ("lag" ^ string_of_int delay)
  end;
  let curr_time = Window.get_time() in
  Window.clear st.window;
  Room.draw_room st.window st.current_room; 
  ignore(draw_hud st);
  Window.render st.window;
  let input = Window.input_query st.input in
  let st = { running = if not (List.mem Window.esc input) || not (List.mem Window.q input) then true else false;
             current_room = 
               st.current_room |> room_updater st;
             window = st.window;
             input = input } in
  game_loop st curr_time