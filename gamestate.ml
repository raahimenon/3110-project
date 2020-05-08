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
    input : Window.input list;
    icons : (string*Animations.image) list;
    last_anim_frame : int;
  }

let player_attack st = 
  let player = st.current_room.player in
  player.attack + 
  match Room.get_item_slot st.current_room st.current_room.player.inventory_slot with
  | Some i -> begin 
      match i.unique_stats with 
      | Combat {attack} -> attack
      | _ -> 0
    end
  | _ -> 0

let update_animation e lf = 
  if Window.get_time () - lf >= GameVars.anim_spf_in_milli then 
    {e with curr_frame_num = Animations.next_frame e.curr_frame_num e.curr_anim}
  else e

let rec apply_buffs (player:Player.t) (buffs:Buff.buff_type list) =
  match buffs with
  | [] -> player
  | h::t -> begin 
      match h with
      | Buff.Max_health mh -> {player with max_health = player.max_health + mh}
      | Buff.Health h -> let h = player.health + h in  {player with health = if h > player.max_health then player.max_health else h}
      | Buff.Attack a -> {player with attack = a}
      | Buff.Defense d -> {player with defence = d}
      | _ -> player
    end

(** [change_state p st] is the player [p] updated by the current state [st] *)
let change_state (player:Player.t) pst st = 
  match pst with
  |Move dir -> 
    {player with 
     state = Move dir; 
     e = {player.e with direction = dir; 
                        curr_anim = (get_anim player.e dir "walk");
                        curr_frame_num = 0};
    }
  |Idle -> 
    {player with 
     e = {player.e with 
          curr_anim = (get_anim player.e player.e.direction "idle"); 
          curr_frame_num = 0; pos = player.e.pos;
          curr_tile = player.e.pos |> to_int;};
     state = Idle; }

  |Interact (dir,time) -> 
    {player with 
     e = {player.e with 
          curr_anim = (get_anim player.e player.e.direction "pickup"); 
          curr_frame_num = 0; 
          pos = player.e.pos;};
     paused = true;
     state = Interact (dir,time);}
  |Use_Item (dir, time) ->
    let player = 
      {player with 
       e ={player.e with curr_anim = (get_anim player.e player.e.direction "item"); 
                         curr_frame_num = 0; pos = player.e.pos;
                         curr_tile = player.e.pos |> to_int;}; 
       state = Use_Item (dir,time); 
       paused = true;} in
    begin match get_item_slot st.current_room player.inventory_slot with
      | Some i -> begin match i.unique_stats with
          |Buff b -> apply_buffs player b.effect
          | _ -> player
        end
      | None -> player end
  |Attack (dir, time, anim) ->
    let player = 
      {player with 
       e ={player.e with curr_anim = (get_anim player.e player.e.direction "attack"); 
                         curr_frame_num = 0; pos = player.e.pos;
                         curr_tile = player.e.pos |> to_int;}; 
       state = Attack (dir,time, anim); 
       paused = true;} in
    begin match get_item_slot st.current_room player.inventory_slot with
      | Some i -> begin match i.unique_stats with
          |Buff b -> apply_buffs player b.effect
          | _ -> player
        end
      | None -> player end


let entity_move e rm velocity = 
  let new_pos = e.direction |> vec_of_dir |> scale_vec velocity |> add e.pos 
  in 
  {e with pos = new_pos; curr_tile = to_int new_pos;} 

let rec read_input input to_read = match input with
  | h :: t -> if List.mem h to_read then Some h else read_input t to_read
  | [] -> None

let rec read_mouse input =
  match List.find_opt (function |Window.MWheel _ -> true |_ -> false) input with
  | Some (MWheel t) -> (-t)
  | _ -> 0

let dir_from_keys key = match key with 
  | t when t = Window.w -> Up
  | t when t =  Window.a -> Left
  | t when t =  Window.s -> Down
  | t when t =  Window.d -> Right
  | _ -> failwith "unbound key input"

let change_state_input player input st = 
  match (read_input input 
           [Window.w; 
            Window.a; 
            Window.s; 
            Window.d; 
            Window.rclick; 
            Window.lclick]) with
  | Some r when r = Window.rclick -> 
    begin match player.state with 
      | Interact _ -> player 
      | _ -> change_state player 
               (Interact (player.e.direction,Window.get_time () )) st end
  | Some l when l = Window.lclick -> 
    begin match  player.state,
                 Room.get_item_slot st.current_room player.inventory_slot with 
    | Attack _, _
    | Use_Item _, _ -> player 
    | _, None -> 
      change_state player
        (Attack (player.e.direction, Window.get_time (), None)) st
    | _, Some i when (is_combat_item i) -> 
      change_state player
        (Attack (player.e.direction, 
                 Window.get_time (), 
                 Some (get_anim i.e player.e.direction "attack"))) st
    | _ -> change_state player
             (Use_Item (player.e.direction,Window.get_time ())) st end
  | Some k -> let dir = dir_from_keys k in begin match player.state with
      | Move d when d = dir -> player
      | _ -> change_state player (Move dir) st end
  | None -> change_state player Idle st


let update_inventory curr_slot st = 
  let next_slot = curr_slot + read_mouse st.input in
  if next_slot >= 0 && next_slot < GameVars.inventory_size 
  then next_slot else curr_slot

let anim_over (anim: Animations.animation) start = 
  (snd anim |> Array.length) * GameVars.anim_spf_in_milli + start - (Window.get_time ()) <= 0

let player_updater (st:state) (player:Player.t) = 
  let player = {player with e = update_animation player.e st.last_anim_frame;} in
  let player = {player with health = player.health - if 
                                       player.being_attacked then 1 else 0;
                            being_attacked = false;} in 
  let player = if player.paused then player else
      begin
        change_state_input player st.input st
      end in
  match player.state with 
  |Move dir -> 
    let new_e =  entity_move player.e st.current_room speed in 
    let collisions = Room.collisions_with_entity st.current_room new_e player.e 
    in
    let new_e = 
      if (collisions) <> [] 
      then player.e else new_e in 
    let enemies_hit= 
      List.filter (fun x -> match x with CEnemy e -> true | _ -> false) collisions 
    in
    {player with e = new_e;
                 inventory_slot = update_inventory player.inventory_slot st;
                 being_attacked =  enemies_hit <> []}, []
  | Idle -> {player with 
             inventory_slot = update_inventory player.inventory_slot st;}, []
  | Interact (dir, time) -> if anim_over player.e.curr_anim time then 
      {player with paused = false;
                   inventory_slot = 
                     update_inventory player.inventory_slot st}, []
    else player, []
  | Attack (dir, time, anim) ->
    let new_e =  entity_move player.e st.current_room 0.5 in 
    let collisions = Room.collisions_with_entity st.current_room new_e player.e in
    let enemies_hit= 
      List.map (fun e -> match e with CEnemy e -> e.id | _ -> -1) collisions
      |> List.filter (fun id -> id >= 0 && (List.mem id player.enemy_buffer |> not)) 
    in
    let player = {player with enemy_buffer = player.enemy_buffer @ enemies_hit} in
    (if anim_over player.e.curr_anim time
     then {player with paused = false; enemy_buffer = [] } else player), enemies_hit
  | Use_Item (dir, time) -> 
    (if anim_over player.e.curr_anim time
     then {player with paused = false } else player), []

(** [enemy_updater st enemy] returns a new [Enemy.t] which represents the
    changes to [enemy] imposed by [st]. Raises [Failure] if any adjacent enemy
    or player is found to have [Buff] stats rather than [Combat] stats. *)
let enemy_updater (st:state) (cols: entity_id list) (enemy:Enemy.t) : Enemy.t  =
  let enemy = {enemy with 
               e = update_animation enemy.e st.last_anim_frame;
               health = enemy.health - 
                        if List.mem enemy.id cols then player_attack st else 0} in
  let p = st.current_room.player in 
  let enemy = 
    begin
      if (distance p.e.pos enemy.e.pos > 4.) then {enemy with state = EIdle;} 
      else 
        let dir = (Vector.dir_of_vec (Vector.subtract p.e.pos enemy.e.pos)) in 
        {enemy with 
         state = EMove dir; e = {enemy.e with direction = dir}} 
    end 
  in 
  match enemy.state with
  | EIdle -> enemy
  | EMove dir ->
    let new_e =  entity_move enemy.e st.current_room (speed/.4.) in 
    let collisions =
      (Room.collisions_with_entity st.current_room new_e enemy.e) in 
    let new_e =  if collisions <> [] then enemy.e else new_e in
    {enemy with 
     e = new_e}
  |_ -> failwith "unexpected enemy state"
(*let d = match enemy.unique_stats with
  | Combat stats -> stats.defense
  | other -> failwith "encountered enemy with malformed attributes"
  in let hp_change = 
     (* Identify nearby enemies attacking [enemy] *)
     st.current_room.enemies |> List.filter (fun (enemy:Enemy.t) ->
         match enemy.state with
         | EAttack dir ->
           (match dir, Vector.subtract enemy.e.pos enemy.e.pos  with
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
       (match dir, add p.e.pos (enemy.e.pos|> scale_vec (-1.))  with
        | Up, (0., -1.) | Down, (0., 1.) | Right, (-1., 0.) | Left, (1., 0.)
          -> (match p.unique_stats with
              | Combat stats -> stats.attack
              | other -> failwith "encountered player with malformed attributes")
        | other -> 0)
     | other -> 0
  in*)
(* Get new state *)
(* TODO: ACTUAL LOGIC PARSING 
   let new_state = EIdle in
   (* Apply damage and new state to enemy *)
   if (min enemy.max_health (enemy.health - hp_change) > 0)
   then  {
   enemy with 
   health = min enemy.max_health (enemy.health - hp_change);
   state = new_state
   }
   else {enemy with state = EDead}*)

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
  | Position (x,y) -> let item = {item with in_use = false} in
    (* Check if the player is trying to interact *)
    (match p.state with
     | Interact (dir,_) 
       (* Check if the player is looking at this item *)
       when Vector.greater (0.7,0.7) (((Vector.subtract (Vector.add p.e.pos (vec_of_dir dir)) (x,y)))|> Vector.abs_vec) -> 
       Some {item with pos = begin match get_unused_inventory st.current_room with Some i -> Inventory {index = i} |None -> item.pos end }
     (* let (x,y) = p.pos in 
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
  let player, cols = room.player |> player_updater st in
  {room with player = player;
             enemies = room.enemies |> List.map (enemy_updater st cols) |> List.filter (fun (e: Enemy.t) -> e.health > 0);
             items = room.items |> List.map (item_updater st) |> List.filter (fun i -> i <> None) |> List.map (Option.get)}

let icons_of_int num st : Animations.image list = 
  let num = string_of_int num in
  let rec icons accu = function
    | str when str = "" -> accu
    | s -> icons ((Animations.get_icon (String.sub s 0 1) st.icons)::accu) (String.sub s 1 (String.length s - 1)) in
  icons [] num |> List.rev

let draw_hud (st : state) = 
  let player = st.current_room.player in
  let ratio = float_of_int player.health /. (float_of_int player.max_health) in
  Window.draw_hud_box st.window st.current_room.player.inventory_slot;
  Window.draw_rect_col st.window 
    (Window.health_col_ratio ratio)
    (float_of_int GameVars.width -. 1. -. GameVars.hud_bezel_tile,1.)
    (1., GameVars.vrad*.ratio);
  Window.draw_image_raw st.window
    (Animations.get_icon "hp" st.icons)
    (float_of_int GameVars.width -. 1. -. GameVars.hud_bezel_tile)
    (1. -. 5./.GameVars.tile_size);
  Window.draw_image st.window
    (Animations.get_icon "attack" st.icons)
    (float_of_int GameVars.width -. 2.)
    (2. +. (float_of_int GameVars.height -. 2.)/.2.);
  List.iteri 
    (fun idx ic -> 
       Window.draw_image st.window ic 
         (float_of_int GameVars.width -. 2. +. 
          (5./.GameVars.tile_size)+. 
          (float_of_int idx) *. 
          (9./.GameVars.tile_size))
         (2. +. (float_of_int GameVars.height -. 2.)/.2.))
    (icons_of_int (player_attack st) st);
  List.map (fun (i : Item.t) ->
      match i.pos with 
      | Inventory _ -> 
        Item.draw st.window 
          st.current_room.player.e.pos i 
      |_ -> ()) st.current_room.items

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
             input = input;
             icons = st.icons;
             last_anim_frame = let time = Window.get_time () in if time - st.last_anim_frame > GameVars.anim_spf_in_milli then time else st.last_anim_frame
           } in
  game_loop st curr_time