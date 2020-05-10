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
    icons : (string * Animations.image) list;
    last_anim_frame : int;
  }

let player_attack st = 
  let player = st.current_room.player in
  player.unique_stats.attack *. begin
    match  
      Room.get_item_slot st.current_room st.current_room.player.inventory_slot
    with
    | Some i -> begin 
        match i.unique_stats with 
        | Combat {attack} -> attack
        | _ -> 1.
      end
    | _ -> 1. end
  |> int_of_float

let player_speed st = 
  let player = st.current_room.player in
  player.unique_stats.movement_speed + 
  match Room.get_item_slot st.current_room st.current_room.player.inventory_slot
  with
  | Some i -> begin 
      match i.unique_stats with 
      | Combat {movement_speed} -> movement_speed
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
      | Buff.Max_health mh -> 
        apply_buffs {player with max_health = player.max_health + mh} t
      | Buff.Health h -> let h = player.health + h in 
        apply_buffs {player with 
                     health = 
                       if h > player.max_health then player.max_health else h} t
      | Buff.Attack a -> 
        apply_buffs {player with 
                     unique_stats = 
                       {player.unique_stats with 
                        attack = player.unique_stats.attack +. (float_of_int a)}} t
      | Buff.Movement_speed s ->
        apply_buffs {player with 
                     unique_stats = 
                       {player.unique_stats with 
                        movement_speed = 
                          player.unique_stats.movement_speed + s}} t
    end

(** [change_state p st] is the player [p] updated by the current state [st] *)
let change_state (player:Player.t) pst st = 
  match pst with
  | Move dir -> 
    {player with 
     state = Move dir; 
     e = {player.e with direction = dir; 
                        curr_anim = (get_anim player.e dir "walk");
                        curr_frame_num = 0};
     paused = false;}
  | Idle -> 
    {player with 
     e = {player.e with 
          curr_anim = (get_anim player.e player.e.direction "idle"); 
          curr_frame_num = 0; };
     paused = false;
     state = Idle; }
  | Interact (dir, time) -> 
    {player with 
     e = {player.e with 
          curr_anim = (get_anim player.e player.e.direction "pickup"); 
          curr_frame_num = 0; };
     paused = true;
     state = Interact (dir,time);}
  | Knock (dir, time)->
    {player with 
     e = {player.e with 
          curr_anim = (get_anim player.e player.e.direction "idle"); 
          curr_frame_num = 0;};
     paused = true;
     state = Knock (dir,time);}
  | Use_Item (dir, time) ->
    let player = 
      {player with 
       e ={player.e with 
           curr_anim = (get_anim player.e player.e.direction "item"); 
           curr_frame_num = 0;}; 
       state = Use_Item (dir,time); 
       paused = true;} in
    begin match get_item_slot st.current_room player.inventory_slot with
      | Some i -> begin match i.unique_stats with
          |Buff b -> apply_buffs player b.effect
          | _ -> player
        end
      | None -> player end
  | Attack (dir, time, anim) ->
    {player with 
     e =
       {player.e with 
        curr_anim = (get_anim player.e player.e.direction "attack"); 
        curr_frame_num = 0; pos = player.e.pos;
        curr_tile = player.e.pos |> to_int;}; 
     state = Attack (dir,time, anim); 
     paused = true;}
  | Drop (dir, time) -> 
    {player with
     e = 
       {player.e with 
        curr_anim = (get_anim player.e player.e.direction "item");
        curr_frame_num = 0; pos = player.e.pos;
        curr_tile = player.e.pos |> to_int;};
     state = Drop (dir, time);
     paused = true;}


let entity_move e speed = 
  let new_pos = e.direction |> vec_of_dir |> scale_vec speed |> add e.pos 
  in 
  {e with pos = new_pos; curr_tile = to_int new_pos;} 

let try_movement e rm speed = 
  let new_e =  entity_move e speed in 
  let collisions = Room.collisions_with_entity rm new_e e in 
  let final_e = if (collisions) <> [] then e else new_e in 
  final_e,collisions

let move_vector e velocity  = 
  let new_pos = velocity |> add e.pos 
  in 
  {e with pos = new_pos; curr_tile = to_int new_pos;} 

let try_movement_vector e rm velocity = 
  let new_e =  move_vector e velocity in 
  let collisions = Room.collisions_with_entity rm new_e e in 
  let final_e = if (collisions) <> [] then e else new_e in 
  final_e, collisions


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
            Window.lclick;
            Window.e]) with
  | Some r when r = Window.rclick -> 
    begin match player.state with 
      | Interact _ -> player 
      | _ -> change_state player 
               (Interact (player.e.direction, Window.get_time () )) st end
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
  | Some e when e = Window.e -> 
    change_state player (Drop (player.e.direction, Window.get_time ())) st
  | Some k -> let dir = dir_from_keys k in begin match player.state with
      | Move d when d = dir -> player
      | _ -> change_state player (Move dir) st end
  | None -> change_state player Idle st

let reverse_diection = function 
  | Left -> Right
  | Up -> Down
  | Right -> Left
  | Down -> Up

let update_inventory curr_slot st = 
  let next_slot = curr_slot + read_mouse st.input in
  if next_slot >= 0 && next_slot < GameVars.inventory_size 
  then next_slot else curr_slot

let anim_over (anim: Animations.animation) start speed = 
  (snd anim |> Array.length) * int_of_float (float_of_int GameVars.anim_spf_in_milli *. 10./.(float_of_int speed)) + start - (Window.get_time ()) <= 0

let calculate_attack enemy_list = 
  List.fold_left 
    (fun t (e:Enemy.t) -> t + (int_of_float e.unique_stats.attack)) 0 enemy_list

let taking_damage player = 
  Window.get_time () - player.last_damage > 500

let take_damage st player = 
  if player.attacking_enemies <> [] && 
     (taking_damage player) then
    let new_player = 
      {player with 
       health = player.health - calculate_attack player.attacking_enemies;
       last_damage = Window.get_time ();} in 
    change_state new_player 
      (Knock (reverse_diection (dir_of_vec (subtract (List.hd player.attacking_enemies).e.pos player.e.pos)), Window.get_time ())) st 
  else player 

let player_updater (st:state) (player:Player.t) = 
  let player = {player with e = update_animation player.e st.last_anim_frame;} in
  let player = player |> take_damage st in 
  let player = if player.paused then player else
      begin
        change_state_input player st.input st
      end in
  match player.state with 
  |Move dir -> 
    let new_e,collisions =  try_movement player.e st.current_room 
        (float_of_int (player_speed st)*.GameVars.speed) in 
    let enemies_hit = 
      List.filter_map (fun x -> match x with CEnemy e -> Some e | _ -> None) collisions 
    in
    {player with e = new_e;
                 inventory_slot = update_inventory player.inventory_slot st;
                 attacking_enemies =  enemies_hit @ player.attacking_enemies;},[]
  | Idle -> {player with 
             inventory_slot = update_inventory player.inventory_slot st;}, []
  | Interact (dir, time) -> if anim_over player.e.curr_anim time (player_speed st) then 
      {player with paused = false;
                   inventory_slot = 
                     update_inventory player.inventory_slot st}, []
    else player, []
  | Attack (dir, time, anim) ->
    let new_e =  entity_move player.e  0.5 in 
    let collisions = Room.collisions_with_entity st.current_room new_e player.e in
    let enemies_hit = 
      List.map (fun e -> match e with CEnemy e -> e.id | _ -> -1) collisions
      |> List.filter (fun id -> id >= 0 && (List.mem id player.enemy_buffer |> not)) 
    in
    let player = {player with enemy_buffer = player.enemy_buffer @ enemies_hit} in
    (if anim_over player.e.curr_anim time (player_speed st)
     then {player with paused = false; enemy_buffer = [] } else player), enemies_hit
  | Use_Item (dir, time)
  | Drop (dir, time) ->
    (if anim_over player.e.curr_anim time (player_speed st)
     then {player with paused = false } else player), []
  | Knock (dir, time) ->     
    let new_e,collisions =  try_movement_vector player.e st.current_room 
        (scale_vec 0.2 (vec_of_dir dir)) in 
    if (Window.get_time () - time > 100) then 
      (change_state player Idle st),[] else
      {player with e = new_e;},[]

(** [enemy_updater st enemy] returns a new [Enemy.t] which represents the
    changes to [enemy] imposed by [st].  *)
let enemy_updater (st:state) (cols: entity_id list) (enemy:Enemy.t)  =
  let enemy = {enemy with 
               e = update_animation enemy.e st.last_anim_frame;
               health = enemy.health - 
                        if List.mem enemy.id cols then player_attack st else 0}
  in
  let p = st.current_room.player in 
  let vec_to_player = Vector.subtract p.e.pos enemy.e.pos in 
  let enemy = 
    begin
      let dir = (Vector.dir_of_vec vec_to_player) in 
      if (distance p.e.pos enemy.e.pos > 6. || 
          (enemy.e.direction <> dir && distance p.e.pos enemy.e.pos > 3.)) then 
        {enemy with 
         state = EIdle;
         e = {enemy.e with
              curr_anim = (Entity.get_anim enemy.e enemy.e.direction "idle");
              curr_frame_num = 0;}; 
        } 
      else 
        match enemy.state with 
        | EMove d when d = dir -> enemy
        |_ ->
          {enemy with 
           state = EMove dir; 
           e = {enemy.e with 
                direction = dir; 
                curr_anim = (Entity.get_anim enemy.e dir "walk");
                curr_frame_num = 0;}; 

          } 
    end 
  in 
  match enemy.state with
  | EIdle -> enemy,false
  | EMove dir ->
    let new_e,collisions =   
      try_movement_vector enemy.e st.current_room 
        (Vector.scale_vec ((1./.Vector.magnitude vec_to_player) *. speed/.4. *. 
                           float_of_int enemy.unique_stats.movement_speed) 
           vec_to_player)
    in 
    {enemy with 
     e = new_e},(List.mem (CPlayer st.current_room.player) collisions )
  |_ -> failwith "unexpected enemy state"

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
      | Drop (dir, _) ->
        if p.inventory_slot = index then 
          let new_e =  
            entity_move 
              {item.e with direction = p.e.direction; pos = p.e.pos} 1. in 
          let collisions = 
            Room.collisions_with_entity st.current_room new_e item.e in 
          if List.length collisions > 0 then Some item else
            Some {item with e = new_e; pos = Position new_e.pos}
        else Some item
      | other -> Some {item with in_use = false} end
  | Position (x,y) -> let item = {item with in_use = false} in
    (* Check if the player is trying to interact *)
    (match p.state with
     | Interact (dir,_) 
       (* Check if the player is looking at this item *)
       when Vector.greater (0.7,0.7) (((Vector.subtract (Vector.add p.e.pos (vec_of_dir dir)) (x,y)))|> Vector.abs_vec) -> 
       Some {item with pos = begin match get_unused_inventory st.current_room with Some i -> Inventory {index = i} |None -> item.pos end }
     | other -> Some item)

let room_updater st room = 
  let _ = match read_input st.input [Window.q; Window.esc] with 
    | Some esc when esc = Window.esc -> 
      let numsaves = Sys.readdir "saves" |> Array.length |> string_of_int in
      Save.save room (numsaves);
      exit 0 
    | Some _ ->
      Window.exit_window (st.window);
      exit 0
    | None -> () in  
  let player, cols = room.player |> player_updater st  in
  let room = {room with player = player;} in 
  let enemy_list = room.enemies |>  List.map (enemy_updater st cols) in
  let new_enemies = fst (List.split enemy_list) |> List.filter (fun (e: Enemy.t) -> e.health > 0) in 
  let attacking_enemies = enemy_list |> List.filter_map (fun (e,b) -> if b then Some e else None) in 
  {room with 
   player = {room.player with attacking_enemies = attacking_enemies;};
   enemies = new_enemies;
   items = room.items |> List.filter_map (item_updater st) }

let icons_of_int ?style:(style = 0) num st : Animations.image list = 
  let num = string_of_int num in
  let rec icons accu = function
    | str when str = "" -> accu
    | s -> icons ((Animations.get_icon 
                     (String.sub s 0 1 ^ 
                      if style > 0 then "p" else if style < 0 then "m" else "") 
                     st.icons)::accu)
             (String.sub s 1 (String.length s - 1)) in
  icons [] num |> List.rev

let icons_of_float ?style:(style = 0) num st : Animations.image list = 
  if Float.floor num = num then icons_of_int ~style:style (int_of_float num) st 
  else
    let num = string_of_float num in
    let rec icons accu = function
      | str when str = "" -> accu
      | s -> 
        let fname = String.sub s 0 1 in
        icons ((Animations.get_icon 
                  ((if fname = "." then "dot" else fname )^ 
                   if style > 0 then "p" else if style < 0 then "m" else "") 
                  st.icons)::accu) 
          (String.sub s 1 (String.length s - 1)) in
    icons [] num |> List.rev

let rec get_buffs items (accu) : float * int * int * int * int * int =
  let catk, cspd, bh, bmh, batk, bspd = accu in
  match items with
  | [] -> accu
  | h::t -> begin match h with 
      | Buff.Health h -> get_buffs t (catk, cspd, h, bmh, batk, bspd)
      | Buff.Max_health mh -> get_buffs t (catk, cspd, bh, mh, batk, bspd)
      | Buff.Attack atk -> get_buffs t (catk, cspd, bh, bmh, atk, bspd)
      | Buff.Movement_speed spd -> get_buffs t (catk, cspd, bh, bmh, batk, spd)
    end

let draw_hud (st : state) = 
  let player = st.current_room.player in
  let ratio = float_of_int player.health /. (float_of_int player.max_health) in
  let item = Room.get_item_slot st.current_room player.inventory_slot in
  let combat_attack, combat_speed, buff_health, buff_mh, buff_atk, buff_spd
    = match item with 
    | Some i -> begin match i.unique_stats with 
        |Combat {attack; movement_speed} -> attack, movement_speed, 0, 0, 0, 0
        |Buff b -> get_buffs b.effect (0.,0,0,0,0,0) end
    | None -> 0., 0, 0, 0, 0, 0 in
  (* Initialize HUD box drawing *)
  Window.draw_hud_box st.window st.current_room.player.inventory_slot;
  (* Draw HUD health bar *)
  if(buff_health > 0) then begin
    let buff_health_display = icons_of_int ~style:1 (buff_health) st in
    Window.draw_image st.window (Animations.get_icon "+" st.icons)
      (float_of_int GameVars.width -. 2.) 0.;
    List.iteri 
      (fun idx ic -> 
         Window.draw_image st.window ic 
           (float_of_int GameVars.width -. 2. +. 
            (5./.GameVars.tile_size)+. 
            (float_of_int (idx + 3 - List.length buff_health_display)) *. 
            (9./.GameVars.tile_size))
           (0.))
      buff_health_display end
  else ();
  let health_display = (icons_of_int (player.health) st) in
  List.iteri 
    (fun idx ic -> 
       Window.draw_image st.window ic 
         (float_of_int GameVars.width -. 2. +. 
          (5./.GameVars.tile_size)+. 
          (float_of_int (idx + 3 - List.length health_display)) *. 
          (9./.GameVars.tile_size))
         (1.))
    health_display;
  Window.draw_rect_col st.window 
    (Window.health_col_ratio ratio)
    (float_of_int GameVars.width -. 1. -. 
     GameVars.hud_bezel_tile,2.+.5./.GameVars.tile_size)
    (1., GameVars.vrad*.ratio);
  Window.draw_image_raw st.window
    (Animations.get_icon "hp" st.icons)
    (float_of_int GameVars.width -. 1. -. GameVars.hud_bezel_tile)
    (2.);
  List.iteri 
    (fun idx ic -> 
       Window.draw_image st.window ic 
         (float_of_int GameVars.width -. 2. +. 
          (2./.GameVars.tile_size)+. 
          (float_of_int idx) *. 
          (9./.GameVars.tile_size))
         (2. +. 10./.GameVars.tile_size +. GameVars.vrad))
    (icons_of_int (player.max_health) st);
  if(buff_mh > 0) then begin
    let buff_mh_display = icons_of_int ~style:1 (buff_mh) st in
    Window.draw_image st.window (Animations.get_icon "+" st.icons)
      (float_of_int GameVars.width -. 2.)
      (3. +. 10./.GameVars.tile_size +. GameVars.vrad);
    List.iteri 
      (fun idx ic -> 
         Window.draw_image st.window ic 
           (float_of_int GameVars.width -. 2. +. 
            (5./.GameVars.tile_size)+. 
            (float_of_int (idx + 3 - List.length buff_mh_display)) *. 
            (9./.GameVars.tile_size))
           (3. +. 10./.GameVars.tile_size +. GameVars.vrad))
      buff_mh_display end
  else ();
  (* Draw player stats *)
  Window.draw_image st.window
    (Animations.get_icon "attack" st.icons)
    (float_of_int GameVars.width -. 2.)
    (float_of_int GameVars.height -. 4.);
  let atk_display = icons_of_int 
      ~style:
        (if combat_attack > 0. then 1 
         else if combat_attack < 0. then -1 
         else 0)
      (player_attack st) st in
  List.iteri 
    (fun idx ic -> 
       Window.draw_image st.window ic 
         (float_of_int GameVars.width -. 2. +. 
          (5./.GameVars.tile_size)+. 
          (float_of_int (idx + 3 - List.length atk_display)) *. 
          (9./.GameVars.tile_size))
         (float_of_int GameVars.height -. 4.))
    atk_display;
  if(buff_atk > 0) then begin
    let buff_atk_display = icons_of_int ~style:1 (buff_atk) st in
    Window.draw_image st.window (Animations.get_icon "+" st.icons)
      (float_of_int GameVars.width -. 2.)
      (float_of_int GameVars.height -. 1.);
    List.iteri 
      (fun idx ic -> 
         Window.draw_image st.window ic 
           (float_of_int GameVars.width -. 2. +. 
            (5./.GameVars.tile_size)+. 
            (float_of_int (idx + 3 - List.length buff_atk_display)) *. 
            (9./.GameVars.tile_size))
           (float_of_int GameVars.height -. 3.))
      buff_atk_display end;
  Window.draw_image st.window
    (Animations.get_icon "speed" st.icons)
    (float_of_int GameVars.width -. 2.)
    (float_of_int GameVars.height -. 2.);
  let spd_display = icons_of_int ~style:
      (if combat_speed > 0 then 1 
       else if combat_speed < 0 then -1 
       else 0)
      (player_speed st) st in
  List.iteri 
    (fun idx ic -> 
       Window.draw_image st.window ic 
         (float_of_int GameVars.width -. 2. +. 
          (5./.GameVars.tile_size)+. 
          (float_of_int (idx + 3 - List.length spd_display)) *. 
          (9./.GameVars.tile_size))
         (float_of_int GameVars.height -. 2.))
    spd_display;
  if(buff_spd > 0) then begin
    let buff_spd_display = icons_of_int ~style:1 (buff_spd) st in
    Window.draw_image st.window (Animations.get_icon "+" st.icons)
      (float_of_int GameVars.width -. 2.)
      (float_of_int GameVars.height -. 1.);
    List.iteri 
      (fun idx ic -> 
         Window.draw_image st.window ic 
           (float_of_int GameVars.width -. 2. +. 
            (5./.GameVars.tile_size)+. 
            (float_of_int (idx + 3 - List.length buff_spd_display)) *. 
            (9./.GameVars.tile_size))
           (float_of_int GameVars.height -. 1.))
      buff_spd_display end;
  (* Draw inventory *)
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

  begin if (delay) > 0 then Window.wait (delay) 
    else print_endline ("lag" ^ string_of_int delay)
  end;

  let curr_time = Window.get_time () in

  Window.clear st.window;
  Room.draw_room st.window st.current_room; 
  ignore (draw_hud st);
  Window.render st.window;

  let input = Window.input_query st.input in
  let st = { running = not (List.mem Window.esc input) || 
                       not (List.mem Window.q input);
             current_room = 
               st.current_room |> room_updater st;
             input = input;
             last_anim_frame = begin let time = Window.get_time () in  
               if time - st.last_anim_frame > GameVars.anim_spf_in_milli 
               then time else st.last_anim_frame end;
             window = st.window;
             icons = st.icons;
           } in
  game_loop st curr_time