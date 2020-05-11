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

(** [player_attack st] is the attack of [st.player] in state [st].*)
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

(** [player_speed st] is the speed of [st.player] in state [st].*)
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


(** [update_animation e lf] is [e] with the next animation frame if  
    [GameVars.amin_spf_in_milli] milliseconds have passed since [lf], which
    represents the last timestep that would update the frame.*)
let update_animation e lf = 
  if Window.get_time () - lf >= GameVars.anim_spf_in_milli then 
    {e with curr_frame_num = Animations.next_frame e.curr_frame_num e.curr_anim}
  else e

(** [change_animation e anim_name] is [e] with the current animation changed to 
    the animation given by [anim_name].*)
let change_animation e anim_name = 
  {e with 
   curr_anim = (get_anim e e.direction anim_name); 
   curr_frame_num = 0;}

(** [apply_buffs player buffs] is the new player generated when the 
    each buff in [buffs] is applied to [player].*)
let rec apply_buffs (buffs:Buff.buff_type list) (player:Player.t)  =
  match buffs with
  | [] -> player
  | h :: t -> 
    begin 
      match h with
      | Buff.Max_health mh -> 
        {player with max_health = player.max_health + mh} 
      | Buff.Health h -> let h = player.health + h in
        {player with 
         health = if h > player.max_health then player.max_health else h} 
      | Buff.Attack a -> 
        {player with unique_stats = {player.unique_stats with 
                                     attack = player.unique_stats.attack +.
                                              (float_of_int a)}} 
      | Buff.Movement_speed s ->
        {player with unique_stats = {player.unique_stats with 
                                     movement_speed = 
                                       player.unique_stats.movement_speed + s}} 
    end
    |> apply_buffs t


(** [use_item player st] is the player [player] after using the item
    present in [player.inventory_slot], where the current game state is [st] *) 
let use_item player st = 
  begin match get_item_slot st.current_room player.inventory_slot with
    | Some i -> begin match i.unique_stats with
        |Buff b -> apply_buffs b.effect player
        | _ -> player end
    | None -> player end

(** [change_state p pst st] is the player [p] updated with the new player state 
    [pst] where the current game state is [st] *) 
let change_state (player:Player.t) pst st = 
  let player = {player with state = pst} in 
  match pst with
  | Move dir -> 
    {player with e = change_animation ({player.e with direction = dir;}) "walk";
                 paused = false;}
  | Idle -> 
    {player with e = change_animation player.e "idle"; paused = false;}
  | Interact (dir, time) -> 
    {player with e = change_animation player.e "pickup"; paused = true;}
  | Knock (dir, time)->
    {player with e = change_animation player.e "idle"; paused = true;}
  | Use_Item (dir, time) ->
    let player = 
      {player with e = change_animation player.e "item"; paused = true;} in
    use_item player st
  | Attack (dir, time, anim) ->
    {player with e = change_animation player.e "attack"; paused = true;}
  | Drop (dir, time) -> 
    {player with e = change_animation player.e "item"; paused = true;}

(**[entity_move e speed] returns entity [e] with position updated to move
   in the direction of [e.direction] with magnitude [speed].*)
let entity_move e speed = 
  let new_pos = e.direction |> vec_of_dir |> scale_vec speed |> add e.pos 
  in 
  {e with pos = new_pos; curr_tile = to_int new_pos;} 

(**[try_movement e rm speed] returns [e' , collisions] 
   where [e'] is [entity_move e speed] if [entity_move e speed] 
   does not collide with any entity apart from [e] in room [rm] and is [e] 
   otherwise, and [collisions] is the collisions encountered by 
   [entity_move e speed] in room [rm].*)
let try_movement e rm speed = 
  let new_e =  entity_move e speed in 
  let collisions = Room.collisions_with_entity rm new_e e in 
  let final_e = if (collisions) <> [] then e else new_e in 
  final_e,collisions

(**[move_vector e velocity] returns entity [e] with position updated to be 
   added component-wise to [velocity].*)
let move_vector e velocity  = 
  let new_pos = velocity |> add e.pos 
  in 
  {e with pos = new_pos; curr_tile = to_int new_pos;} 

(**[try_movement e rm speed] returns [e' * collisions] 
   where [e'] is [move_vector e velocity] if [move_vector e velocity] 
   does not collide with any entity apart from [e] in room [rm] and is [e] 
   otherwise, and [collisions] is the collisions encountered by 
   [move_vector e velocity] in room [rm].*)
let try_movement_vector e rm velocity = 
  let new_e =  move_vector e velocity in 
  let collisions = Room.collisions_with_entity rm new_e e in 
  let final_e = if (collisions) <> [] then e else new_e in 
  final_e, collisions

(** [read_input input to_read] returns None if no element of [input] is a member
    of [to_read] and otherwise [Some a] where [a] is the first element of 
    [to_read] encountered in [read_input] when traversing left to right. *)
let rec read_input input to_read = match input with
  | h :: t -> if List.mem h to_read then Some h else read_input t to_read
  | [] -> None

(** [read_mouse input ] TODO *)
let rec read_mouse input =
  match 
    List.find_opt (function | Window.MWheel _ -> true | _ -> false) input with
  | Some (MWheel t) -> (-t)
  | _ -> 0

(** [dir_from_keys key] is the direction corresponding to the given input [key].
    Requires: [key] is one of [Window.w],[Window.a], [Window.s], or [Window.d].
*)
let dir_from_keys key = match key with 
  | t when t = Window.w -> Up
  | t when t =  Window.a -> Left
  | t when t =  Window.s -> Down
  | t when t =  Window.d -> Right
  | _ -> failwith "unbound key input"

(** [player_update_lclick player st] returns [player] updated with the behavoiur
    when user presses the left mouse button, where the state of the game is 
    [st].*)
let player_update_lclick player st = 
  match Room.get_item_slot st.current_room player.inventory_slot with 
  | None -> change_state player
              (Attack (player.e.direction, Window.get_time (), None)) st
  | Some i when (is_combat_item i) -> 
    change_state player
      (Attack (player.e.direction, Window.get_time (), 
               Some (get_anim i.e player.e.direction "attack"))) st
  | _ -> change_state player
           (Use_Item (player.e.direction,Window.get_time ())) st 

(** [change_state_input player st] returns [player] with [player.state] updated 
    based on the latest user input in [st.input].*)
let change_state_input player st = 
  let inps = [Window.w; Window.a; Window.s; Window.d; Window.rclick; 
              Window.lclick; Window.e] in 
  match (read_input st.input inps) with
  | Some r when r = Window.rclick -> 
    change_state player (Interact (player.e.direction, Window.get_time () )) st 
  | Some l when l = Window.lclick -> 
    player_update_lclick player st
  | Some e when e = Window.e -> 
    change_state player (Drop (player.e.direction, Window.get_time ())) st
  | Some k -> let dir = dir_from_keys k in 
    begin 
      match player.state with
      | Move d when d = dir -> player
      | _ -> change_state player (Move dir) st 
    end
  | None -> change_state player Idle st

(**[reverse_direction dir] returns the direction diametrically opposed to [dir].
*)
let reverse_direction = function 
  | Left -> Right
  | Up -> Down
  | Right -> Left
  | Down -> Up

(**[update_inventory curr_slot st] returns the inventory slot produced by 
   updating slot [curr_clot] and input from [st.input].
   Requires: 0 <= curr_slot < GameVars.inventory_size *)
let update_inventory curr_slot st = 
  let next_slot = curr_slot + read_mouse st.input in
  if next_slot >= 0 && next_slot < GameVars.inventory_size 
  then next_slot else curr_slot

(**[anim_over anim start speed] returns [true] if the animation [anim] has 
   completed more than one cycle of rotation through animation frames
   if it started at time [start] with speed [speed], and [false] otherwise*)
let anim_over (anim: Animations.animation) start speed = 
  0 >= (snd anim |> Array.length) * int_of_float 
         (float_of_int GameVars.anim_spf_in_milli *.
          10./.(float_of_int speed)) + start - (Window.get_time ()) 

(** [calculate_attack enemy_list] is the sum of the attack stats of all enemies
    in [enemy_list] *)
let calculate_attack enemy_list = 
  List.fold_left 
    (fun t (e:Enemy.t) -> t + (int_of_float e.unique_stats.attack)) 0 enemy_list

(** [taking_damage player] returns [true] if [player] is ready to take damage 
    from enemy attacks and [false] otherwise. *)
let taking_damage player = 
  Window.get_time () - player.last_damage > 500

(** [take_damage st player] is [player] updated after taking attacks from all 
    enemies in [player.attacking_enemies], where [st] represents the state of 
    the current game.*)
let take_damage st player = 
  if player.attacking_enemies <> [] && 
     (taking_damage player) then
    let new_player = 
      {player with 
       health = player.health - calculate_attack player.attacking_enemies;
       last_damage = Window.get_time ();} in 
    let first_enemy = List.hd player.attacking_enemies in 
    let vec_to_enemy = subtract first_enemy.e.pos player.e.pos in 
    change_state new_player 
      (Knock (reverse_direction(dir_of_vec vec_to_enemy),Window.get_time ())) st 
  else player 

(** [player_attack_update player st dir time anim] updates [player] after one 
    frame of an attack, where the state of the game is [st], 
    and [player.state] = [Attack (dir, time, anim)].
    Requires: [player.state] = [Attack (dir, time, anim)] *)
let player_attack_update player st dir time anim = 
  let new_e =  entity_move player.e  0.5 in 
  let collisions = Room.collisions_with_entity st.current_room new_e player.e 
  in
  let enemies_hit = 
    List.map (fun e -> match e with CEnemy e -> e.id | _ -> -1) collisions
    |> List.filter (fun id -> id >= 0 && 
                              (List.mem id player.enemy_buffer |> not)) 
  in
  let player = {player with enemy_buffer = player.enemy_buffer @ enemies_hit} 
  in
  (if anim_over player.e.curr_anim time (player_speed st)
   then {player with paused = false; enemy_buffer = [] } else player), 
  enemies_hit

(** [player_move_update player st] updates [player] for one frame of movement,
    where the state of the game is [st].
    Requires: [player.state] matches [Move _ ] *)
let player_move_update player st = 
  let new_e,collisions =  try_movement player.e st.current_room 
      (float_of_int (player_speed st)*.GameVars.speed) in 
  let enemies_hit = 
    List.filter_map (fun x -> match x with CEnemy e -> Some e | _ -> None) 
      collisions 
  in
  {player with e = new_e;
               inventory_slot = update_inventory player.inventory_slot st;
               attacking_enemies =  enemies_hit @ player.attacking_enemies;},[]

(** [player_updater st player] returns [player',attacks] where [player'] 
    represents the changes to [player] imposed by [st] and 
    [player.attacking_enemies], and [attacks] is a list of the ids 
    of all entities attacked by [player].  *)
let player_updater (st:state) (player:Player.t) = 
  let player = {player with e = update_animation player.e st.last_anim_frame;} 
               |> take_damage st in 
  let player = if player.paused then player else change_state_input player st in
  match player.state with 
  | Move dir -> 
    player_move_update player st
  | Idle -> 
    {player with 
     inventory_slot = update_inventory player.inventory_slot st;}, []
  | Interact (dir, time) -> 
    if anim_over player.e.curr_anim time (player_speed st) then 
      {player with paused = false; 
                   inventory_slot = 
                     update_inventory player.inventory_slot st}, []
    else player, []
  | Attack (dir, time, anim) -> 
    player_attack_update player st dir time anim 
  | Use_Item (dir, time)
  | Drop (dir, time) -> 
    (if anim_over player.e.curr_anim time (player_speed st)
     then {player with paused = false } else player), []
  | Knock (dir, time) ->     
    let new_e,collisions = 
      try_movement_vector player.e st.current_room 
        (scale_vec 0.2 (vec_of_dir dir)) in 
    if (Window.get_time () - time > 100) then 
      (change_state player Idle st),[] else
      {player with e = new_e;},[]

(** [take_damage_enemy st attacks dir_player enemy] 
    returns [enemy'] where [enemy'] represents [enemy] after it takes damage 
    depending on whether it is in the list of entity ids attacked by the player 
    [attacks], where [st] is the state of the room and [dir_player] is the 
    closest direction in pointing towards the position of [st.player].*)
let take_damage_enemy st attacks dir_player (enemy: Enemy.t) = 
  if List.mem enemy.id attacks 
  then
    {enemy with 
     health = enemy.health -  player_attack st ;
     state = EKnock 
         (reverse_direction  (dir_player), Window.get_time ());} 
  else enemy 


(** [change_enemy_state enemy player dir_player] returns
    [enemy] with a new state given by the player [player] and the direction from
    the [enemy] to [player] given by [dir_player]. *)
let change_enemy_state (enemy: Enemy.t) (player: Player.t) dir_player = 
  let aggro = not 
      (distance player.e.pos enemy.e.pos > 6. || 
       (enemy.e.direction <> dir_player && 
        distance player.e.pos enemy.e.pos > 3.)) in 
  let enemy = {enemy with aggro_on = aggro} in
  if (enemy.aggro_on) 
  then 
    match enemy.state with 
    | EMove d when d = dir_player -> enemy
    | EKnock _ -> enemy
    | _ ->
      {enemy with 
       state = EMove dir_player; 
       e = change_animation {enemy.e with direction = dir_player;} "walk"} 
  else       
    {enemy with state = EIdle; e = change_animation enemy.e "idle"}


(** [enemy_velocity enemy vec_to_player] returns the velocity of [enemy] given
    that the vector from the position of [enemy] to the player character is 
    [vec_to_player] *)
let enemy_velocity (enemy: Enemy.t) vec_to_player = 
  scale_vec ((1./.magnitude vec_to_player) *. speed/.4. *. 
             float_of_int enemy.unique_stats.movement_speed) vec_to_player

(** [enemy_updater st attacks enemy] returns [enemy', b] where [enemy'] 
    represents the changes to [enemy] imposed by game state [st], and 
    the ids of the entities attacked by player [attacks], and [b] is [true] 
    if [enemy] attacked the player in this loop and [false] otherwise *)
let enemy_updater (st: state) (attacks: entity_id list) (enemy: Enemy.t)  =
  let enemy = {enemy with 
               e = update_animation enemy.e st.last_anim_frame;} in
  let player = st.current_room.player in 
  let vec_to_player = Vector.subtract player.e.pos enemy.e.pos in 
  let dir_player = Vector.dir_of_vec vec_to_player in 
  let enemy = enemy |> take_damage_enemy st attacks dir_player in 
  let enemy = change_enemy_state enemy player dir_player  in 
  match enemy.state with
  | EIdle -> enemy,false
  | EMove dir ->
    let new_e,collisions =  
      try_movement_vector enemy.e st.current_room 
        (enemy_velocity enemy vec_to_player) in 
    {enemy with e = new_e},(List.mem (CPlayer player) collisions)
  | EKnock (dir, time) ->     
    if (Window.get_time () - time > 100) then {enemy with state = EIdle;}, false 
    else
      let new_e,_ =  try_movement_vector enemy.e st.current_room 
          (scale_vec 0.15 (vec_of_dir dir)) in {enemy with e = new_e;}, false
  | _ -> failwith "unexpected enemy state"

(** [item_update st item] returns [None] if [item] is 
    used up, otherwise returns [Some item'] where [item'] is [item]
     updated after is changed by game state [st].  *)
let item_updater (st:state) (item:Item.t) : Item.t option =
  let p = st.current_room.player in
  match item.pos with
  | Inventory {index} -> begin match p.state with
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
    (match p.state with
     | Interact (dir,_) 
       when greater (0.7,0.7) (((subtract (add p.e.pos (vec_of_dir dir)) (x,y))) |> abs_vec) -> 
       Some {item with pos = begin match get_unused_inventory st.current_room with Some i -> Inventory {index = i} |None -> item.pos end }
     | other -> Some item)


(** [room_updater st room] returns [room] updated by one game loop
    where the game state is given by [st].  *)
let room_updater st room = 
  let _ = match read_input st.input [Window.q; Window.esc] with 
    | Some esc when esc = Window.esc -> 
      let numsaves = Sys.readdir "saves" |> Array.length |> string_of_int in
      Save.save room (numsaves); exit 0 
    | Some _ ->
      Window.exit_window (st.window); exit 0
    | None -> () in  
  let player, attacks = room.player |> player_updater st  in
  let room = {room with player = player;} in 
  let enemy_list = room.enemies |>  List.map (enemy_updater st attacks) in
  let new_enemies = fst (List.split enemy_list) 
                    |> List.filter (fun (e: Enemy.t) -> e.health > 0) in 
  let attacking_enemies = enemy_list |> List.filter_map 
                            (fun (e,b) -> if b then Some e else None) in 
  {room with 
   player = {room.player with attacking_enemies = attacking_enemies;};
   enemies = new_enemies;
   items = room.items |> List.filter_map (item_updater st) }

(** [icons_of_int ?style num st] is the list of images representing [num] drawn
    from icons with corresponding filenames in [st], according to [?style].
    If style = 0 then the numbers will be loaded as white. If greater, green,
    and if less, red. *)
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

(** [get_buffs buffs accu] evaluates to (catk,cspd,bh,bmh,batk,bspd), where
    catk is the attack gained from the Combat stat of the item, 
    cspd is the speed gained from the Combat stat of the item,
    bh is the health gained from the Buff stat of the item,
    bmh is the max health gained from the Buff stat of the item,
    batk is the attack gained from the Buff stat of the item,
    bspd is the speed gained from the Buff stat of the item.
    Note that this function should only be called with buff items, so the 
    first two elements of the 6-tuple will always be 1.,0 (attack is 1 as
    it is a multiplier) *)
let rec get_buffs buffs accu : float * int * int * int * int * int =
  let catk, cspd, bh, bmh, batk, bspd = accu in
  match buffs with
  | [] -> accu
  | h::t -> begin match h with 
      | Buff.Health h -> get_buffs t (catk, cspd, h, bmh, batk, bspd)
      | Buff.Max_health mh -> get_buffs t (catk, cspd, bh, mh, batk, bspd)
      | Buff.Attack atk -> get_buffs t (catk, cspd, bh, bmh, atk, bspd)
      | Buff.Movement_speed spd -> get_buffs t (catk, cspd, bh, bmh, batk, spd)
    end

(** [draw_num_hud num win y] takes the image list of numbers [num] and draws
    them to window [win] at y-position [y] at the assumed x-position in
    the HUD*)
let draw_num_hud num win y = 
  List.iteri 
    (fun idx ic -> 
       Window.draw_image win ic 
         (float_of_int width -. 2. +. 
          (5./.tile_size)+. 
          (float_of_int (idx + 3 - List.length num)) *. 
          (9./.tile_size)) y) num

(** [draw_health st player buff_health buff_mh ratio] draws the player's 
    health stats to the HUD, as well as potential item effects*)
let draw_health (st:state) player buff_health buff_mh ratio = 
  let health_display = (icons_of_int (player.health) st) in
  let max_health_display = icons_of_int (player.max_health) st in
  if(buff_health > 0) then begin
    let buff_health_display = icons_of_int ~style:1 (buff_health) st in
    Window.draw_image st.window (Animations.get_icon "+" st.icons)
      (float_of_int width -. 2.) 0.;
    draw_num_hud buff_health_display st.window 0. end;
  draw_num_hud health_display st.window 1.;
  Window.draw_rect_col st.window (Window.health_col_ratio ratio)
    (float_of_int width -. 1. -. 
     hud_bezel_tile,2.+.5./.tile_size)
    (1., vrad*.ratio);
  Window.draw_image_raw st.window (Animations.get_icon "hp" st.icons)
    (float_of_int width -. 1. -. hud_bezel_tile) 2.;
  draw_num_hud max_health_display st.window (2. +. 10./.tile_size +. vrad);
  if(buff_mh > 0) then begin
    let buff_mh_display = icons_of_int ~style:1 (buff_mh) st in
    Window.draw_image st.window (Animations.get_icon "+" st.icons)
      (float_of_int width -. 2.)
      (3. +. 10./.tile_size +. vrad);
    draw_num_hud buff_mh_display st.window (3. +. 10./.tile_size +. vrad) end

(** [draw_attack st combat_attack buff_atk] draws the player's 
    attack stats to the HUD, as well as potential item effects*)
let draw_attack (st : state) combat_attack buff_atk = 
  Window.draw_image st.window
    (Animations.get_icon "attack" st.icons)
    (float_of_int GameVars.width -. 2.)
    (float_of_int GameVars.height -. 4.);
  let atk_display = icons_of_int 
      ~style:
        (if combat_attack > 1. then 1 else if combat_attack < 1. then -1 else 0)
      (player_attack st) st in
  draw_num_hud atk_display st.window (float_of_int GameVars.height -. 4.);
  if(buff_atk > 0) then begin
    let buff_atk_display = icons_of_int ~style:1 (buff_atk) st in
    Window.draw_image st.window (Animations.get_icon "+" st.icons)
      (float_of_int GameVars.width -. 2.) (float_of_int GameVars.height -. 1.);
    draw_num_hud buff_atk_display st.window (float_of_int GameVars.height -. 3.) end;
  Window.draw_image st.window
    (Animations.get_icon "speed" st.icons)
    (float_of_int GameVars.width -. 2.)
    (float_of_int GameVars.height -. 2.)

(** [draw_attack st combat_attack buff_atk] draws the player's 
    speed stats to the HUD, as well as potential item effects*)
let draw_speed (st : state) combat_speed buff_spd = 
  Window.draw_image st.window
    (Animations.get_icon "speed" st.icons)
    (float_of_int GameVars.width -. 2.)
    (float_of_int GameVars.height -. 2.);
  let spd_display = icons_of_int ~style:
      (if combat_speed > 0 then 1 else if combat_speed < 0 then -1 else 0)
      (player_speed st) st in
  draw_num_hud spd_display st.window (float_of_int GameVars.height -. 2.);
  if(buff_spd > 0) then begin
    let buff_spd_display = icons_of_int ~style:1 (buff_spd) st in
    Window.draw_image st.window (Animations.get_icon "+" st.icons)
      (float_of_int GameVars.width -. 2.) (float_of_int GameVars.height -. 1.);
    draw_num_hud buff_spd_display st.window (float_of_int GameVars.height -. 1.) end;
  (* Draw inventory *)
  List.map (fun (i : Item.t) ->
      match i.pos with 
      | Inventory _ -> 
        Item.draw st.window 
          st.current_room.player.e.pos i 
      |_ -> ()) st.current_room.items

(** [draw_hud st] draws the HUD of state [st] to its window*)
let draw_hud (st : state) = 
  let player = st.current_room.player in
  let ratio = float_of_int player.health /. (float_of_int player.max_health) in
  let item = Room.get_item_slot st.current_room player.inventory_slot in
  let combat_attack, combat_speed, buff_health, buff_mh, buff_atk, buff_spd
    = match item with 
    | Some i -> begin match i.unique_stats with 
        |Combat {attack; movement_speed} -> attack, movement_speed, 0, 0, 0, 0
        |Buff b -> get_buffs b.effect (1.,0,0,0,0,0) end
    | None -> 1., 0, 0, 0, 0, 0 in
  (* Draw HUD *)
  Window.draw_hud_box st.window st.current_room.player.inventory_slot;
  draw_health st player buff_health buff_mh ratio;
  draw_attack st combat_attack buff_atk;
  draw_speed st combat_speed buff_spd

(** [gen_room_check st] is the truth value of whether or not a new room should 
    be created. This is true if a player is interacting with a [Room.Exit] and
    false otherwise.*)
let gen_room_check st =
  let player = st.current_room.player in
  match player.state with
  |Interact _ ->
    let new_e,collisions =  try_movement player.e st.current_room 
        (float_of_int (player_speed st)*.GameVars.speed) in 
    let exit = List.filter (fun x -> match x with CExit _ -> true |_ -> false) collisions in
    List.length exit > 0
  | _ -> false

(** [draw_room st] draws the current room and HUD of [st] to its window*)
let draw_room st = 
  Window.clear st.window;
  Room.draw_room st.window st.current_room; 
  ignore (draw_hud st);
  Window.render st.window 

(** [load_room st] is a [Room.t] of the next room created after the 
    current one in [st]*)
let load_room st =
  let () = Random.init (st.current_room.seed) in
  let rm_ref : Room.t option ref = ref None in
  Thread.create (fun seed ->
      rm_ref := Some (Room_gen.simple_gen (Random.int 1073741823) st.window)) 
    () |> ignore;
  let frame = ref 0 in
  let loading = Animations.load_animation "./sprites/loading/" "loading" 
      (Window.get_renderer st.window) in
  let time = Window.get_time () |> ref in
  while (!rm_ref = None) do 
    Window.clear st.window;
    Window.draw_image st.window (Animations.frame loading !frame) (hrad -. 2.) (vrad -. 2.);
    Window.render st.window;
    if Window.get_time () - !time > GameVars.anim_spf_in_milli then begin
      time := Window.get_time ();
      frame := (!frame + 1) mod (snd loading |> Array.length) end;
  done;
  Option.get !rm_ref

(** [create_new_room st] is a [Room.t] of the next room created after the 
    current one in [st] updated with all the elements of [st.current_room]
    that should carry over *)
let create_new_room st =
  let rm = load_room st in
  let pos = rm.player.e.pos in
  {rm with player = 
             {st.current_room.player with 
              paused = false; 
              e = {st.current_room.player.e with 
                   pos = pos; 
                   curr_tile = to_int pos};};
           items = Room.get_inventory st.current_room @ rm.items}

(** [game_loop st time] is the main game loop. It draws the current room 
    to the screen, checks for input, and updates based on the input *)
let rec game_loop st time = 
  let curr_time = Window.get_time () in
  let delta = curr_time - time in 
  let delay = spf_in_milli - delta in
  begin if (delay) > 0 then Window.wait (delay) 
    else print_endline ("lag" ^ string_of_int delay)
  end;
  let curr_time = Window.get_time () in
  draw_room st;
  let next_room = 
    if gen_room_check st 
    then create_new_room st
    else room_updater st st.current_room in

  let input = Window.input_query st.input in
  let st = { st with 
             running = not (List.mem Window.esc input) || 
                       not (List.mem Window.q input);
             current_room = next_room;
             input = input;
             last_anim_frame = begin let time = Window.get_time () in  
               if time - st.last_anim_frame > GameVars.anim_spf_in_milli 
               then time else st.last_anim_frame end;
           } in
  game_loop st curr_time