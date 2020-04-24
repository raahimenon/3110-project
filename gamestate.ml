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
             (match dir, e.pos.x -. enemy.pos.x, e.pos.y -. enemy.pos.y with
              | Up, 0., -1. | Down, 0., 1. | Right, -1., 0. | Left, 1., 0.
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
         (match dir, p.pos.x -. enemy.pos.x, p.pos.y -. enemy.pos.y with
          | Up, 0., -1. | Down, 0., 1. | Right, -1., 0. | Left, 1., 0.
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
       (match dir, p.pos.x -. pos.x, p.pos.y -. pos.y with
        | Up, 0., -1. | Down, 0., 1. | Right, -1., 0. | Left, 1., 0.
          -> {item with pos = Inventory}
        | other -> item)
     | other -> item)

let room_updater (st:state) room:Room.t = 
  {room with player = room.player |> player_updater st;
             enemies = room.enemies |> List.filter_map (enemy_updater st);
             items = room.items |> List.map (item_updater st)}


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