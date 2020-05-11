open Yojson.Basic.Util

(** [room_from_seed s] is the [Room.t] generated from seed [s] *)
let room_from_seed (s : int) win = 
  Room_gen.simple_gen s win

(** [item_from_seed_id_pos] is the [Item.item_type] generated from seed [s]
    with id [id] at position [(x,y)] *)
let item_from_seed_id_pos 
    (s : int) 
    (id : int) 
    (x:float) 
    (y:float) 
    (win : Window.window) = 
  Item.make_item s id win x y

(** [item_from_json] win json is the item generated by json [json] *)
let item_from_json win json = 
  let seed = member "seed" json |> to_int in
  let id = member "id" json |> to_int in
  let x = member "x" json |> to_float in
  let y = member "y" json |> to_float in
  let item = item_from_seed_id_pos seed id x y win in
  match item.unique_stats with 
  | Buff b -> 
    {item with unique_stats = Buff {b with durability = member "durability" json |> to_int}}
  | _ -> item

(** [enemy_from_json] win json is the enemy generated by json [json] *)
let enemy_from_json win s json =
  let id = member "id" json |> to_int in
  let x = member "x" json |> to_float in
  let y = member "y" json |> to_float in
  let enemy = Enemy.make_enemy s id win x y 0.1 in
  {enemy with health = member "health" json |> to_int}

(** [load s win] is the [Room.t] generated from [s.json] in the saves folder,
    associated with window [win] *)
let load s win =
  let json = try Yojson.Basic.from_file ("./saves/"^s) with e -> 
    print_string "Generating New Room\n";
    failwith "Bad Filename" in
  let seed = member "seed" json |> to_int in
  let room = room_from_seed seed win in
  let player_json = member "player" json in
  let player = Player.make_player "link" 0 win 
      (member "x" player_json |> to_float) 
      (member "y" player_json |> to_float) in
  let player = 
    {player with
     health = member "health" player_json |> to_int;
     max_health = member "max_health" player_json |> to_int;
     unique_stats = 
       {attack = member "attack" player_json |> to_float;
        movement_speed = member "speed" player_json |> to_int}} in
  let items = member "items" json |> to_list |> List.map (item_from_json win) in
  let enemies = 
    member "enemies" json |> to_list |> List.map (enemy_from_json win seed) in
  {room with items = items; player = player; enemies = enemies}

