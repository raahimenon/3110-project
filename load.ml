open Yojson.Basic.Util

let room_from_seed (s : int) win = 
  Room_gen.simple_gen s win

let item_from_seed_id_pos 
    (s : int) 
    (id : int) 
    (x:float) 
    (y:float) 
    (win : Window.window) = 
  Item.make_item s id win x y

let rec room_from_entities (rm : Room.t) (entities : (int * int * int * int) list) : Room.t =
  match entities with
  | [] -> rm
  | h::t -> 
    let id,x,y,health = h in
    let next_room = 
      if id = 0 then {rm with player = {rm.player with e = {rm.player.e with pos = (x |> float_of_int, y|>float_of_int); curr_tile = (x,y); };health = health}}
      else rm in
    room_from_entities next_room t

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

let load s win =
  let json = try Yojson.Basic.from_file ("./saves/"^s) with e -> 
    print_string "File DNE - Are you sure this file exists in the saves folder?\n";
    Window.exit_window win;
    exit 0 in
  let seed = member "seed" json |> to_int in
  let room = room_from_seed seed win in
  let player_json = member "player" json in
  let player = Player.make_player "link" 0 win (member "x" player_json |> to_float) (member "y" player_json |> to_float) in
  let player = 
    {player with
     health = member "health" player_json |> to_int;
     max_health = member "max_health" player_json |> to_int;
     unique_stats = 
       {attack = member "attack" player_json |> to_float;
        movement_speed = member "speed" player_json |> to_int}} in
  let items = member "items" json |> to_list |> List.map (item_from_json win) in
  {room with items = items; player = player}

