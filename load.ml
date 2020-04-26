open Yojson.Basic.Util

let room_from_seed (s : int) = 
  let default_tiles = (Room.Floor (Animations.load_image "./sprites/room/floor.bmp")) |> Array.make_matrix 11 11 in
  Array.fill default_tiles.(0) 0 11 (Room.Wall (Animations.load_image "./sprites/room/wall.bmp"));
  Array.fill default_tiles.(10) 0 11 (Room.Wall (Animations.load_image "./sprites/room/wall.bmp"));
  Room.({
      player = Player.make_player "link" 0; 
      enemies =[];
      items =[];
      tiles = default_tiles
    })

let entity_from_seed_id (s : int) (id : int) = 
  Item.make_item "item"

let rec room_from_entities (rm : Room.t) (entities : (int * int * int * int) list) : Room.t =
  match entities with
  | [] -> rm
  | h::t -> 
    let id,x,y,health = h in
    let next_room = 
      if id = 0 then {rm with player = {rm.player with pos = (x |> float_of_int, y|>float_of_int); curr_tile = (x,y); health = health}}
      else rm in
    room_from_entities next_room t

let load s =
  let json = Yojson.Basic.from_file ("./saves/"^s) in
  let seed = member "seed" json |> to_int in
  let room = room_from_seed seed in
  let entities = 
    member "entities" json 
    |> to_list 
    |> List.map (fun (entity : Yojson.Basic.t) -> 
        member "id" entity |> to_int,
        member "x" entity |> to_int,
        member "y" entity |> to_int,
        member "health" entity |> to_int
      )
  in
  room_from_entities room entities

