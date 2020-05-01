open Player
open Enemy
open Item
open Vector
type tile = 
  | Floor of Animations.image 
  | Wall of Animations.image
  | Exit of Animations.image
type t = 
  {
    player: Player.t;
    enemies: Enemy.t list;
    items: Item.t list;
    tiles: tile array array;
  }
type collision = CItem of Item.t | CEnemy of Enemy.t 
               | CWall of tile | CExit of tile

let next_room (rm : t) = rm
let update_room (rm : t) f = {rm with player = Player.update rm.player f}

let draw_tile (win : Window.window) (rm : t) (x : int) (y : int) =
  let position = (x,y) |> Vector.from_int in
  let (x_draw,y_draw) = Vector.center rm.player.pos position in
  match rm.tiles.(y).(x) with
  | Floor (im)
  | Wall (im) 
  | Exit (im) -> Window.draw_image win im x_draw y_draw

let draw_room (win : Window.window) (rm : t) = 
  Array.iteri (fun y row -> Array.iteri (fun x tile -> draw_tile win rm x y) row) rm.tiles;
  ignore(List.map (fun i -> match i.pos with |Position _ -> Item.draw win rm.player.pos i |_ -> ()) rm.items);
  Player.draw win rm.player.pos rm.player

let entity_at_tile rm tile =
  List.exists (fun x -> x.curr_tile = tile && match x.pos with |Inventory _ -> false |_ -> true) rm.items
  || List.exists (fun (x:Enemy.t)-> x.curr_tile = tile) rm.enemies

let get_unused_inventory (rm : t) : int option =
  let rec create_list length = function
    | t when List.length t < length -> (create_list (length - 1) []) @ [length - 1]
    | t -> t in
  let idxs = create_list GameVars.inventory_size [] in
  let inventory = List.map (fun i -> match i.pos with | Inventory {index = idx} -> idx | _ -> (-1)) rm.items 
                  |> List.filter ((>=) 0) in
  let unused = List.filter (fun i -> List.mem i inventory |> not) idxs in
  let lowest_unused = List.fold_left (fun acc idx -> min acc idx) (GameVars.inventory_size) unused in
  print_endline (string_of_int lowest_unused);
  if lowest_unused < GameVars.inventory_size && lowest_unused >= 0 then Some lowest_unused
  else None

let get_inventory (rm : t) : Item.t list = 
  List.filter (fun i -> match i.pos with |Inventory _ -> true |_ -> false) rm.items
  |> List.sort (fun i1 i2 -> 
      match i1.pos with 
      | Inventory {index = t1 } -> 
        begin
          match i1.pos with
          | Inventory {index = t2} ->
            t1 - t2
          | _ -> failwith "Trying to access non-inventory inventory item"
        end
      | _ -> failwith "Trying to access non-inventory inventory item"
    )

let get_item_slot (rm : t) idx : Item.t option = 
  List.find_opt (fun i -> match i.pos with |Inventory {index} when index = idx -> true |_ -> false) rm.items

let scale_pos_pix pos = pos |> Vector.scale_vec 16. |> Vector.to_int


let check_item_collision (player:Player.t) item = match item.pos with
  |Position pos -> Window.collision 
                     (Vector.add_ints (scale_pos_pix pos) item.bounding_box_pos) item.bounding_box 
                     (Vector.add_ints (scale_pos_pix player.pos) player.bounding_box_pos) player.bounding_box
  | _ -> false

let check_wall_collisions (player:Player.t) (tile,(y,x)) = 
  (match tile with 
   | Floor _-> false 
   | _-> true ) && Window.collision 
    (16*x,16*y) (16,16) 
    (Vector.add_ints (scale_pos_pix player.pos) player.bounding_box_pos) 
    player.bounding_box

let rec generate_tile_with_cords rm lst = match lst with
  |(y,x)::t -> let ar = try([rm.tiles.(y).(x),(y,x)]) with e -> [] in ar @ generate_tile_with_cords rm t 
  |[] -> []

let collision_with_player rm (player:Player.t) =
  let items =  List.filter (check_item_collision player) rm.items in 
  let (x,y) = player.curr_tile in 
  let tile_array = [(y,x); (y,x+1); (y+1,x); (y-1,x); (y,x-1)] 
                   |> generate_tile_with_cords rm
  in 
  let tiles = List.filter (check_wall_collisions player) tile_array in 
  if List.length items <> 0 then Some (CItem (List.hd items))
  else begin 
    if List.length tiles <> 0 then let t,(x,y) =  List.hd tiles in Some (CWall t)
    else None end 


(*|| List.exists (fun (x:Enemy.t)-> x.curr_tile = player.curr_tile) rm.enemies*)
