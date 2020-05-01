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
  Player.draw win rm.player.pos rm.player ;
  let _ = List.map (Item.draw win rm.player.pos) rm.items in ()

let entity_at_tile rm tile = 
  List.exists (fun x -> x.curr_tile = tile && x.pos <> Inventory) rm.items
  || List.exists (fun (x:Enemy.t)-> x.curr_tile = tile) rm.enemies

let scale_pos_pix pos = pos |> Vector.scale_vec 16. |> Vector.to_int


let check_item_collision (player:Player.t) item = match item.pos with
  |Position pos -> Window.collision 
                     (Vector.add_ints (scale_pos_pix pos) item.bounding_box_pos) item.bounding_box 
                     (Vector.add_ints (scale_pos_pix player.pos) player.bounding_box_pos) player.bounding_box
  | _ -> false

let check_wall_collisions (player:Player.t) (tile,(x,y)) = 
  (match tile with 
   | Floor _-> false 
   | _-> true ) && Window.collision 
    (16*x,16*y) (16,16) 
    (Vector.add_ints (scale_pos_pix player.pos) player.bounding_box_pos) 
    player.bounding_box

let rec generate_tile_with_cords rm lst = match lst with
  |(x,y)::t -> let ar = try([rm.tiles.(x).(y),(x,y)]) with e -> [] in ar @ generate_tile_with_cords rm t 
  |[] -> []

let collision_with_player rm (player:Player.t) =
  let items =  List.filter (check_item_collision player) rm.items in 
  let (x,y) = player.curr_tile in 
  let tile_array = [(x,y); (x+1,y); (x,y+1); (x,y-1); (x-1,y)] 
                   |> generate_tile_with_cords rm
  in 
  let tiles = List.filter (check_wall_collisions player) tile_array in 
  if List.length items <> 0 then Some (CItem (List.hd items))
  else begin 
    if List.length tiles <> 0 then let t,(x,y) =  List.hd tiles in Some (CWall t)
    else None end 


(*|| List.exists (fun (x:Enemy.t)-> x.curr_tile = player.curr_tile) rm.enemies*)