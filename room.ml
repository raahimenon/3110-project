open Player
open Enemy
open Item
open Vector
type tile = 
  | Floor of Animations.image 
  | Wall of Animations.image
  | Exit of Animations.image
  | Boundary
type t = 
  {
    player: Player.t;
    enemies: Enemy.t list;
    items: Item.t list;
    tiles: tile array array;
  }
type collision = CItem of Item.t | CEnemy of Enemy.t 
               | CWall of tile | CExit of tile | CPlayer of Player.t

let next_room (rm : t) = rm

let draw_tile (win : Window.window) (rm : t) (x : int) (y : int) =
  let position = (x,y) |> Vector.from_int in
  let (x_draw,y_draw) = Vector.center rm.player.e.pos position in
  match rm.tiles.(y).(x) with
  | Floor (im)
  | Wall (im) 
  | Exit (im) -> Window.draw_image win im x_draw y_draw
  | Boundary -> ()

let draw_room (win : Window.window) (rm : t) = 
  Array.iteri (fun y row -> Array.iteri (fun x tile -> draw_tile win rm x y) row) rm.tiles;
  ignore(List.map (fun item -> match item.pos with |Position _ -> Item.draw win rm.player.e.pos item |_ -> ()) rm.items);
  ignore(List.map (fun enemy -> Enemy.draw win rm.player.e.pos enemy)  rm.enemies);
  Player.draw win rm.player.e.pos rm.player

let entity_at_tile rm tile =
  List.exists (fun x -> x.e.curr_tile = tile && match x.pos with |Inventory _ -> false |_ -> true) rm.items
  || List.exists (fun (x:Enemy.t)-> x.e.curr_tile = tile) rm.enemies

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

let check_e_collisions (e1: Entity.e) (e2: Entity.e) = 
  (Window.collision 
     (Vector.add_ints (scale_pos_pix e1.pos) e1.bounding_box_pos) e1.bounding_box 
     (Vector.add_ints (scale_pos_pix e2.pos) e2.bounding_box_pos) e2.bounding_box)

let check_wall_collisions (e:Entity.e) (tile,(y,x)) = 
  (match tile with 
   | Floor _-> false 
   | _-> true ) && 
  Window.collision 
    (16*x + 2,16*y + 2) (12,12) 
    (Vector.add_ints (scale_pos_pix e.pos) e.bounding_box_pos) 
    e.bounding_box

let rec generate_tile_with_cords rm lst = match lst with
  | (y,x)::t -> let ar = try rm.tiles.(y).(x),(y,x) with e -> Boundary, (y,x)
    in ar :: generate_tile_with_cords rm t 
  | [] -> []

let floor f = Float.floor f |> int_of_float

let ciel f = Float.ceil f |> int_of_float

let collisions_with_entity rm (e:Entity.e) (ignore : Entity.e)=
  let players = List.filter 
      (fun (player:Player.t) -> check_e_collisions e player.e && player.e.pos <> ignore.pos) [rm.player] in
  let enemies = List.filter 
      (fun (enemy:Enemy.t) -> check_e_collisions e enemy.e && enemy.e.pos <> ignore.pos) (rm.enemies) in 
  let items =  
    List.filter 
      (fun item -> match item.pos with 
         | Inventory _ -> false
         | _ -> check_e_collisions e item.e && item.e.pos <> ignore.pos) (rm.items) in 
  let (x,y) = e.pos in 
  let tile_array = [(floor y, floor x);
                    (floor y, ciel x);
                    (ciel y, ciel x);
                    (ciel y, floor x)] 
                   |> generate_tile_with_cords rm in 
  let tiles = List.filter (check_wall_collisions e) tile_array in 
  (List.map (fun item -> CItem item) items)@
  (List.map (fun (tile,(a,b)) ->  CWall tile) tiles) @
  (List.map (fun enemy -> CEnemy enemy) enemies) @ 
  (List.map (fun player -> CPlayer player) players)

