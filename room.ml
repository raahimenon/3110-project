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

let next_room (rm : t) = rm
let update_room (rm : t) f = {rm with player = Player.update rm.player f}

let draw_tile (win : Window.window) (rm : t) (x : int) (y : int) =
  (*let player_x, player_y = rm.player.pos in
    let xf = float_of_int x in
    let yf = float_of_int y in
    let x_off = xf -. player_x in 
    let y_off = yf -. player_y in
    let x_draw = x_off +. GameVars.hrad in
    let y_draw = y_off +. GameVars.vrad in *)
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