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

let collision_with_player rm (player:Player.t) = 
  List.exists 
    (fun x -> match x.pos with
       |Position pos -> Window.collision (scale_pos_pix pos) x.size 
                          (scale_pos_pix player.pos) player.size
       | _ -> false) rm.items

(*|| List.exists (fun (x:Enemy.t)-> x.curr_tile = player.curr_tile) rm.enemies*)