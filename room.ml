open Player
open Enemy
open Item

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
  let player_x, player_y = rm.player.pos in
  let xf = float_of_int x in
  let yf = float_of_int y in
  let x_off = xf -. player_x in 
  let y_off = yf -. player_y in
  let x_draw = x_off +. GameVars.hrad in
  let y_draw = y_off +. GameVars.vrad in 
  match rm.tiles.(y).(x) with
  | Floor (im)
  | Wall (im)
  | Exit (im) -> Window.draw_image win im x_draw y_draw

let draw_room (win : Window.window) (rm : t) = 
  Array.iteri (fun y row -> Array.iteri (fun x tile -> draw_tile win rm x y) row) rm.tiles;
  Player.draw win rm.player