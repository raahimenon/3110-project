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

val next_room : t -> t
val update_room: t -> (Player.t -> Player.t) -> t
val draw_room: Window.window -> t -> unit
