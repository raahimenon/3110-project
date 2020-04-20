open Player
type tile = 
  | Floor of Animations.image 
  | Wall of Animations.image
  | Exit of Animations.image
type t = 
  {
    player: Player.t;
    tiles: tile array array;
  }

val tile_size : int
val next_room : t -> t
val update_room: t -> (Player.t -> Player.t) -> t
val draw_room: t -> unit
