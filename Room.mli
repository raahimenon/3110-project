type tile = 
  | Floor of Animations.image 
  | Wall of Animations.image
  | Exit of Animations.image

val tile_size : int
val next_room : t -> t
val update_room: t -> t

type t = 
  {
    enemy_list: Enemy.t list;
    item_list: Item.t list;
    player: Player.t;
    tiles: tile array array;
  }
