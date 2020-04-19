open Enemy
open Item 
open Player
type tile = 
  | Floor of Animations.image 
  | Wall of Animations.image
  | Exit of Animations.image

let tile_size = 0
let next_room a = failwith "unimplemented"
let update_room =  failwith "unimplemented"

type t = 
  {
    enemy_list: Enemy.t list;
    item_list: Item.t list;
    player: Player.t;
    tiles: tile array array;
  }
