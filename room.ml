open Player
open Enemy
open Item
open Graphics
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

let next_room t = t
let update_room t f = {t with player = Player.update t.player f}
let draw_room t = Player.draw t.player
