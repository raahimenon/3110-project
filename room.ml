open Player
open Graphics
type tile = 
  | Floor of Animations.image 
  | Wall of Animations.image
  | Exit of Animations.image
type t = 
  {
    player: Player.t;
    tiles: tile array array;
  }

let next_room t = t
let update_room t f = {t with player = Player.update t.player f}
let draw_room t = Player.draw t.player
