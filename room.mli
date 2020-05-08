open Player
open Enemy
open Item

type tile = 
  | Floor of Animations.image 
  | Wall of Animations.image
  | Exit of Animations.image
  | Boundary

type collision = CItem of Item.t | CEnemy of Enemy.t 
               | CWall of tile | CExit of tile | CPlayer of Player.t
type t = 
  {
    player: Player.t;
    enemies: Enemy.t list;
    items: Item.t list;
    tiles: tile array array;
  }

val next_room : t -> t

val draw_room: Window.window -> t -> unit

val entity_at_tile : t -> int*int -> bool

val get_unused_inventory : t -> int option

val get_item_slot : t -> int -> Item.t option

val collisions_with_entity : t -> Entity.e -> Entity.e -> collision list
