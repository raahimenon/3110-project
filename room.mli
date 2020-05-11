(** Rooms in the game*)

open Player
open Enemy
open Item

(**The type of a tile in a room. *)
type tile = 
  | Floor of Animations.image 
  | Wall of Animations.image
  | Exit of Animations.image
  | Boundary

(** The type of a collision of one entity with another*)
type collision = CItem of Item.t | CEnemy of Enemy.t 
               | CWall of tile | CExit of tile | CPlayer of Player.t

(**The type representing a room*)
type t = 
  {
    seed: int;
    player: Player.t;
    enemies: Enemy.t list;
    items: Item.t list;
    tiles: tile array array;
  }

(**[draw_room win rm] renders all tiles and entities in room [rm] to window
   [win].*)
val draw_room: Window.window -> t -> unit

(**[get_inventory rm] returns all the items in the player's inventory in room 
   [rm]. *)
val get_inventory : t -> Item.t list

(**[get_unused_inventory rm] returns [None] if the player's inventory in 
   [rm] is full and [Some i] where [i] is the index of the lowest vacant 
   inventory slot otherwise. *)
val get_unused_inventory : t -> int option

(**[get_item_slot rm i] returns [Some item] if [item] is present at the slot 
   [i] in the inventory of the player in [rm] and [None] if there is no item 
   present. *)
val get_item_slot : t -> int -> Item.t option

(**[collisions_with_entity rm e1 e2] returns a list of all collisions of [e1]
   in room [rm] excluding collisions with [e2]. *)
val collisions_with_entity : t -> Entity.e -> Entity.e -> collision list
