(** Items in the game*)

open Entity

(**The type representing the position of an item.*)
type item_pos =  Inventory of {index : int} | Position of Entity.pos_t

(**The type representing the possible categories of item.*)
type stat_type = Combat of Combat.t | Buff of Buff.t

(**[item_type] is the type of an item. *)
type item_type = {
  seed: int;
  e: Entity.e;
  pos: item_pos;
  id : Entity.entity_id;
  unique_stats : stat_type;
  in_use : bool;
}

(** [Item] is a module representing the type of items in the game
    with a function to draw to the screen. *)
module Item : Entity with type t = item_type

(** [make_item seed id win x y] randomly generates an item with seed [seed] and
    id [id] at position [(x,y)] in window [win].*)
val make_item : int -> entity_id -> Window.window -> float -> float -> item_type


(**[is_combat_item item] returns [true] if [item] is a weapon, i.e. 
   [item.unique_stats] matches [Combat _] and returns [false] otherwise*)
val is_combat_item : item_type -> bool