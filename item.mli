open Entity
type item_pos =  Inventory of {index : int} | Position of Entity.pos_t
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int

type item_type = {
  seed: int;
  e:Entity.e;
  pos: item_pos;
  id : entity_id;
  unique_stats : stat_type;
  in_use : bool;
}

module Item : Entity with type t = item_type

val make_item : int -> entity_id -> Window.window -> float -> float -> item_type

val is_combat_item : item_type -> bool