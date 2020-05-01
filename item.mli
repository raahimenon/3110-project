open Entity
type pos_t =  Inventory | Position of Entity.pos_t
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int

type item_type = {
  animations: Animations.animation list;
  curr_anim: Animations.animation;
  curr_frame_num: int;
  size : Entity.size_t;
  bounding_box : Entity.size_t;
  bounding_box_pos : Entity.size_t;
  name : Entity.name_t;
  frame : Entity.entity_frame;
  pos : pos_t;
  curr_tile : int*int;
  id : entity_id;
  unique_stats : stat_type;
}

module Item : Entity with type t = item_type

val make_item : name_t -> entity_id -> Window.window -> item_type