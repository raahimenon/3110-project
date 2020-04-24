open Entity
type pos_t =  Inventory | Position of {x : float; y:float} 
type size_t = int*int
type name_t = string
type entity_frame = Animations.image
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int

type item_type = {
  animations: Animations.animation list;
  curr_anim: Animations.animation;
  curr_frame_num: int;
  size : size_t;
  name : name_t;
  frame : entity_frame;
  pos : pos_t;
  id : entity_id;
  unique_stats : stat_type;
}
