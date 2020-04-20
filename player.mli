open Entity
type pos_t =  {x : int; y:int} 
type size_t = int*int
type name_t = int*int
type entity_frame = Animations.image
type entity_state = int
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int

type player_type =  {
  animations: (string * Animations.animation) list;
  curr_anim: string;
  curr_frame_num: int;
  size : size_t;
  name : name_t;
  frame : entity_frame;
  pos : pos_t;
  id : entity_id;
  max_health : int;
  health : int;
  state : entity_state;
  unique_stats : stat_type;
}
module Player : Entity with type t = player_type