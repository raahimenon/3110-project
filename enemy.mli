open Entity
type size_t = int*int
type name_t = string
type entity_frame = Animations.image
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int
type entity_state = Idle | Heal | Move of direction | Attack of direction

type enemy_type =  {
  animations: Animations.animation list;
  curr_anim: Animations.animation;
  curr_frame_num: int;
  direction: Entity.direction;
  size : size_t;
  name : name_t;
  frame : entity_frame;
  pos : Entity.pos_t;
  id : entity_id;
  max_health : int;
  health : int;
  state : entity_state;
  unique_stats : stat_type;
  logic : string;
}
module Enemy : Entity with type t = enemy_type

val make_enemy : name_t -> entity_id -> enemy_type