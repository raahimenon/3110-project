open Entity
type size_t = int*int
type name_t = string
type entity_frame = Animations.image
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_state = Idle | Heal | Move of direction | Attack of direction
                  | Interact of direction
type entity_id = int

type player_type =  {
  animations: Animations.animation list;
  curr_anim: Animations.animation;
  curr_frame_num: int;
  direction: Entity.direction;
  size : size_t;
  name : name_t;
  frame : entity_frame;
  pos : Entity.pos_t;
  curr_tile : int*int;
  id : entity_id;
  max_health : int;
  health : int;
  state : entity_state;
  unique_stats : stat_type;
}
module Player : Entity with type t = player_type

val make_player : name_t -> entity_id -> player_type


val get_anim : player_type -> Entity.direction -> string -> Animations.animation