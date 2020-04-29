open Entity
type stat_type = Combat of Combat.t | Buff of Buff.t
type player_state = Idle | Heal | Move of direction | Attack of direction
                  | Interact of direction*int
type entity_id = int

type player_type =  {
  animations: Animations.animation list;
  curr_anim: Animations.animation;
  curr_frame_num: int;
  direction: Entity.direction;
  size : Entity.size_t;
  bounding_box : Entity.size_t;
  name : Entity.name_t;
  frame : Entity.entity_frame;
  pos : Entity.pos_t;
  curr_tile : int*int;
  tile_destination : int*int;
  reach_dest : bool;
  id : entity_id;
  max_health : int;
  health : int;
  state : player_state;
  unique_stats : stat_type;
}
module Player : Entity with type t = player_type

val make_player : name_t -> entity_id -> Window.window -> player_type

val get_anim : player_type -> Entity.direction -> string -> Animations.animation