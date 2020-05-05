open Entity
type stat_type = Combat of Combat.t | Buff of Buff.t
type player_state = Idle | Use_Item of direction*int| Move of direction | Attack of direction
                  | Interact of direction*int
type entity_id = int

type player_type =  {
  e: Entity.e;
  tile_destination : int*int;
  reach_dest : bool;
  id : entity_id;
  max_health : int;
  health : int;
  state : player_state;
  unique_stats : stat_type;
  inventory_slot : int;
  attack : int;
  defence : int;
  paused : bool;
}
module Player : Entity with type t = player_type

val make_player : name_t -> entity_id -> Window.window -> player_type

val get_anim : player_type -> Entity.direction -> string -> Animations.animation