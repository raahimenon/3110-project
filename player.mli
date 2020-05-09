open Entity
type stat_type = Combat of Combat.t | Buff of Buff.t
type player_state = Idle 
                  | Use_Item of direction*int
                  | Move of direction 
                  | Attack of direction*int*(Animations.animation option)
                  | Interact of direction*int
                  | Drop of direction*int
type entity_id = int

type player_type =  {
  e: Entity.e;
  tile_destination : int*int;
  being_attacked : bool;
  id : entity_id;
  max_health : int;
  health : int;
  state : player_state;
  unique_stats : Combat.t;
  inventory_slot : int;
  paused : bool;
  enemy_buffer : entity_id list;
}
module Player : Entity with type t = player_type

val make_player : name_t -> entity_id -> Window.window -> player_type