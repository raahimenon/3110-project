open Entity
open Enemy
type player_state = 
  | Idle 
  | Use_Item of Entity.direction * int 
  | Move of Entity.direction 
  | Attack of Entity.direction * int * (Animations.animation option)
  | Interact of Entity.direction * int
  | Drop of Entity.direction * int
  | Knock of Entity.direction * int
type entity_id = int

type player_type =  {
  e: Entity.e;
  tile_destination:int*int;
  attacking_enemies : Enemy.t list;
  last_damage : int;
  id : entity_id;
  max_health : int;
  health : int;
  state : player_state;
  unique_stats : Combat.t;
  inventory_slot : int;
  paused: bool;
  enemy_buffer: entity_id list;
}
module Player : Entity with type t = player_type

val make_player : name_t -> entity_id -> Window.window -> float -> float -> player_type