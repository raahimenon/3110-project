(** Enemies in the game*)

open Entity

(** The type of possible states of an enemy.*)
type enemy_state = 
  | EIdle | EHeal | EMove of direction | EAttack of direction
  | EDead | EKnock of direction * int

(**[enemy_type] is the type of an enemy *)
type enemy_type =  {
  e: Entity.e;
  id : Entity.entity_id;
  max_health : int;
  health : int;
  aggro_on : bool;
  state : enemy_state;
  unique_stats : Combat.t;
}
module Enemy : Entity with type t = enemy_type

(** [make_enemy seed id w x y diff] creates an enemy with id [id] from seed 
    [seed] at position [(x,y)] and difficulty level [diff] *)
val make_enemy : 
  int -> entity_id -> Window.window -> 
  float -> float -> float -> enemy_type