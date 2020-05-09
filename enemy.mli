open Entity
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int
type enemy_state = EIdle | EHeal | EMove of direction | EAttack of direction
                 |EDead
type enemy_type =  {
  e: Entity.e;
  id : entity_id;
  max_health : int;
  health : int;
  state : enemy_state;
  unique_stats : Combat.t;
  logic : string;
}
module Enemy : Entity with type t = enemy_type

val make_enemy : name_t -> entity_id -> Window.window -> enemy_type