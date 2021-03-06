(** The Player character in the game*)

open Entity
open Enemy

(** The type of possible states of a player character.*)
type player_state = 
  | Idle 
  | Use_Item of Entity.direction * int 
  | Move of Entity.direction 
  | Attack of Entity.direction * int * (Animations.animation option)
  | Interact of Entity.direction * int
  | Drop of Entity.direction * int
  | Knock of Entity.direction * int


(**[player_type] is the type of a player character *)
type player_type =  {
  e: Entity.e;
  tile_destination:int*int;
  attacking_enemies : Enemy.t list;
  last_damage : int;
  id : Entity.entity_id;
  max_health : int;
  health : int;
  state : player_state;
  unique_stats : Combat.t;
  inventory_slot : int;
  paused: bool;
  enemy_buffer: entity_id list;
}
(** [Player] is a module representing the type of the player character
    in the with a function to draw to the screen. *)
module Player : Entity with type t = player_type

(** [make_player name id  window x y] creates a player character with name 
    [name] at position [(x,y)] in window [win].*)
val make_player : name_t -> entity_id ->
  Window.window -> float -> float -> player_type