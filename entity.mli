open Animations
open Gamestate
type pos_t = 
  | Coordinates of {x : int; y:int} 
  | Inventory
type entity_id = int
type size_t = int * int
type name_t = string
type entity_frame = Animations.image
type entity_state
type stat_type = Combat of Combat.t | Buff of Buff.t

type t =  {
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

val update: Gamestate.state -> t ->  t
