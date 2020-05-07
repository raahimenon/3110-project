(**[direction] is the direction an entity is facing; by default, entities face
   downwards *)
type direction = |Up |Down |Left |Right

(**[pos_t] is the position of an entity within the room in units of 
   [GameVars.tile_size] *)
type pos_t =  float*float

(** [size_t] is the size of an entity in the form (width,height) *)
type size_t = int*int

(** [name_t] is the name of an entity *)
type name_t = string

(** [entity_frame] is the current animation frame an entity is using *)
type entity_frame = Animations.image

module type Entity = sig
  (*type pos_t = 
    | Coordinates of {x : int; y:int} 
    | Inventory
    type entity_id = int
    type size_t = int * int
    type name_t = string
    type entity_frame = Animations.image
    type entity_state
    type stat_type = Combat of Combat.t | Buff of Buff.t*)

  type t 
  (** [draw win pos ent] draws an entity to the window [win] at position [pos]*)
  val draw: Window.window -> pos_t -> t -> unit
end

type e ={
  animations: Animations.animation list;
  curr_anim: Animations.animation;
  curr_frame_num: int;
  direction: direction;
  size : size_t;
  bounding_box : size_t;
  bounding_box_pos : size_t;
  name : name_t;
  frame : entity_frame;
  pos : pos_t;
  curr_tile : int*int;
}

val get_anim : e -> direction -> string -> Animations.animation