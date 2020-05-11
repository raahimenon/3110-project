(**Entities in the game *)

(**[direction] represents the direction an entity is facing; 
   by default, entities face downwards *)
type direction = | Up | Down | Left | Right

(**[pos_t] is the position of an entity within the room in units of 
   [GameVars.tile_size] *)
type pos_t =  float * float

(** [size_t] is the size of an entity in the form (width,height) in 
    units of pixels*)
type size_t = int * int

(** [name_t] is the name of an entity used to refer to sprites.*)
type name_t = string

(** [entity_frame] is the current animation frame an entity is using *)
type entity_frame = Animations.image

(** [entity_id] is the id representing a game entity. *)
type entity_id = int

(** [Entity] is a module representing the type of an entity in the game
    that can be drawn to the screen. *)
module type Entity = sig
  (** represents the type of an entity*)
  type t 
  (** [draw win pos ent] draws an entity to the window [win] at position [pos]*)
  val draw: Window.window -> pos_t -> t -> unit
end

(**[e] is a type that contains data common to all entities.*)
type e = {
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
  curr_tile : int * int;
}

(** [get_anim e dir name] the animation of [e] in direction [dir] given with
    name [name].*)
val get_anim : e -> direction -> string -> Animations.animation

(** [change_animation e anim_name] is [e] with the current animation changed to 
    the animation given by [anim_name].*)
val change_animation : e  -> string -> e