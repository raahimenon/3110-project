type direction = |Up |Down |Left |Right
type pos_t =  float*float
type size_t = int*int
type name_t = string
type entity_frame = Animations.image

module type Entity = sig
  type t 
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