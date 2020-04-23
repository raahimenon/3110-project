open Entity
open Graphics
type pos_t =  {x : float; y:float} 
type size_t = int*int
type name_t = string
type entity_frame = Animations.image
type entity_state = int
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int
type direction = |Up |Down |Left |Right

type player_type =  {
  animations: (Animations.animation) list;
  curr_anim: Animations.animation;
  curr_frame_num: int;
  direction: direction;
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

module  Player : (Entity with type t = player_type)  = struct
  type t =  player_type
  let update t f = f t
  let draw t = draw_image
      (Animations.frame t.curr_anim t.curr_frame_num |> make_image)
      (t.pos.x *. GameVars.tile_size |> int_of_float)
      (t.pos.y *. GameVars.tile_size |> int_of_float)
end

let make_player name id = 
  let animations = Animations.load_directions name in
  let curr_anim = Animations.anim_from_dir_name animations "down" "idle" in
  {
    animations = animations;
    curr_anim = curr_anim;
    curr_frame_num = 0;
    direction = Down;
    size = Animations.load_directions name |> List.hd |> Animations.size;
    name = name;
    frame = Animations.curr_frame 0 curr_anim; 
    pos = {x = 0.; y = 0.};
    id = id;
    max_health = 100;
    health = 100;
    state = 0;
    unique_stats = Combat {attack = 10; defense = 10; movement_speed = 5}
  }