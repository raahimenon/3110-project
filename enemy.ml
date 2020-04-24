open Entity
open Graphics
type pos_t =  {x : float; y:float} 
type size_t = int*int
type name_t = string
type entity_frame = Animations.image
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int
type direction = |Up |Down |Left |Right
type entity_state = Idle | Heal | Move of direction | Attack of direction

type enemy_type =  {
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

module  Enemy : (Entity with type t = enemy_type)  = struct
  type t =  enemy_type
  let update t f = f t
  let draw w t = ()
end

let make_enemy name id = 
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
    state = Idle;
    unique_stats = Combat {attack = 10; defense = 10; movement_speed = 5}
  }