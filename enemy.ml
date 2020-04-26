open Entity

type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int
type direction = |Up |Down |Left |Right
type entity_state = Idle | Heal | Move of Entity.direction | Attack of Entity.direction

type enemy_type =  {
  animations: (Animations.animation) list;
  curr_anim: Animations.animation;
  curr_frame_num: int;
  direction: Entity.direction;
  size : Entity.size_t;
  name : Entity.name_t;
  frame : Entity.entity_frame;
  pos : Entity.pos_t;
  id : entity_id;
  max_health : int;
  health : int;
  state : entity_state;
  unique_stats : stat_type;
  logic : string;
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
    pos = 0.,0.;
    id = id;
    max_health = 100;
    health = 100;
    state = Idle;
    unique_stats = Combat {attack = 10; defense = 10; movement_speed = 5};
    logic = "";
  }