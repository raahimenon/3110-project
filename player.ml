open Entity
open Graphics
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int
type direction = |Up |Down |Left |Right
type entity_state = Idle | Heal | Move of Entity.direction | Attack of Entity.direction
                  | Interact of Entity.direction

type player_type =  {
  animations: (Animations.animation) list;
  curr_anim: Animations.animation;
  curr_frame_num: int;
  direction: Entity.direction;
  size : Entity.size_t;
  name : Entity.name_t;
  frame : Entity.entity_frame;
  pos : Entity.pos_t;
  curr_tile : int*int;
  id : entity_id;
  max_health : int;
  health : int;
  state : entity_state;
  unique_stats : stat_type;
}
let left (a,_) = a
let right (_,b) = b
module Player : (Entity with type t = player_type)  = struct
  type t =  player_type
  let update t f = f t
  let draw win t =  Window.draw_image win (snd t.curr_anim).(t.curr_frame_num) (t.pos|>left) (t.pos|>right)
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
    pos = 0.,0.;
    id = id;
    curr_tile = 0,0;

    max_health = 100;
    health = 100;
    state = Idle;
    unique_stats = Combat {attack = 10; defense = 10; movement_speed = 5}
  }

let get_anim (player:player_type) (dir : Entity.direction) (name:string) : Animations.animation =
  match dir with
  | Down ->  Animations.anim_from_dir_name player.animations "down" name
  | Up -> Animations.anim_from_dir_name player.animations "up" name
  | Right -> Animations.anim_from_dir_name player.animations "right" name
  | Left -> Animations.anim_from_dir_name player.animations "left" name