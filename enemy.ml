open Entity

type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int
type direction = |Up |Down |Left |Right
type enemy_state = EIdle | EHeal | EMove of Entity.direction | EAttack of Entity.direction
                 |EDead
type enemy_type =  {
  e: Entity.e;
  id : entity_id;
  max_health : int;
  health : int;
  state : enemy_state;
  unique_stats : stat_type;
  logic : string;
}

module  Enemy : (Entity with type t = enemy_type)  = struct
  type t =  enemy_type
  let update t f = f t
  let draw w center t = ()
end

let make_enemy name id (win : Window.window) = 
  let animations = Animations.load_directions name (Window.get_renderer win) in
  let curr_anim = Animations.anim_from_dir_name animations "down" "idle" in
  {
    e = {
      animations = animations;
      curr_anim = curr_anim;
      curr_frame_num = 0;
      direction = Down;
      size = animations |> List.hd |> Animations.size;
      bounding_box = (16,13);
      bounding_box_pos = (0,2);
      name = name;
      frame = Animations.curr_frame 0 curr_anim; 
      pos = 0.,0.;
      curr_tile =(0,0);
    };
    id = id;
    max_health = 100;
    health = 100;
    state = EIdle;
    unique_stats = Combat {attack = 10; defense = 10; movement_speed = 5};
    logic = "";
  }