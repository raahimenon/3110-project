open Entity

type enemy_state = 
  | EIdle | EHeal | EMove of direction | EAttack of direction
  | EDead | EKnock of direction * int

type enemy_type =  {
  e: Entity.e;
  id : Entity.entity_id;
  max_health : int;
  health : int;
  aggro_on : bool;
  state : enemy_state;
  unique_stats : Combat.t;
}

module  Enemy : (Entity with type t = enemy_type)  = struct
  type t =  enemy_type
  let update t f = f t
  let draw win center t = let (x_draw,y_draw) = Vector.center center t.e.pos in 
    let ratio = float_of_int t.health /. float_of_int t.max_health in
    Window.draw_image win (snd t.e.curr_anim).(t.e.curr_frame_num) x_draw (y_draw);
    Window.draw_rect_col win (0,0,0) (x_draw,y_draw) (1., 1./.GameVars.tile_size);
    Window.draw_rect_col win (Window.health_col_ratio ratio) (x_draw,y_draw) (1.*.ratio, 1./.GameVars.tile_size)
end

let make_enemy seed id (win : Window.window) x y difficulty = 
  Random.init (seed + id);
  let name = 
    GameVars.enemy_objects.(Random.int (Array.length GameVars.enemy_objects)) in
  let animations = Animations.load_directions name (Window.get_renderer win) in
  let curr_anim = Animations.anim_from_dir_name animations "down" "idle" in
  let mhealth = Random.float 0.4 *. difficulty +. 0.8 *. 100. |> int_of_float in
  let atk = Random.float 0.8 *. difficulty +. 0.7 *. 10. in
  let spd = Random.float 0.8 *. difficulty +. 0.7 *. 8. |> int_of_float in
  {
    e = {
      animations = animations;
      curr_anim = curr_anim;
      curr_frame_num = 0;
      direction = Down;
      size = animations |> List.hd |> Animations.size;
      bounding_box = GameVars.boundbox_wh name;
      bounding_box_pos = GameVars.boundbox_xy name;
      name = name;
      frame = Animations.frame curr_anim 0; 
      pos = x, y;
      curr_tile =(int_of_float x,int_of_float y);
    };
    id = id;
    max_health = mhealth;
    aggro_on = true;
    health = mhealth;
    state = EIdle;
    unique_stats = {attack = atk; movement_speed = spd};
  }