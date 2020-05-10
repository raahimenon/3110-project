open Entity
open Vector
open Enemy
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int
type player_state = 
  | Idle 
  | Use_Item of Entity.direction * int 
  | Move of Entity.direction 
  | Attack of Entity.direction * int * (Animations.animation option)
  | Interact of Entity.direction * int
  | Drop of Entity.direction * int
  | Knock of Entity.direction * int

type player_type =  {
  e: Entity.e;
  tile_destination:int*int;
  attacking_enemies : Enemy.t list;
  last_damage : int;
  id : entity_id;
  max_health : int;
  health : int;
  state : player_state;
  unique_stats : Combat.t;
  inventory_slot : int;
  paused: bool;
  enemy_buffer: entity_id list;
}

module Player : (Entity with type t = player_type)  = struct
  type t =  player_type
  let update t f = f t
  let draw win center t =  
    let (x_draw,y_draw) = Vector.center center t.e.pos in 
    let x_front, y_front = match t.e.direction with 
      | Right -> 0.5,0. | Left -> -0.5,0. | Up -> 0., -0.5 | Down -> 0., 0.5 in
    if y_front < 0. || x_front < 0. then begin
      begin match t.state with
        | Attack (_, _, Some anim) -> 
          Window.draw_image win 
            (Animations.frame anim t.e.curr_frame_num)
            (x_draw +. x_front) (y_draw +. y_front)
        | _ -> () end;
      Window.draw_image win (snd t.e.curr_anim).(t.e.curr_frame_num) x_draw (y_draw) end
    else begin
      Window.draw_image win (snd t.e.curr_anim).(t.e.curr_frame_num) x_draw (y_draw);
      begin match t.state with
        | Attack (_, _, Some anim) -> 
          Window.draw_image win 
            (Animations.frame anim t.e.curr_frame_num)
            (x_draw +. x_front) (y_draw +. y_front)
        | _ -> () end end
end

let make_player name id (win : Window.window) x y= 
  let animations = Animations.load_directions name (Window.get_renderer win) in
  let curr_anim = Animations.anim_from_dir_name animations "down" "idle" in
  {
    e = 
      {animations = animations;
       curr_anim = curr_anim;
       curr_frame_num = 0;
       direction = Down;
       size = animations |> List.hd |> Animations.size;
       bounding_box = GameVars.boundbox_wh name;
       bounding_box_pos = GameVars.boundbox_xy name;
       name = name;
       frame = Animations.frame curr_anim 0; 
       pos = x,y;
       curr_tile = int_of_float x,int_of_float y;};
    id = id;
    tile_destination = 0,0;
    attacking_enemies = [];
    last_damage = -500;
    max_health = 100;
    health = 100;
    state = Idle;
    unique_stats = {attack = 10.; movement_speed = 10};
    inventory_slot = 0;
    paused = false;
    enemy_buffer = [];
  }

