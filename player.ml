open Entity
open Vector
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int
type direction = |Up |Down |Left |Right
type player_state = Idle 
                  | Use_Item of Entity.direction*int 
                  | Move of Entity.direction 
                  | Attack of Entity.direction*int*(Animations.animation option)
                  | Interact of Entity.direction*int

type player_type =  {
  e: Entity.e;
  tile_destination:int*int;
  being_attacked : bool;
  id : entity_id;
  max_health : int;
  health : int;
  state : player_state;
  unique_stats : stat_type;
  inventory_slot : int;
  attack: int;
  defence: int;
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

let make_player name id (win : Window.window)= 
  let animations = Animations.load_directions name (Window.get_renderer win) in
  let curr_anim = Animations.anim_from_dir_name animations "down" "idle" in
  {
    e = 
      {animations = animations;
       curr_anim = curr_anim;
       curr_frame_num = 0;
       direction = Down;
       size = animations |> List.hd |> Animations.size;
       bounding_box = (14,13);
       bounding_box_pos = (1,2);
       name = name;
       frame = Animations.frame curr_anim 0; 
       pos = 1.,2.;
       curr_tile = 1,2;};
    id = id;
    tile_destination = 0,0;
    being_attacked = false;
    max_health = 100;
    health = 100;
    state = Idle;
    unique_stats = Combat {attack = 10; defense = 10; movement_speed = 5};
    inventory_slot = 0;
    attack = 10;
    defence = 10;
    paused = false;
    enemy_buffer = [];
  }

