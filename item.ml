open Entity
open Vector

type item_pos =  Inventory of {index : int} | Position of Entity.pos_t

type stat_type = Combat of Combat.t | Buff of Buff.t

type item_type = {
  seed: int;
  e: Entity.e;
  pos: item_pos;
  id : Entity.entity_id;
  unique_stats : stat_type;
  in_use : bool;
}

module Item : (Entity with type t = item_type)  = struct
  type t =  item_type
  let update t f = f t
  let draw win center t = match t.pos with
    | Inventory {index = idx} -> 
      let x_draw = GameVars.hud_bezel_tile in
      let y_draw = float_of_int idx *. 2. +. 1. in
      Window.draw_image win (snd t.e.curr_anim).(t.e.curr_frame_num) x_draw y_draw;
      begin match t.unique_stats with
        | Buff {durability; max_durability} ->
          let ratio = float_of_int durability /. (float_of_int max_durability) in
          Window.draw_rect_col win 
            (Window.health_col_ratio ratio)
            (x_draw,y_draw+.1.+.1./.GameVars.tile_size)
            (1.*.ratio, 1./.GameVars.tile_size)
        | _ -> () end
    | Position (x,y) -> let (x_draw,y_draw) = Vector.center center (x,y) in
      Window.draw_image win (snd t.e.curr_anim).(t.e.curr_frame_num) x_draw y_draw
end

let make_item seed id (win: Window.window) x y = 
  Random.init (seed + id);
  let combat = Random.int 2 in 
  let name = 
    if combat = 0 then 
      GameVars.buff_objects.(Random.int (Array.length GameVars.buff_objects))
    else
      GameVars.combat_objects.(Random.int (Array.length GameVars.combat_objects))
  in
  let animations = Animations.load_directions name (Window.get_renderer win) in
  let curr_anim = Animations.anim_from_dir_name animations "down" "idle" in
  let int_x = int_of_float x in
  let int_y = int_of_float y in
  {
    seed = seed;
    e ={
      animations = animations;
      curr_anim = curr_anim;
      curr_frame_num = 0;
      size = animations |> List.hd |> Animations.size ;
      bounding_box = GameVars.boundbox_wh name;
      bounding_box_pos = GameVars.boundbox_xy name;
      direction = Down;
      name = name;
      frame = Animations.frame curr_anim 0; 
      pos = (x, y);
      curr_tile = int_x, int_y;};

    pos = if x >= 0. then Position (x, y) else Inventory{index = int_x * -1 - 1};
    id = id;
    unique_stats = if combat = 0 then
        let mhealth = if Random.int 2 = 1 then Random.int 10 + 1 else 0 in
        let atk = if Random.int 2 = 1 then Random.int 5 + 1 else 0 in
        let spd = if Random.int 2 = 1 then Random.int 2 + 1 else 0 in
        let health = if Random.int 2 = 1 || (mhealth = 0 && atk = 0 && spd = 0) 
          then Random.int 50 + 1 else 0 in
        let durability = 
          if atk > 0 || spd > 0 then 1 else 
            (Random.float 5. +. 2.) *. 
            begin if health > 0 then Random.float 1. else 1. end *. 
            begin if mhealth > 0 then Random.float 1. else 1. end 
            |> Float.round |> int_of_float 
            |> (+) 1 in
        Buff {
          max_durability = durability;
          durability = durability;
          effect = 
            [Health health; Max_health mhealth; Attack atk; Movement_speed spd];
        } else begin
        let atk = Random.float 1. +. 0.7 in
        let spd = Random.int 5 + 1 - int_of_float (10. *. (atk-.0.7)/.1.) in
        Combat {
          attack =  atk;
          movement_speed = spd;
        } end
    ;
    in_use = false
  }

let is_combat_item i = match i.unique_stats with
  | Combat _ -> true
  | Buff _ -> false