open Entity
open Vector

type item_pos =  Inventory of {index : int} | Position of Entity.pos_t
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int

type item_type = {
  e: Entity.e;
  pos: item_pos;
  id : entity_id;
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
  {
    e ={animations = animations;
        curr_anim = curr_anim;
        curr_frame_num = 0;
        size = animations |> List.hd |> Animations.size ;
        bounding_box = GameVars.boundbox_wh name;
        bounding_box_pos = GameVars.boundbox_xy name;
        direction = Down;
        name = name;
        frame = Animations.frame curr_anim 0; 
        pos = (float_of_int x, float_of_int y);
        curr_tile = x, y;};
    pos = Position (float_of_int x, float_of_int y);
    id = id;
    unique_stats = if combat = 0 then 
        Buff {
          max_durability = 2;
          durability = 2;
          effect = [Health 30];
        } else
        Combat {
          attack =  30;
          defense = 20;
          movement_speed = 5;
        }
    ;
    in_use = false
  }

let is_combat_item i = match i.unique_stats with
  | Combat _ -> true
  | Buff _ -> false