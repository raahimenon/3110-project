open Entity
open Vector
type pos_t =  Inventory of {index : int} | Position of {x : float; y:float} 
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int

type item_type = {
  animations: Animations.animation list;
  curr_anim: Animations.animation;
  curr_frame_num: int;
  size : Entity.size_t;
  name : Entity.name_t;
  frame : Entity.entity_frame;
  pos : pos_t;
  curr_tile : int*int;
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
      Window.draw_image win (snd t.curr_anim).(t.curr_frame_num) x_draw y_draw;
      begin match t.unique_stats with
        | Buff {durability; max_durability} ->
          let ratio = float_of_int durability /. (float_of_int max_durability) in
          Window.draw_rect_col win (255. *. (1. -. ratio) |> int_of_float, 255.*.ratio|>int_of_float, 0) (x_draw+.0.1,y_draw+.0.8) (0.8*.ratio, 1./.GameVars.tile_size)
        | _ -> () end
    | Position {x;y} -> let (x_draw,y_draw) = Vector.center center (x,y) in 
      Window.draw_image win (snd t.curr_anim).(t.curr_frame_num) x_draw y_draw
end

let make_item name id (win: Window.window) x y = 
  let animations = Animations.load_directions name (Window.get_renderer win) in
  let curr_anim = Animations.anim_from_dir_name animations "down" "idle" in
  {
    animations = animations;
    curr_anim = curr_anim;
    curr_frame_num = 0;
    size = animations |> List.hd |> Animations.size;
    name = name;
    frame = Animations.curr_frame 0 curr_anim; 
    pos = Position {x = float_of_int x; y = float_of_int y};
    curr_tile = x, y;
    id = id;
    unique_stats = Buff {
        max_durability = 2;
        durability = 2;
        effect = [Health 30]
      };
    in_use = false
  }