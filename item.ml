open Entity
open Vector
type pos_t =  Inventory | Position of Entity.pos_t
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int

type item_type = {
  animations: Animations.animation list;
  curr_anim: Animations.animation;
  curr_frame_num: int;
  size : Entity.size_t;
  bounding_box : Entity.size_t;
  name : Entity.name_t;
  frame : Entity.entity_frame;
  pos : pos_t;
  curr_tile : int*int;
  id : entity_id;
  unique_stats : stat_type;
}

module Item : (Entity with type t = item_type)  = struct
  type t =  item_type
  let update t f = f t
  let draw win center t = match t.pos with
    | Inventory -> ()
    | Position (x,y) -> let (x_draw,y_draw) = Vector.center center (x,y) in 
      Window.draw_image win (snd t.curr_anim).(t.curr_frame_num) x_draw y_draw
end

let make_item name id (win: Window.window)= 
  let animations = Animations.load_directions name (Window.get_renderer win) in
  let curr_anim = Animations.anim_from_dir_name animations "down" "idle" in
  {
    animations = animations;
    curr_anim = curr_anim;
    curr_frame_num = 0;
    size = animations |> List.hd |> Animations.size ;
    bounding_box = animations |> List.hd |> Animations.size;
    name = name;
    frame = Animations.curr_frame 0 curr_anim; 
    pos = Position  (2.,3.);
    curr_tile = (2,3);
    id = id;
    unique_stats = Buff {
        max_durability = 1;
        durability = 1;
        effect = []
      }
  }