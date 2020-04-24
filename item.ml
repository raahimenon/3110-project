open Entity
open Graphics
type pos_t =  Inventory | Position of {x : float; y:float} 
type size_t = int*int
type name_t = string
type entity_frame = Animations.image
type stat_type = Combat of Combat.t | Buff of Buff.t
type entity_id = int

type item_type = {
  animations: Animations.animation list;
  curr_anim: Animations.animation;
  curr_frame_num: int;
  size : size_t;
  name : name_t;
  frame : entity_frame;
  pos : pos_t;
  id : entity_id;
  unique_stats : stat_type;
}

module Item : (Entity with type t = item_type)  = struct
  type t =  item_type
  let update t f = f t
  let draw t = 
    let pos = match t.pos with
      | Inventory -> failwith "cannot draw inventory item"
      | Position record -> [|record.x; record.y|]
    in
    draw_image
      (Animations.frame t.curr_anim t.curr_frame_num |> make_image)
      (pos.(0) *. GameVars.tile_size |> int_of_float)
      (pos.(1) *. GameVars.tile_size |> int_of_float)
end

let make_item name id = 
  let animations = Animations.load_directions name in
  let curr_anim = Animations.anim_from_dir_name animations "down" "idle" in
  {
    animations = animations;
    curr_anim = curr_anim;
    curr_frame_num = 0;
    size = Animations.load_directions name |> List.hd |> Animations.size;
    name = name;
    frame = Animations.curr_frame 0 curr_anim; 
    pos = Position {x = 0.; y = 0.};
    id = id;
    unique_stats = Buff {
        max_durability = 1;
        durability = 1;
        effect = []
      }
  }