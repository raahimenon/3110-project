let tile_size = 16.
let fps = 30.
let spf = 1. /. fps
let spf_in_milli = int_of_float (spf*.1000.)

let anim_fps = 12.
let anim_spf = 1. /. anim_fps
let anim_spf_in_milli = int_of_float (anim_spf *. 1000.)

let scale = 3.
let speed =  tile_size*.spf/.40.

let hrad = 10.
let vrad = 9.

let width = (int_of_float hrad) * 2 + 1
let height = (int_of_float vrad) * 2 + 1

let hud_bezel_tile = 0.5
let hud_bezel_px = tile_size *. hud_bezel_tile |> int_of_float

let inventory_size = vrad |> int_of_float

let combat_objects = [|"sword"|]
let buff_objects = [|"blue-rupee"|]
let enemy_objects = [|"red-frog"; "voltorb"|]

let item_spawn_threshold = 4
let item_spawn_probability = 0.5


let bounding_boxes = Yojson.Basic.from_file "boundbox.json"

let boundbox_helper o value = 
  Yojson.Basic.Util.member o bounding_boxes
  |> Yojson.Basic.Util.member value
  |> Yojson.Basic.Util.to_int

let boundbox_width o = boundbox_helper o "width"
let boundbox_height o = boundbox_helper o "height"
let boundbox_wh o = (boundbox_width o, boundbox_height o)
let boundbox_x o = boundbox_helper o "x"
let boundbox_y o = boundbox_helper o "y"
let boundbox_xy o = (boundbox_x o, boundbox_y o)