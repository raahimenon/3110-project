let tile_size = 16.
let fps = 30.
let spf = 1. /. fps
let spf_in_milli = int_of_float (spf*.1000.)
let scale = 3.
let speed =  tile_size*.spf/.4.

let hrad = 10.
let vrad = 9.

let width = (int_of_float hrad) * 2 + 1
let height = (int_of_float vrad) * 2 + 1

let hud_bezel_tile = 0.5
let hud_bezel_px = tile_size *. hud_bezel_tile |> int_of_float

let inventory_size = vrad |> int_of_float