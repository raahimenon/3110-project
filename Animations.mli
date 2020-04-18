type image = Graphics.color array array
type animation = image array

val load_animation = string -> animation

val load_animations = string -> animation list

val next_frame = int -> animation -> image

val curr_frame = int -> animation -> image
