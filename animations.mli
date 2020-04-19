type image = Graphics.color array array
type animation = string * image array

(** [load_image f] is the [image] generated from the text file [f] relative to 
    the current filepath. [f] must be formatted like a matrix where position
    i,j is the comma delimited rgba value of the pixel, columns are split by
    spaces and rows by newlines*)
val load_image : string -> image

(** [load_animation f n] is the [animation] with name [n] generated from the 
    images stored in folder [f] relative to the current filepath *)
val load_animation : string -> string -> animation

(** [load_animations f] is the [animation list] generated from the animations
    stored in folder [f] relative to the current filepath *)
val load_animations : string -> animation list

(** [next_frame i a] is the next image in animation [a] given that it is
    currently on frame number [i] (this assumes the animation loops) *)
val next_frame : int -> animation -> image

(** [curr_frame i a] is the image at frame [i] in animation [a] *)
val curr_frame : int -> animation -> image

(** [im_to_str i] converts image [i] into a string formatted lke a matrix where
    position i,j is the comma delimited rgba value of the pixel, columns are 
    split by spaces, and rows by newlines
*)
val im_to_str : image -> string