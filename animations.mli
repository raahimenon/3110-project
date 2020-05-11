(** Module for handling sprite animations *)

(**[image] represents the type of an image that can be drawn to the screen. *)
type image = int * int * Sdltexture.t

(**[icon] represents the type of an icon that can be drawn to the hud. *)
type icon = string * image

(**[animation] represents the type of an animation or spritesheet. *)
type animation = string * image array

(** [load_image f r] is the [image] generated from the text file [f] relative to 
    the current filepath. [f] must be formatted like a matrix where position
    i,j is the comma delimited rgba value of the pixel, columns are split by
    spaces and rows by newlines*)
val load_image : string -> Sdlrender.t -> image

(** [load_animation f n r] is the [animation] with name [n] generated from the 
    images stored in folder [f] relative to the current filepath *)
val load_animation : string -> string -> Sdlrender.t -> animation

(** [load_direction f d r] is the [animation list] generated from the animations
    stored in folder [f] with direction [d] relative to the current filepath *)
val load_direction : string -> string -> Sdlrender.t -> animation list

(** [load_directions o r] is the [animation list] generated from the animations 
    of object with name [o] *)
val load_directions : string -> Sdlrender.t -> animation list

(** [load_icons r] is the list of icons and their names in the misc folder*)
val load_icons : Sdlrender.t -> (string*image) list

(** [get_icon i is] is the image of the icon named [i] from icon list [is] *)
val get_icon : string -> icon list -> image

(** [next_frame i a] is the next frame index next image in animation [a] given 
    that it is currently on frame number [i] (assuming the animation loops) *)
val next_frame : int -> animation -> int

(** [size_im i] is (w,h) where [w] is the width and [h] is the height of 
    image [i]*)
val size_im : image -> (int * int)

(** [size a] is (w,h) where [w] is the width and [h] is the height of
    animation [a] *)
val size : animation -> (int * int)

(** [name a] is the string name of animation [a] *)
val name : animation -> string

(** [anims_from_direction anims d] is the list of animations in [anim] facing
    direction [d] (their name must be of the form "<direction>*<name>"). [d]
    must be lowercase *)
val anims_from_direction : animation list -> string -> animation list

(** [anim_from_name anims n] is the animation with exact name [n] in [anims] - 
    [n] must include direction (it must be of the form 
    "<direction>*<anim_name>") *)
val anim_from_name : animation list -> string -> animation

(** [anim_from_name_dir anims d n] is the animation facing direction [d] with
    name [n] - [n] must not include direction and [d] must be lowercase*)
val anim_from_dir_name : animation list -> string -> string -> animation

(** [frame a i] is the frame at index [i] of animation [a] *)
val frame : animation -> int -> image