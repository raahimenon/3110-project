val background : Graphics.color

(** [load_image fname] is a [loaded_image] from the file with filename [fname]*)
val load_image : string -> Graphics.color array array

(** [im_to_str i] is the string representation of image [i]*)
val im_to_str : Graphics.color array array -> string