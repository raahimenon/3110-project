(** [generate_layout] runs the wave function collapse algorithm with the given 
    template and pattern size (patterns are always squares). 
    More information about wave function collapse can be found at 
    https://github.com/mxgmn/WaveFunctionCollapse *)
val generate_room : int -> Room.tile array array -> int -> int -> int -> float
  -> Window.window -> Room.t

val simple_gen : int -> Window.window -> Room.t