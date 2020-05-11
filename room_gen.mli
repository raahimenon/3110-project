(** [generate_layout] runs the wave function collapse algorithm with the given 
    template and pattern size (patterns are always squares). 
    More information about Wave Function Collapse can be found at 
    https://github.com/mxgmn/WaveFunctionCollapse. Enemy placement is done 
    using Perlin Worms. More information about Perlin Worms can be found at 
    https://dl.acm.org/doi/pdf/10.1145/325165.325247. Item placement is 
    done using Prim's MST algorithm. More information about Prim's can be 
    found at https://en.wikipedia.org/wiki/Prim%27s_algorithm. *)
val generate_room : int -> Room.tile array array -> int -> int -> int -> float
  -> Window.window -> Room.t

(** [simple_gen] is a wrapper for generate_room that obfuscates constants 
    which do not need to be changed during typical use. *)
val simple_gen : int -> Window.window -> Room.t