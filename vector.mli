open Entity

(** representation of a 2 dimensional float vector*)
type t = float * float
(** representation of a 2 dimensional int vector*)
type s = int * int 

(** [to_int x]  is the integer vector that is formed by the integer parts 
    of entries of [x]*)
val to_int : t -> s

(** [from_int x] is the float vector that is formed from entries of [x] 
    considered as floats.*)
val from_int : s -> t

(** [from_int x] is the float vector that is formed from entries of [x] 
    after each entry is rounded down to the nearest integer.*)
val floor : t -> t

(** [add x y] is the float vector that is formed when [x] and [y] are added,
    with addition taking place componentwise.*)
val add : t -> t-> t

(** [greater x y] is [true] if all entries of  [x] are greater than the 
    corresponding entries of  [y], and [false] otherwise.*)
val greater : t -> t -> bool

(** [print x] returns a representation of s.*)
val print : t -> string

val abs_vec : t->t

(** [add_ints x y] is the int vector that is formed when [x] and [y] are added,
    with addition taking place componentwise.*)
val add_ints : s -> s -> s

(** [sub_ints x y] is the int vector that is formed when [x] and [y] are added,
    with addition taking place componentwise.*)
val sub_ints : s -> s -> s

(** [vec_of_dir dir] is the unit vector corresponding to direction [dir] in the
    plane.*)
val vec_of_dir : Entity.direction -> t

(** [dir_of_vec vec] is direction most closely aligned with vector [vec]. *)
val dir_of_vec : t -> Entity.direction

(** [magnitude vec] is magnitude of vector [vec]. *)
val magnitude : t -> float

(** [add sc x] is the float vector that is formed when each entry of [x] is
    multiplied with [sc].*)
val scale_vec : float -> t -> t

(** [subtract x y] is the float vector that is formed when [x] and [y] are 
    subtracted, with subtraction taking place componentwise.*)
val subtract : t -> t -> t

(** [distance x y] is the distance between [x] and [y] when considered as  
    points in the euclidean plane.*)
val distance : t -> t -> float 

(** [center x y] is the float vector that represents the position of [y] when 
    centered with respect to vector [x] and the middle of the screen.*)
val center : t -> t -> t