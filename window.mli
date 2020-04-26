type window
(** To check if an input is a value, check if it is equal to 
    [Sdlkeycode.<Key>], where <Key> is a representation of the key you want 
    (make sure that it is capitalized) *)
type input = Sdlkeycode.t list(*(Sdlkeycode.t * int) option*)

(** Some common key codes*)
val q : Sdlkeycode.t
val esc : Sdlkeycode.t
val w : Sdlkeycode.t
val a : Sdlkeycode.t
val s : Sdlkeycode.t
val d : Sdlkeycode.t
val enter : Sdlkeycode.t
val space : Sdlkeycode.t

(** [create_window n w h] creates a window titled [n] with width [w] and
    height [h]*)
val create_window : string -> int -> int -> window

(** [clear w] clears the draw buffer for window [w] *)
val clear : window -> unit

val clear_white : window -> unit

(** [draw_image w i x y] draws image [i] to the buffer for window [w] at 
    position (x,y) in tiled coordinates with a scale factor of GameVars.scale *)
val draw_image : window -> Animations.image -> float -> float -> unit

(** [render w] renders the draw buffer for window [w]*)
val render : window -> unit

(** [exit_window] closes window [w] and quits it*)
val exit_window : window -> unit

(** [wait s] waits fot the specified number of seconds [s] before proceeding *)
val wait : float -> unit

(** [input_state] checks if there are any keys to be read at the moment *)
val input_query : input -> input

val get_renderer : window -> Sdlrender.t

val get_time : unit -> int

val wait : ms: int -> unit