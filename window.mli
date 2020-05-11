type window
(** To check if an input is a value, check if it is equal to 
    [Sdlkeycode.<Key>], where <Key> is a representation of the key you want 
    (make sure that it is capitalized) *)
type input = 
  | Key of Sdlkeycode.t
  | MWheel of int (*(Sdlkeycode.t * int) option*)
  | MClick of int

(** Some common key codes*)
val q : input
val esc : input
val w : input
val a : input
val s : input
val d : input
val e : input
val x : input
val enter : input
val space : input
val up : input
val lclick : input
val rclick : input

(** [create_window n w h] creates a window titled [n] with width [w] and
    height [h]*)
val create_window : string -> int -> int -> window

(** [clear w] clears the draw buffer for window [w] *)
val clear : window -> unit

val clear_white : window -> unit

(** [draw_image w i x y] draws image [i] to the buffer for window [w] at 
    position (x,y) in tiled coordinates with a scale factor of GameVars.scale *)
val draw_image : window -> Animations.image -> float -> float -> unit

(** [draw_image_raw w i x y] draws image [i] to the buffer for window [w] at 
    position (x,y) in tiled coordinates with a scale factor of GameVars.scale,
    without correcting for height shift by tilesize *)
val draw_image_raw : window -> Animations.image -> float -> float -> unit

(** [render w] renders the draw buffer for window [w]*)
val render : window -> unit

(** [exit_window] closes window [w] and quits it*)
val exit_window : window -> unit

(** [wait s] waits fot the specified number of seconds [s] before proceeding *)
val wait : float -> unit

(** [input_state] checks if there are any keys to be read at the moment *)
val input_query : input list -> input list

(** [collision p1 s1 p2 s2] is the truth value of whether or not a collision
    occurs between an object at position [p1] with size [s1] and the object
    at position [p2] with size [s2] *)
val collision : int*int -> int*int -> int*int -> int*int -> bool

(** [get_renderer win] is the [Sdlrender.t] associated with [win] *)
val get_renderer : window -> Sdlrender.t

(** [get_time ()] is the number of ticks since the game has started *)
val get_time : unit -> int

(** [wait ms] waits for [ms] milliseconds before proceeding with the program *)
val wait : ms: int -> unit

(** [draw_hud_box win int slot] draws the frame for the HUD and the marker
    over inventory slot [slot] *)
val draw_hud_box : window -> int -> unit

(** [draw_rect_col win (r,g,b) (x,y) (w,h)] draws a rectangle with color 
    [(r,g,b)] at position [(x,y)] with width [w] and height [h] to window
    [win] *)
val draw_rect_col : 
  window -> int * int * int -> float * float -> float * float -> unit

val health_col_ratio : float -> int*int*int