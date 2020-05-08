open Room
type state =
  {
    running: bool;
    current_room: Room.t;
    window : Window.window;
    input : Window.input list;
    icons: (string*Animations.image) list;
    last_anim_frame : int;
  }
val game_loop: state -> int -> unit
