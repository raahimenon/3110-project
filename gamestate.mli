open Room
type state =
  {
    running: bool;
    current_room: Room.t;
    window : Window.window;
    input : Window.input list;
    icons: (string*Animations.image) list;
  }
val game_loop: state -> int -> unit
