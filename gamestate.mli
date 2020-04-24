open Room
open Graphics
type state =
  {
    running: bool;
    current_room: Room.t;
    window : Window.window;
    input : Window.input
  }
val game_loop: state -> unit
