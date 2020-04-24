open Room
open Graphics
type state =
  {
    running: bool;
    current_room: Room.t
  }
val game_loop: state -> float -> unit
