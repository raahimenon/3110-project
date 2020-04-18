type state =
  {
    current_room: Room.t
  }
val game_loop: state -> state