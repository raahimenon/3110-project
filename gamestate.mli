(**Logic for the game loop.*)

open Room

(**The type that represents the state of the game.*)
type state =
  {
    running: bool;
    current_room: Room.t;
    window : Window.window;
    input : Window.input list;
    icons: (string*Animations.image) list;
    last_anim_frame : int;
  }

(**[game_loop st time] returns the state of the game after the state 
   [st] is updated for one frame of the game, where [time] is the time elapsed 
   in milliseconds since the beginning of the previous game loop.*)
val game_loop : state -> int -> unit
