(**Function for saving the state of the game.*)

(** [save r s] saves the contents of room [r] in file [s] if possible. *)
val save : Room.t -> string -> unit