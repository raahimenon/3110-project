(** [Combat.t] is the type of a combat entity (to be added onto entities which fight) *)
type t = {
  attack : int;
  defense : int;
  movement_speed : int
}
