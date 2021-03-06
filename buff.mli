(**[Buff] will be the module containing buff effects*)

(** [buff_type] is a type representing the effects of a given buff.*)
type buff_type = Max_health of int 
               | Health of int 
               | Attack of int 
               | Movement_speed of int

type t = {
  max_durability : int;
  durability : int;
  effect : buff_type list
}
