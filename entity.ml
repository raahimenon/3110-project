open Combat
open Buff
open Graphics


module type Entity = sig
  (*type pos_t = 
    | Coordinates of {x : int; y:int} 
    | Inventory
    type entity_id = int
    type size_t = int * int
    type name_t = string
    type entity_frame = Animations.image
    type entity_state
    type stat_type = Combat of Combat.t | Buff of Buff.t*)
  type t 
  val draw: t-> unit
  val update: t-> (t-> t) -> t
end

