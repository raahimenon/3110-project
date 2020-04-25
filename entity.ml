type direction = |Up |Down |Left |Right
type pos_t =  float*float
type size_t = int*int
type name_t = string
type entity_frame = Animations.image

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
  val draw: Window.window -> t-> unit

  val update: t-> (t-> t) -> t
end

