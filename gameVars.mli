(**Module containing constants used in the execution of the game.*)
val tile_size : float
val fps : float
val spf : float
val spf_in_milli :int

val anim_fps : float
val anim_spf : float
val anim_spf_in_milli : int

val scale : float
val speed:float

val hrad : float
val vrad : float
val width : int
val height : int

val inventory_size : int

val hud_bezel_tile : float
val hud_bezel_px : int

val combat_objects : string array
val buff_objects : string array
val enemy_objects : string array

val item_spawn_threshold : int
val item_spawn_probability : float

val bounding_boxes : Yojson.Basic.t

val boundbox_width : string -> int
val boundbox_height : string -> int
val boundbox_wh : string -> int*int

val boundbox_x : string -> int
val boundbox_y : string -> int
val boundbox_xy : string -> int*int