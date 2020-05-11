open OUnit2

(** Test Plan:

    OUnit VS Manual Testing: 
      Entity generation functions were tested using this OUnit test suite.
      (Note that for Enemies and Items, specific parameters could not be tested
      as their values are generated randomly. This was left for playtesting).

      Collision functions were partially tested using this OUnit test suite. The
      parts not tested involve responsiveness, which must be playtested instead.

      Load functions were partially tested using this OUnit test suite. The 
      parts not tested involve room generation, which must be playtested 
      instead.

      Inventory functions were partially tested using this OUnit test suite. The 
      parts not tested involve UI elements, which must be playtested instead.

      GameState functions were not tested using this OUnit test suite; being 
      in response to user input, they cannot be tested in this manner.

      Room generation functions were not tested using this OUnit test suite; 
      they were tested manually as the quality of room generation is subjective.

      Window functions were not tested using this OUnit test suite; being 
      visual, they cannot be tested in this manner.

    Which Modules were Tested and How:
      OUnit Testing was done via a mix of Black-Box and Glass-Box testing; only 
      exposed functions were tested (black-box) but more functionality is
      exposed than necessary, resulting in a somewhat glass-box result. 

      For example, "entity.e"s are not abstracted, so we can test them directly.

      Modules Tested via this OUnit Test Suite:
        Buff
        Combat
        Enemy
        Entity
        GameVars
        Load
        Save
        Player
        Item
        Vector

    Completeness of Test Cases:
    All aspects of the system that could be tested quantitatively are tested
    in this suite. Any aspects left out were left out intentionally as they must
    be playtested visually to demonstrate correctness.
*)

(** HELPER FUNCTIONS FOR TEST SUITE *)

(** [make_player_id_test name player expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [player.id] with
    [expected_output]
*)
let make_player_id_test
    (name:string)
    (player:Player.Player.t)
    (expected_output:int) : test =
  name >:: (fun _ ->
      assert_equal expected_output player.id
        ~printer:(string_of_int))

(** [make_player_state_test name player expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [player.state] with
    [expected_output]
*)
let make_player_state_test
    (name:string)
    (player:Player.Player.t)
    (expected_output:Player.player_state) : test =
  name >:: (fun _ ->
      assert_equal expected_output player.state
        ~printer:(function
            | Player.Idle -> "idle"
            | Player.Move _ -> "move"
            | Player.Use_Item _ -> "use item"
            | Player.Attack _ -> "attack"
            | Player.Interact _ -> "interact"
            | Player.Drop  _ -> "drop"
            | Player.Knock _ -> "knock"))

(** [make_player_health_test name player expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [player.health] with
    [expected_output] *)
let make_player_health_test
    (name:string)
    (player:Player.Player.t)
    (expected_output:int) : test =
  name >:: (fun _ ->
      assert_equal expected_output player.health
        ~printer:(string_of_int))

(** [make_player_max_health_test name player expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [player.max_health] with
    [expected_output] *)
let make_player_max_health_test
    (name:string)
    (player:Player.Player.t)
    (expected_output:int) : test =
  name >:: (fun _ ->
      assert_equal expected_output player.max_health
        ~printer:(string_of_int))

(** [make_player_attack_test name player expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [player.unique_stats.attack]
    with [expected_output] *)
let make_player_attack_test
    (name:string)
    (player:Player.Player.t)
    (expected_output:float) : test =
  name >:: (fun _ ->
      assert_equal expected_output player.unique_stats.attack
        ~printer:(string_of_float))

(** [make_player_speed_test name player expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [player.unique_stats.speed]
    with [expected_output] *)
let make_player_speed_test
    (name:string)
    (player:Player.Player.t)
    (expected_output:int) : test =
  name >:: (fun _ ->
      assert_equal expected_output player.unique_stats.movement_speed
        ~printer:(string_of_int))

(**[make_entity_name_test ent expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [e.name] with
   [expected_output] *)
let make_entity_name_test
    (name:string)
    (ent:Entity.e)
    (expected_output:string) : test =
  name >:: (fun _ ->
      assert_equal expected_output ent.name
        ~printer:(fun x -> x))

(**[make_entity_direction_test ent expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [e.direction] with
   [expected_output] *)
let make_entity_direction_test
    (name:string)
    (ent:Entity.e)
    (expected_output:Entity.direction) : test =
  name >:: (fun _ ->
      assert_equal expected_output ent.direction
        ~printer:(function
            | Entity.Down -> "down"
            | Entity.Up -> "up"
            | Entity.Right -> "right"
            | Entity.Left -> "left" ))

(**[make_entity_pos_test ent expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [e.pos] with
   [expected_output] *)
let make_entity_pos_test
    (name:string)
    (ent:Entity.e)
    (expected_output:Entity.pos_t) : test =
  name >:: (fun _ ->
      assert_equal expected_output ent.pos
        ~printer:(fun (x,y) -> string_of_float x ^ "," ^ string_of_float y))    

(**[make_entity_bbox_test ent expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [e.bounding_box, e.bounding_box_pos]
    with [expected_output] *)
let make_entity_bbox_test
    (name:string)
    (ent:Entity.e)
    (expected_output:Entity.size_t*Entity.size_t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (ent.bounding_box, ent.bounding_box_pos)
        ~printer:(fun ((w,h),(x,y)) -> 
            "\nw,h: " ^ string_of_int w ^ "," ^ string_of_int h ^ "\n" ^
            "x,y: " ^ string_of_int x ^ "," ^ string_of_int y ^ "\n" ))   

(**[make_entity_num_anims_test ent expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [List.length ent.animations]
    with [expected_output] *)
let make_entity_num_anims_test
    (name:string)
    (ent:Entity.e)
    (expected_output:int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (List.length ent.animations)
        ~printer:(string_of_int))

(**[make_item_type_test item expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [item.unique_stats] as either
   "buff" or "combat" with [expected_output] *)
let make_item_type_test
    (name:string)
    (item:Item.Item.t)
    (expected_output:string) : test =
  name >:: (fun _ ->
      assert_equal expected_output 
        (match item.unique_stats with |Buff _ -> "buff" |Combat _ -> "combat")
        ~printer:(fun s -> s))

(** [make_empty_inventory_test rm] constructs an OUnit test named [name] that
    asserrs the quality of every element of [rm.items] with 
    [pos <> Inventory i]*)
let make_empty_inventory_test
    (name:string)
    (rm:Room.t) : test =
  name >:: (fun _ ->
      let _ = List.map 
          (fun (i : Item.Item.t) -> 
             assert_equal "position"
               (match i.pos with 
                |Item.Inventory i -> "inventory" 
                |Item.Position p -> "position")
               ~printer:(fun x -> x)
          ) rm.items in ())

(**[make_get_inventory_slot_test rm idx expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [List.length ent.animations]
    with [expected_output] *)
let make_get_inventory_slot_test
    (name:string)
    (rm:Room.t)
    (idx:int)
    (expected_output:string option) : test =
  name >:: (fun _ ->
      assert_equal expected_output 
        (match Room.get_item_slot rm idx with 
         |Some i -> Some (i.e.name)
         |None -> None)
        ~printer: (function |Some s -> s |None -> "none"))

(**[make_vector_to_int_test vec expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.to_int vec]
    with [expected_output] *)
let make_vector_to_int_test
    (name:string)
    (vec:Vector.t)
    (expected_output:Vector.s) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.to_int vec)
        ~printer: (fun (x,y) -> string_of_int x^","^string_of_int y))

(**[make_vector_from_int_test vec expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.from_int vec]
    with [expected_output] *)
let make_vector_from_int_test
    (name:string)
    (vec:Vector.s)
    (expected_output:Vector.t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.from_int vec)
        ~printer: Vector.print)

(**[make_vector_floor_test vec expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.floor vec]
    with [expected_output] *)
let make_vector_floor_test
    (name:string)
    (vec:Vector.t)
    (expected_output:Vector.t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.floor vec)
        ~printer: Vector.print)

(**[make_vector_add_test vec1 vec2 expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.add vec1 vec2]
    with [expected_output] *)
let make_vector_add_test
    (name:string)
    (vec1:Vector.t)
    (vec2:Vector.t)
    (expected_output:Vector.t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.add vec1 vec2)
        ~printer: Vector.print)

(**[make_vector_sub_test vec1 vec2 expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.subtract vec1 vec2]
    with [expected_output] *)
let make_vector_sub_test
    (name:string)
    (vec1:Vector.t)
    (vec2:Vector.t)
    (expected_output:Vector.t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.subtract vec1 vec2)
        ~printer: Vector.print)

(**[make_vector_add_ints_test vec1 vec2 expected_output] constructs an OUnit 
   test named [name] that asserts the quality of [Vector.add_ints vec1 vec2]
    with [expected_output] *)
let make_vector_add_ints_test
    (name:string)
    (vec1:Vector.s)
    (vec2:Vector.s)
    (expected_output:Vector.s) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.add_ints vec1 vec2)
        ~printer: (fun (x,y) -> string_of_int x^","^string_of_int y))

(**[make_vector_add_ints_test vec1 vec2 expected_output] constructs an OUnit 
   test named [name] that asserts the quality of [Vector.add_ints vec1 vec2]
    with [expected_output] *)
let make_vector_sub_ints_test
    (name:string)
    (vec1:Vector.s)
    (vec2:Vector.s)
    (expected_output:Vector.s) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.sub_ints vec1 vec2)
        ~printer: (fun (x,y) -> string_of_int x^","^string_of_int y))

(**[make_vector_greater_test vec1 vec2 expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.greater vec1 vec2]
    with [expected_output] *)
let make_vector_greater_test
    (name:string)
    (vec1:Vector.t)
    (vec2:Vector.t)
    (expected_output:bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.greater vec1 vec2)
        ~printer: string_of_bool)

(**[make_vector_abs_test vec expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.abs_vec vec]
    with [expected_output] *)
let make_vector_abs_test
    (name:string)
    (vec:Vector.t)
    (expected_output:Vector.t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.abs_vec vec)
        ~printer: Vector.print)

(**[make_vector_of_direction_test dir expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.vec_of_dir dir]
    with [expected_output] *)
let make_vector_of_direction_test
    (name:string)
    (dir:Entity.direction)
    (expected_output:Vector.t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.vec_of_dir dir)
        ~printer: Vector.print)

(**[make_direction_of_vector_test vec expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.dir_of_vec vec]
    with [expected_output] *)
let make_direction_of_vector_test
    (name:string)
    (vec:Vector.t)
    (expected_output:Entity.direction) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.dir_of_vec vec)
        ~printer: (function 
            |Entity.Down->"down"
            |Entity.Up->"up"
            |Entity.Right->"right"
            |Entity.Left->"left"))

(**[make_vector_magnitude_test vec expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.magnitude vec]
    with [expected_output] *)
let make_vector_magnitude_test
    (name:string)
    (vec:Vector.t)
    (expected_output:float) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.magnitude vec)
        ~printer: string_of_float)

(**[make_vector_scale_test vec sc expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.scale_vec sc vec]
    with [expected_output] *)
let make_vector_scale_test
    (name:string)
    (vec:Vector.t)
    (sc:float)
    (expected_output:Vector.t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.scale_vec sc vec)
        ~printer: Vector.print)

(**[make_vector_distance_test vec1 vec2 expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.distance vec1 vec2]
    with [expected_output] *)
let make_vector_distance_test
    (name:string)
    (vec1:Vector.t)
    (vec2:Vector.t)
    (expected_output:float) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.distance vec1 vec2)
        ~printer: string_of_float)

(**[make_vector_center_test vec1 vec2 expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.center vec1 vec2]
    with [expected_output] *)
let make_vector_center_test
    (name:string)
    (vec1:Vector.t)
    (vec2:Vector.t)
    (expected_output:Vector.t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.center vec1 vec2)
        ~printer: Vector.print)

(**[make_vector_print_test vec expected_output] constructs an OUnit test 
   named [name] that asserts the quality of [Vector.print vec]
    with [expected_output] *)
let make_vector_print_test
    (name:string)
    (vec:Vector.t)
    (expected_output:string) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Vector.print vec)
        ~printer: (fun x -> x))

(** [make_pixel_collision_test name pos1 size1 pos2 size2 expected_ouput]
    constructs an OUnit test named [name] that asserts the quality of 
    [Window.collision pos1 size1 pos2 size2] with [expected_output]*)
let make_pixel_collision_test
    (name:string)
    (pos1:Vector.s)
    (size1:Vector.s)
    (pos2:Vector.s)
    (size2:Vector.s)
    (expected_output:bool): test =
  name >:: (fun _ -> 
      assert_equal expected_output (Window.collision pos1 size1 pos2 size2)
        ~printer:string_of_bool)

(** [make_entity_collision_test name rm e ignore expected_ouput]
    constructs an OUnit test named [name] that asserts the quality of 
    [Room.collisions_with_entity rm e ignore] with [expected_output]*)
let make_entity_collision_test
    (name:string)
    (rm:Room.t)
    (e:Entity.e)
    (ignore:Entity.e)
    (expected_output:string list): test =
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Room.collisions_with_entity rm e ignore
         |> List.map (function 
             |Room.CWall _ -> "wall"
             |Room.CEnemy _ -> "enemy"
             |Room.CExit _ -> "exit"
             |Room.CItem _ ->"item"
             |Room.CPlayer _->"player")
         |> List.sort_uniq (String.compare))
        ~printer:(String.concat ";"))

(** [make_room_seed_test name rm expected_ouput] constructs an OUnit test named
    [name] that asserts the quality of [rm.seed] with [expected_output]*)
let make_room_seed_test
    (name:string)
    (rm:Room.t)
    (expected_output:int): test =
  name >:: (fun _ -> 
      assert_equal expected_output rm.seed
        ~printer:string_of_int)

(** [make_description_test_fail name adv room expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with
    [description adv room]
*)
(* let make_description_test_fail
    (name:string)
    (adv:Adventure.t)
    (room : string)
    (expected_output:exn) : test =
   name >:: (fun _ ->
      assert_raises expected_output (fun () -> description adv room)) *)

(********************************************************************
   End helper functions.
 ********************************************************************)

let window = 
  Window.create_window "Test Suite" 
    (GameVars.width * (int_of_float GameVars.tile_size)) 
    (GameVars.height * (int_of_float GameVars.tile_size))

let player = Player.make_player "link" 0 window  6. 7. |> ref
let voltorb = Enemy.make_enemy 0 0 window 5. 7. 5. |> ref
let red_frog = Enemy.make_enemy 0 0 window 0. 1. 5. |> ref
let blue_rupee = Item.make_item 0 0 window 0. 0. |> ref
let sword = Item.make_item 0 0 window 0. 0. |> ref

let tiles = Room.Floor(
    Window.get_renderer window
    |> Animations.load_image "./sprites/room/floor.bmp")
            |> Array.make 10
            |> Array.make 10
let wall_row = Room.Wall (Animations.load_image "./sprites/room/wall.bmp" 
                            (Window.get_renderer window))
               |> Array.make 10

let () = begin tiles.(0) <- wall_row; tiles.(9) <- wall_row end

let room = ref Room.({
    seed = 12345678;
    player = !player;
    enemies = [!voltorb;!red_frog];
    items = [!blue_rupee;!sword];
    tiles = tiles
  })

let () = 
  while !voltorb.e.name <> "voltorb" do
    voltorb := Enemy.make_enemy 0 (!voltorb.id + 1) window 5. 7. 5.
  done;
  while !red_frog.e.name <> "red-frog" do
    red_frog := Enemy.make_enemy 0 (!red_frog.id + 1) window 0. 1. 5.
  done;
  while !blue_rupee.e.name <> "blue-rupee" do
    blue_rupee := Item.make_item 0 (!blue_rupee.id + 1) window 0. 0.
  done;
  while !sword.e.name <> "sword" do
    sword := Item.make_item 0 (!sword.id + 1) window 0. 0.
  done

let initialize_tests =
  [
    make_player_id_test "Initial Player ID" !player 0;
    make_player_state_test "Initial Player State" !player Player.Idle;
    make_entity_name_test "Player Name link" !player.e "link";
    make_entity_direction_test "Initial Player Direction" !player.e Entity.Down;
    make_entity_pos_test "Initial Player Position" !player.e (6.,7.);
    make_entity_bbox_test "Player Bounding Box Loads" !player.e ((12, 12),(2, 4));
    make_entity_num_anims_test "Number of Player Animations" !player.e 20;
    make_player_health_test "Player Initial Health" !player 100;
    make_player_max_health_test "Player Initial Max Health" !player 100;
    make_player_attack_test "Player Initial Attack" !player 10.;
    make_player_speed_test "Player Initial Attack" !player 10;

    make_entity_name_test "Red Frog Name red-frog" !red_frog.e "red-frog";
    make_entity_name_test "Voltorb Name voltorb" !voltorb.e "voltorb";
    make_entity_bbox_test "Red Frog BBox" !red_frog.e ((10,10),(3,3));
    make_entity_bbox_test "Voltorb BBox" !voltorb.e ((14,14),(1,1));
    make_entity_num_anims_test "Number of Enemy Animations" !red_frog.e 12;

    make_entity_name_test "Blue Rupee Name blue-rupee" !blue_rupee.e "blue-rupee";
    make_entity_name_test "Sword Name sword" !sword.e "sword";
    make_entity_bbox_test "Blue Rupee BBox" !blue_rupee.e ((6, 15),(4,1));
    make_entity_bbox_test "Sword BBox" !sword.e ((7, 13), (4,2));
    make_entity_num_anims_test "Number of Rupee Animations" !blue_rupee.e 1;
    make_entity_num_anims_test "Number of Sword Animations" !sword.e 5;
    make_item_type_test "Blue Rupee is Buff" !blue_rupee "buff";
    make_item_type_test "Sword is Combat" !sword "combat";

    make_empty_inventory_test "Initial Room, Nothing in Inventory" !room;
  ]

let vec1x,vec1y = (3.7,4.2)
let vec2x,vec2y = (1.3,4.5)
let int1x,int1y = (3,4)
let int2x,int2y = (1,4)

let vector_tests =
  [
    make_vector_to_int_test "(3.7, 4.2) -> (3,4)" (vec1x,vec1y) (3,4);
    make_vector_from_int_test "(3,4) -> (3.,4.)" (3,4) (3.,4.);
    make_vector_floor_test "(3.7, 4.2) -> (3.,4.)" (vec1x,vec1y) (3.,4.);
    make_vector_add_test 
      "(3.7,4.2)+(1.3,4.5)->(5.,8.7)" 
      (vec1x,vec1y) (vec2x,vec2y) 
      (vec1x+.vec2x,vec1y+.vec2y);
    make_vector_add_test 
      "(3.7,4.2)+(-1.3,-4.5)->(2.4,-0.3)" 
      (vec1x, vec1y) (0.-.vec2x,0.-.vec2y) 
      (vec1x-.vec2x,vec1y-.vec2y);
    make_vector_sub_test
      "(3.7,4.2)-(1.3,4.5)->(2.4,-0.3)"
      (vec1x, vec1y) (vec2x,vec2y) 
      (vec1x-.vec2x,vec1y-.vec2y);
    make_vector_sub_test 
      "(3.7,4.2)-(-1.3,-4.5)->(5.,8.7)" 
      (vec1x,vec1y) (0.-.vec2x,0.-.vec2y) 
      (vec1x+.vec2x,vec1y+.vec2y);
    make_vector_add_ints_test 
      "(3,4)+(1,4)->(4,8)" 
      (int1x,int1y) (int2x,int2y) 
      (int1x+int2x,int1y+int2y);
    make_vector_add_ints_test 
      "(3,4)+(-1,-4)->(2,0)" 
      (int1x, int1y) (0-int2x,0-int2y) 
      (int1x-int2x,int1y-int2y);
    make_vector_sub_ints_test
      "(3,4)-(1,4)->(2,0)"
      (int1x, int1y) (int2x,int2y) 
      (int1x-int2x,int1y-int2y);
    make_vector_sub_ints_test 
      "(3,4)-(-1,-4)->(4,8)" 
      (int1x,int1y) (0-int2x,0-int2y) 
      (int1x+int2x,int1y+int2y);
    make_vector_greater_test 
      "(3.7,4.2) (1.3,4.5) False" (vec1x,vec1y) (vec2x,vec2y) false;
    make_vector_greater_test 
      "(1.3,4.5) (3.7,4.2) False" (vec2x,vec2y) (vec1x,vec1y) false;
    make_vector_greater_test 
      "(3.7.3,4.5) (1.3,4.2) True" (vec1x,vec2y) (vec2x,vec1y) true;
    make_vector_abs_test 
      "|(-3.7,-4.2)| = (3.7,4.2)" (0.-.vec1x,0.-.vec2x) (vec1x,vec2x);
    make_vector_abs_test 
      "|(-3.7,4.2)| = (3.7,4.2)" (0.-.vec1x,vec2x) (vec1x,vec2x);
    make_vector_of_direction_test "Down -> (0, 1.0)" Down (0.,1.);
    make_vector_of_direction_test "Up -> (0, -1.0)" Up (0.,-1.);
    make_vector_of_direction_test "Left -> (-1.0, 0)" Left (-1.,0.);
    make_vector_of_direction_test "Down -> (1.0, 0)" Right (1.,0.);
    make_direction_of_vector_test "(3.7,4.2) -> Down" (vec1x,vec1y) Down;
    make_direction_of_vector_test "(3.7,-4.2) -> Down" (vec1x,0.-.vec1y) Up;
    make_direction_of_vector_test "(4.2,3.7) -> Right" (vec1y, vec1x) Right;
    make_direction_of_vector_test "(-4,2,3.7) -> Down" (0.-.vec1y,vec1x) Left;
    make_vector_magnitude_test "(3,4) -> 5" (3.,4.) 5.;
    make_vector_magnitude_test "(0,0) -> 0" (0.,0.) 0.;
    make_vector_scale_test 
      "(3.7,4.2)*2.3 -> (8.51,9.66)" (vec1x,vec1y) 2.3 (vec1x*.2.3, vec1y*.2.3);
    make_vector_distance_test "(0,0) -> (3,4) -> 5" (0.,0.) (3.,4.) 5.;
    make_vector_center_test 
      "Center at Origin, (3.7,4.2)" (0.,0.) (vec1x,vec1y) 
      (GameVars.hrad +. vec1x,GameVars.vrad +. vec1y);
    make_vector_center_test 
      "Center at hrad,vrad, (3.7,4.2)" (GameVars.hrad,GameVars.vrad) 
      (vec1x,vec1y) (vec1x,vec1y);
  ]

let collision_tests = 
  [
    make_entity_collision_test 
      "Player and Voltorb" !room 
      {!player.e with pos = Vector.subtract !player.e.pos (1.,0.)} !player.e 
      ["enemy"];
    make_entity_collision_test 
      "Voltorb and Player" !room 
      {!voltorb.e with pos = Vector.add !voltorb.e.pos (1.,0.)} !voltorb.e 
      ["player"];
    make_entity_collision_test
      "Frog and Sword and Wall" !room
      {!red_frog.e with pos = Vector.subtract !red_frog.e.pos (0.,1.)} !red_frog.e
      ["item";"wall"];
    make_entity_collision_test
      "Sword and Frog and Wall" !room
      {!sword.e with pos = Vector.add !sword.e.pos (0.,0.5)} !sword.e
      ["enemy";"wall"];
  ]

let () = begin 
  player := {!player with health = 50; max_health = 150; 
                          unique_stats = {attack = 999.; movement_speed = 150}};
  blue_rupee := {!blue_rupee with pos = Inventory{index = 0}};
  sword := {!sword with pos = Inventory{index = 1}};
  room := {!room with player = !player; items = [!blue_rupee;!sword]}
end

let inventory_tests =
  [
    Some "blue-rupee" |> make_get_inventory_slot_test "Rupee Slot 0" !room 0;
    Some "sword" |> make_get_inventory_slot_test "Sword Slot 1" !room 1;
    None |> make_get_inventory_slot_test "None Slot 3" !room 3;
  ]

let () = Save.save !room "testsave"
let load_room = Load.load "testsave.json" window

let save_load_tests =
  [
    Some "blue-rupee" |> make_get_inventory_slot_test "Rupee Slot 0" load_room 0;
    Some "sword" |> make_get_inventory_slot_test "Sword Slot 1" load_room 1;
    None |> make_get_inventory_slot_test "None Slot 3" load_room 3;
    make_player_health_test "Player Loaded Health" load_room.player 50;
    make_player_max_health_test "Player Loaded Health" load_room.player 150;
    make_player_attack_test "Player Loaded Attack" load_room.player 999.;
    make_player_speed_test "Player Loaded Speed" load_room.player 150;
    make_room_seed_test "Loaded Seed" load_room 12345678;
  ] 

let suite =
  "test suite for A2"  >::: List.flatten [
    initialize_tests;
    inventory_tests;
    vector_tests;
    collision_tests;
    save_load_tests;
  ]

let _ = run_test_tt_main suite; exit 0