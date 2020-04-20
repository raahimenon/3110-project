open OUnit2

let test_img = Animations.load_image "test.txt"

(**[make_load_image_test name im expected_output] constructs an OUnit test
   named [name] that asserts the quality of [expected_output] with 
   [im_to_str im]*)
let make_load_image_test
    (name:string)
    (im:Graphics.color array array)
    (expected_output:string) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (Animations.im_to_str im)
        ~printer: (fun x -> x)
    )


(*
  (**[make_anims_from_direction_test name obj dir expected_output] constructs an
   OUnit test named [name] that asserts the quality of [expected_output] with
   the names of [anims_from_direction obj dir]*)
  let make_anims_from_direction_test
    (name : string)
    (obj : string)
    (dir : string)
    (expected_output : string array) = 
  name >:: (fun _ -> 
      assert_equal 
        expected_output
        (Animations.anims_from_direction obj dir |> (Array.map Animations.name))
        ~printer: (fun x -> "[|"^String.concat ";" (x |> Array.to_list) ^"|]");
    )

  (** [make_num_frames_test name obj dir anim_name expected_output] constructs an
    OUnit test named [name] that asserts the quality of [expected_output] with
    the number of frames of the animation named [anim_name] in the array 
    [anims_from_direction obj dir]**)
  let make_num_frames_test
    (name : string)
    (obj : string)
    (dir : string)
    (anim_name : string)
    (expected_output : int) =
  let anim = Animations.anims_from_direction obj dir 
             |> Array.to_list
             |> List.filter (fun a -> String.equal anim_name (Animations.name a)) in
  name >:: (fun _ ->
      assert_equal expected_output (match anim with [] -> 0 |h::k -> Array.length (Animations.frames h))
        ~printer: (string_of_int)
    )
*)

(** [make_playing_animation_test name obj dir expected_output*)

let load_tests = [
  make_load_image_test
    "test.txt base load test"
    test_img
    "0,0,0,0 0,255,255,255\n255,0,255,255 255,255,0,255"
]

(*
  let anim_tests = [
  make_anims_from_direction_test
    "trainer down"
    "trainer"
    "down"
    [|"walk"|];
  make_num_frames_test
    "trainer down walk 2"
    "trainer"
    "down"
    "walk"
    2;
  ]

  let gameobject_tests = [


  ]
*)

let suite = 
  "test suite for my project" >::: List.flatten [
    load_tests;
  ]

let _ = run_test_tt_main suite