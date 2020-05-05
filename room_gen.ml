(** [reflect_sample sample] returns [sample] reflected over the x 
    axis. Reflections over the y axis can be achieved by rotating the sample
    180 degrees before calling this function. *)
let reflect_sample_x (sample : Room.tile array array)
  : Room.tile array array =
  sample |> Array.mapi (fun i row -> 
      row |> Array.mapi (fun j elem -> 
          sample.((Array.length sample) - 1 - i).(j)))

(** [rotate_sample_90 sample] returns the result of rotating [sample] 90
    degrees clockwise. Rotation is achieved by reflecting the array across
    the x axis, then transposing. Assumes square samples. *)
let rotate_sample_90 (sample : Room.tile array array) : Room.tile array array =
  sample |> reflect_sample_x |> Array.mapi (fun i row ->
      row |> Array.mapi (fun j elem ->
          sample.(j).(i)))


(** Generate all possible samples based on inputs. Include rotations and
    reflections if desired (may significantly decrease generator efficiency). *)
let sample_space (input : Room.tile array array) (sample_dim : int)
    (rotations_on : bool) (reflections_on : bool)
  : Room.tile array array array = 
  (* Create base sample space *)
  Array.init (Array.length input - sample_dim + 1)
    (fun i -> Array.init (Array.length input.(0) - sample_dim + 1)
        (fun j -> Array.init sample_dim
            (fun n -> Array.init sample_dim
                (fun m -> input.(i+n).(j+m)))))

  (* Flatten top level of array *)
  |> Array.to_list |> Array.concat

  (* Conditional addition of reflected samples *)
  |> (if reflections_on
      then
        (fun s -> 
           Array.concat [s; (Array.map reflect_sample_x s);
                         (Array.map (fun e ->
                              e |> rotate_sample_90 |> rotate_sample_90
                              |> reflect_sample_x) s)])
      else (fun s -> s))

  (* Conditional addition of rotated samples *)
  |> (if rotations_on 
      then (fun s ->
          let rot90 = Array.map rotate_sample_90 s
          in let rot180 = Array.map rotate_sample_90 rot90
          in let rot270 = Array.map rotate_sample_90 rot180
          in Array.concat [s; rot90; rot180; rot270])
      else (fun s -> s))

(** [get_entropy elem] returns the number of possibilities for a given element 
    in the wave *)
let get_entropy (elem : bool array) : int =
  Array.fold_left (fun prev b -> prev + Bool.to_int b) 0 elem

(** [get_indices condition a] returns the indices of all elements in [a] that 
    satisfy [condition]. *)
let get_indices (condition : 'a -> bool) (a : 'a array) : int array =
  a |> Array.mapi (fun i elem -> if condition elem then Some i else None)
  |> Array.to_list |> List.filter_map (fun o -> o) |> Array.of_list

(* Collapses the wave function. Raises [Failure] if collapse leads to a 
   contradictory state. *)
let iters = ref 0
let rec collapse_loop (samples : Room.tile array array array)
    (wave : bool array array array) (output : Room.tile option array array)
    (seed : int) (minimum_entropy : int) : Room.tile array array =
  (* Initialize RNG TODO: does this need to be done in-function? *)
  Random.init seed;

  (* Randomly select a minimum entropy wave section and collapse it *)
  (* Identify potential wave sections *)
  let candidate_indices =
    wave |> Array.to_list |> List.map Array.to_list |> List.flatten
    |> Array.of_list |> get_indices
      (fun elem -> get_entropy elem = minimum_entropy) |> Array.map
      (fun index -> [|(index / Array.length wave);
                      (index mod Array.length wave)|])
  in

  print_endline "get random target";
  (* Randomly select a wave section *)
  let target_coords =
    Array.get candidate_indices (Random.int (Array.length candidate_indices))
  in
  (* Collapse the selected section to a randomly selected sample *)
  (* Identify potential samples *)
  let sample_indices =
    wave.(target_coords.(0)).(target_coords.(1)) |> get_indices (fun s -> s)
  in

  print_endline "get random sample";
  (* Randomly select a sample *)
  let sample_index = 
    Array.get sample_indices (Random.int (Array.length sample_indices))
  in

  (* Collapse *)
  wave.(target_coords.(0)).(target_coords.(1))
  <- Array.mapi (fun i e -> if i = sample_index then true else false) samples;

  (* Propogate the collapse to output *)
  for i = 0 to Array.length samples.(sample_index) - 1 do
    for j = 0 to Array.length samples.(sample_index).(0) - 1 do
      (* Sanity check. Verifies that the collapse is either agreeing with 
         existing tiles, or ovewriting [None] tiles. TODO: remove *)
      (match output.(target_coords.(0) + i).(target_coords.(1) + j) with
       | None -> ()
       | Some t when t = samples.(sample_index).(i).(j) -> ()
       | Some other -> failwith "sanity check failed at room_gen line 110");

      output.(target_coords.(0) + i).(target_coords.(1) + j)
      <- Some samples.(sample_index).(i).(j)
    done
  done;


  (* Recalculate local wave and minimum entropy *)
  let new_min_ent = ref minimum_entropy in
  (* Adjust local wave possibilities *)
  (* Iterate across potentially affected area *)
  for i = max 0
        (target_coords.(0) - (Array.length samples.(0)) + 1)
    to min (Array.length wave - 1)
        (target_coords.(0) + (Array.length samples.(0)) - 1) do
    for j = max 0
          (target_coords.(1) - (Array.length samples.(0).(0)) + 1)
      to min (Array.length wave.(0) - 1)
          (target_coords.(1) + (Array.length samples.(0).(0)) - 1) do
      (* Iterate across each sample *)
      (* Check that sample is viable by comparing each predicted tile with
         partially collapsed output *)
      (* FLAG POTENTIALLY FIXED AREA?? *)
      wave.(i).(j)
      <- (samples |> Array.map
            (fun s -> let viable = ref true in
              for n = 0 to Array.length s - 1 do
                for m = 0 to Array.length s.(n) - 1 do
                  match output.(i + n).(j + m) with
                  | Some t when t <> s.(n).(m) -> viable := false
                  | other -> ()
                done;
              done;
              !viable););
      (* Check for a new minimum entropy *)
      (if output.(i).(j) = None
       then new_min_ent := min !new_min_ent (get_entropy wave.(i).(j))
       else ());
    done;
  done;

  (* Fail if any entropy value reaches 0, indicating contradiction *)
  if !new_min_ent = 0 then failwith "contradiction" else ();

  (*TODO: Remove debug prints bundle *)
  let temp = ref 0 in
  for i = 0 to Array.length output - 1 do
    for j = 0 to Array.length output.(0) - 1 do
      match output.(i).(j) with
      | None -> ()
      | Some t -> temp := !temp + 1
    done;
  done;
  iters := !iters + 1;
  print_float (Sys.time ());
  print_string ("\t" ^ (string_of_int !iters) ^ " / " ^ (string_of_int ((Array.length output) * (Array.length output.(0)))
                                                         ^ "\t" ^ ": "));
  print_int !temp; print_string (" " ^ (string_of_bool (Array.exists (Array.mem None) output)) ^ " min_ent: ");
  print_int !new_min_ent;
  print_endline "";

  (* Check if further repetitions are required *)
  if (Array.exists (Array.mem None) output)
  (* If incomplete, keep going *)
  then (collapse_loop samples wave output seed !new_min_ent)
  (* If complete, unbind tiles from options and return *)
  else output |> Array.map
         (fun row -> row |> Array.to_list |> List.filter_map
                       (fun o -> o) |> Array.of_list)

(** Central method. Too sleepy to document. Haven't even test it yet. *)
let generate_room (seed : int) (input : Room.tile array array)
    (sample_dim : int) (output_rows : int) (output_cols : int)
    (difficulty : int) (window : Window.window) : Room.t =
  (* Validate inputs *)
  if (sample_dim > Array.length input)
  || (sample_dim > Array.length input.(0))
  || (sample_dim > output_rows)
  || (sample_dim > output_cols)
  (* TODO: validate difficulty *)
  then failwith "invalid inputs at generate_room"
  else ();

  (* Complexity switches *)
  let rotations_on = true in
  let reflections_on = true in
  let attempts = 100 in
  let time_cap = Sys.time () +. 100. in

  (* Generate sample space list *)
  let samples = sample_space input sample_dim rotations_on reflections_on in
  print_endline ("\nsample count: "^(string_of_int (Array.length samples)));

  (* 3D boolean array represents the wave *)
  let wave = Array.make (output_rows - sample_dim + 1)
      (Array.make (output_cols - sample_dim + 1) 
         (Array.make (Array.length samples) true)) in

  (* Empty tile array for the final layout *)
  let output = Array.make output_rows (Array.make output_cols None) in

  (* Generative loop *)
  let tiles : Room.tile array array ref = ref [||] in
  print_string ("initial tiles array length: "^(string_of_int (Array.length !tiles)^"\n"));
  for attempt = 1 to attempts do
    if (Array.length !tiles = 0)
    then
      ((* Make an attempt *)
        print_endline "attempt";
        let w_cop = wave |> Array.map (Array.map Array.copy) in
        let o_cop = output |> Array.map Array.copy in
        try tiles := collapse_loop (samples) (w_cop) (o_cop) (seed) (Array.length samples)
        with Failure f -> print_string ("Attempt " ^ (string_of_int attempt)
                                        ^ " failed.\n");

          (* Ensure limits have not been exceeded *)
          if (Sys.time () > time_cap || attempt = attempts)
          then failwith ("Timed out after attempt " ^ (string_of_int attempt)
                         ^ " and elapsed time "
                         ^ (string_of_float (Sys.time ()-. time_cap +. 1000.)))
          else ())
    else ();
  done;
  print_endline "success!!!";

  (* TODO: Clean floating rooms *)


  (* TODO: Place entrance and exit *)


  (* TODO: Place enemies *)
  let enemies = [] in

  (* TODO: Place items *)
  (* TODO: respond to enemy density and/or entrance/exit distance *)
  let items = [] in

  {
    player = Player.make_player "link" 0 window;
    enemies = enemies;
    items = items;
    tiles = !tiles;
  }

let simple_gen (seed : int) (window : Window.window): Room.t =
  let f = Room.Floor (Animations.load_image "./sprites/room/floor.bmp" (Window.get_renderer window)) in
  let w = Room.Wall (Animations.load_image "./sprites/room/wall.bmp" (Window.get_renderer window)) in
  let input =
    [|
      [| w ; f ; w ; w |];
      [| w ; f ; w ; w |];
      [| w ; f ; f ; f |];
      [| w ; f ; f ; f |];
    |]
  in
  generate_room (seed) (input) (1) (10) (10) (0) (window)
  |> (fun room -> 
      print_int (Array.length room.tiles); print_endline "";
      print_int (Array.length room.tiles.(0)); print_endline "";

      (room.tiles |> Array.iter
         (fun row ->
            row |> Array.iter
              (fun elem ->
                 match elem with
                 | Room.Floor a -> print_string "___"
                 | Room.Wall a -> print_string "WWW"
                 | other -> print_string "???");
            print_string "\n")); room)