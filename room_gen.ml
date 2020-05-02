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
let rec collapse_loop (samples : Room.tile array array array)
    (wave : bool array array array) (output : Room.tile option array array)
    (seed : int) (minimum_entropy : int) : Room.tile array array =
  (* Initialize RNG TODO: does this need to be done in-function? *)
  Random.init seed;
  print_endline "1";
  (* Randomly select a minimum entropy wave section and collapse it *)
  (* Identify potential wave sections *)
  let candidate_indices =
    wave |> Array.to_list |> List.map Array.to_list |> List.flatten
    |> Array.of_list |> get_indices
      (fun elem -> get_entropy elem = minimum_entropy) |> Array.map
      (fun index -> [|(index / Array.length wave);
                      (index mod Array.length wave)|])
  in
  print_endline "2";
  (* Randomly select a wave section *)
  let target_coords =
    Array.get candidate_indices (Random.int (Array.length candidate_indices))
  in
  print_endline "2.5";
  (* Collapse the selected section to a randomly selected sample *)
  (* Identify potential samples *)
  let sample_indices =
    wave.(target_coords.(0)).(target_coords.(1)) |> get_indices (fun s -> s)
  in

  (* Randomly select a sample *)
  let sample_index = 
    Array.get sample_indices (Random.int (Array.length sample_indices))
  in
  print_endline "3";
  (* Collapse *)
  wave.(target_coords.(0)).(target_coords.(1))
  <- Array.mapi (fun i e -> if i = sample_index then true else false) samples;

  (* Propogate the collapse to output *)
  for i = 0 to Array.length samples.(sample_index) - 1 do
    for j = 0 to Array.length samples.(sample_index).(0) - 1 do
      output.(target_coords.(0) + i).(target_coords.(1) + j)
      <- Some samples.(sample_index).(i).(j)
    done
  done;
  print_endline "4";
  (* Recalculate local wave and minimum entropy *)
  let new_min_ent = ref minimum_entropy in
  (* Adjust local wave possibilities *)
  print_endline "entering nest";
  for i = max 0 (target_coords.(0) - (Array.length samples.(0)) + 1)
    to min (Array.length wave - 1)
        (target_coords.(0) + (2 * (Array.length samples.(0))) - 1) do
    print_endline "l1";
    for j = max 0 (target_coords.(1) - (Array.length samples.(0).(0)) + 1)
      to min (Array.length wave.(0) - 1)
          (target_coords.(1) + (2 * (Array.length samples.(0).(0))) - 1) do
      (* Iterate across samples *) print_endline "l2";
      for index = 0 to Array.length samples - 1 do
        (* Check that sample is viable by comparing each predicted tile with
           partially collapsed output *) print_endline "l3";
        wave.(i).(j).(index)
        <- samples |> Array.for_all
             (fun s -> let viable = ref true in
               for n = 0 to Array.length s - 1 do
                 print_endline "l4";
                 for m = 0 to Array.length s.(n) - 1 do
                   print_endline "l5";
                   match output.(i + n).(j + m) with
                   | Some t when t <> s.(n).(m) -> viable := false
                   | other -> ()
                            ;print_endline "new in4 value";
                 done;
                 print_endline "new in3 value";
               done;
               !viable);
        print_endline "new in2 value";
      done;
      (* Check for a new minimum entropy *)
      new_min_ent := min !new_min_ent (get_entropy wave.(i).(j));
      if !new_min_ent = 0
      then Array.iter (fun b -> b |> string_of_bool |> (print_string " "; print_string)) wave.(i).(j)
      else
        print_endline "new in1 value";
    done;
    print_endline "new outer value";
  done;
  print_endline "5";

  (* Fail if any entropy value reaches 0, indicating contradiction *)
  (*if !new_min_ent = 0 then failwith "contradiction" else*)
  (* For the moment, returns partial floor. TODO: restore original *)
  let output = if !new_min_ent = 0
    then output |> Array.map
           (fun row ->
              row |> Array.map
                (fun opt -> match opt with
                   | Some t -> Some t
                   | None ->   let window = Window.create_window "3110 Project" (GameVars.width * (int_of_float GameVars.tile_size)) (GameVars.height * (int_of_float GameVars.tile_size)) in
                     Some (Room.Floor (Animations.load_image "./sprites/room/floor.bmp" (Window.get_renderer window)))))
    else output
  in

  (* Check if further repetitions are required *)
  if not (Array.exists (Array.mem None) output)
  (* If complete, unbind tiles from options and return *)
  then output |> Array.map
         (fun row -> row |> Array.to_list |> List.filter_map
                       (fun o -> o) |> Array.of_list)
         (* If incomplete, keep going *)
  else collapse_loop samples wave output seed !new_min_ent

(** Central method. Too sleepy to document. Haven't even test it yet. *)
let generate_room (seed : int) (input : Room.tile array array)
    (sample_dim : int) (output_rows : int) (output_cols : int)
    (difficulty : int) : Room.t =
  (* Validate inputs *)
  if (sample_dim > Array.length input)
  || (sample_dim > Array.length input.(0))
  || (sample_dim > output_rows)
  || (sample_dim > output_cols)
  (* TODO: validate difficulty *)
  then (
    (* TODO: get rid of this debugging junk *)
    (*(List.iter (fun b -> print_string ("\n" ^ (Bool.to_string b) ^ "\n"))[(sample_dim > Array.length input)
                                                                          ; (sample_dim > Array.length input.(0))
                                                                          ; (sample_dim > output_rows)
                                                                          ; (sample_dim > output_cols)]);*)
    failwith "invalid inputs at generate_room")
  else
    print_endline "6";
  (* Complexity switches *)
  let rotations_on = true in
  let reflections_on = true in
  let attempts = 5 in
  let time_cap = Sys.time () +. 10. in

  (* Generate sample space list *)
  let samples = sample_space input sample_dim rotations_on reflections_on in

  (* 3D boolean array represents the wave *)
  let wave = Array.make (output_rows - sample_dim + 1)
      (Array.make (output_cols - sample_dim + 1) 
         (Array.make (Array.length samples) true)) in
  print_endline "7";
  (* Empty tile array for the final layout *)
  let output = Array.make output_rows (Array.make output_cols None) in
  print_endline "8";
  (* Generative loop *)
  let tiles : Room.tile array array ref = ref [||] in
  for attempt = 1 to attempts do
    (* Make an attempt *)
    try tiles := collapse_loop samples wave output seed (Array.length samples)
    with Failure f -> print_string ("Attempt " ^ (string_of_int attempt)
                                    ^ " failed.");

      (* Ensure limits have not been exceeded *)
      if (Sys.time () > time_cap || attempt = attempts)
      then failwith ("Timed out after \t attempt " ^ (string_of_int attempt)
                     ^ " \n \t \t \t and elapsed time "
                     ^ (string_of_float (Sys.time ()-. time_cap +. 1000.)))
      else ()
  done;
  print_endline "9";
  (* TODO: Clean floating rooms *)


  (* TODO: Place entrance and exit *)


  (* TODO: Place enemies *)
  let enemies = [] in

  (* TODO: Place items *)
  (* TODO: respond to enemy density and/or entrance/exit distance *)
  let items = [] in

  {
    (Load.load "0.json"
       (Window.create_window "3110 Project"
          (GameVars.width * (int_of_float GameVars.tile_size))
          (GameVars.height * (int_of_float GameVars.tile_size)))) with
    enemies = enemies;
    items = items;
    tiles = !tiles;
  }