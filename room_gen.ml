let ft_ref = ref [||]	
(* [sample_viable output i j s] checks if the sample [s] could be placed in 	
   [output] at coordinates [(i, j)]. *)	
let sample_viable (output : Room.tile option array array) (i : int) (j : int)	
    (s : Room.tile array array) : bool =	
  let viable = ref true in	
  for n = 0 to Array.length s - 1 do	
    for m = 0 to Array.length s.(n) - 1 do	
      match output.(i + n).(j + m) with	
      | Some t when t <> s.(n).(m) -> viable := false	
      | other -> ()	
    done;	
  done;	
  !viable

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
          let rot90 = Array.map rotate_sample_90 s in
          let rot180 = Array.map rotate_sample_90 rot90 in
          let rot270 = Array.map rotate_sample_90 rot180 in
          Array.concat [s; rot90; rot180; rot270])
      else (fun s -> s))

let weight_ref : int array ref = ref [||]
let noisy_target_coords : int array = [|0; 0|]

(** [get_entropy elem] returns the number of possibilities for a given element 
    in the wave *)
let get_entropy (elem : bool array) : float =
  let total_pos =
    Array.fold_left (fun prev b -> prev +. (Bool.to_float b)) 0. elem
  in
  let total_SE = ref 0. in
  Array.iter2
    (fun valid weight ->
       if (not valid)
       then ()
       else (let p = (float_of_int weight)
                     /. total_pos in
             total_SE := !total_SE
                         -. (p *. (log p) /. (log 2.) /. (float_of_int weight))))
    elem !weight_ref;

  if (Array.mem true elem) then !total_SE +. Random.float 1e-6 else -1.

(** [get_indices condition a] returns the indices of all elements in array 
    [a] that satisfy [condition]. *)
let get_indices (condition : 'a -> bool) (a : 'a array) : int array =
  a |> Array.mapi (fun i elem -> if condition elem then Some i else None)
  |> Array.to_list |> List.filter_map (fun o -> o) |> Array.of_list

(** [get_indices2 condition a] returns the indices of all elements in matrix 
    [a] that satisfy [condition]. *)
let get_indices2 (condition : 'a -> bool) (a : 'a array array)
  : int array array =
  let temp = Queue.create () in

  for i = 0 to (Array.length a - 1) do
    for j = 0 to (Array.length a.(i) - 1) do
      if (condition a.(i).(j))
      then (Queue.add [|i; j|] temp)
      else ()
    done;
  done;

  Array.init (Queue.length temp) (fun i -> Queue.pop temp)

let iters = ref 0

let count_instances (collection : 'a array) (elem : 'a) : int =
  let count = ref 0 in
  for i = 0 to Array.length collection - 1 do
    if collection.(i) = elem then count := !count + 1 else ()
  done;
  !count

let rec collapse_loop (samples : Room.tile array array array)
    (wave : bool array array array) (output : Room.tile option array array)
    (seed : int) (minimum_entropy : float) : Room.tile array array =
  (* Initialize RNG TODO: does this need to be done in-function? *)
  Random.init seed;

  (* Get possible samples *)
  let sample_indices =
    wave.(noisy_target_coords.(0)).(noisy_target_coords.(1)) |> get_indices (fun s -> s)
  in

  print_endline ("get random sample of " ^ (string_of_int (Array.length sample_indices)));
  (* Randomly select a sample *)
  let sample_index = 
    Array.get sample_indices (Random.int (Array.length sample_indices))
  in

  (* Propogate the collapse to output *)
  for i = 0 to Array.length samples.(sample_index) - 1 do
    for j = 0 to Array.length samples.(sample_index).(i) - 1 do
      output.(noisy_target_coords.(0) + i).(noisy_target_coords.(1) + j)
      <- Some samples.(sample_index).(i).(j)
    done
  done;

  (* Adjust wave possibilities *)
  for i = max 0 (noisy_target_coords.(0) - (Array.length samples.(0)) + 1)
    to min (Array.length wave - 1)
        (noisy_target_coords.(0) + (Array.length samples.(0)) - 1) do
    for j = max 0 (noisy_target_coords.(1) - (Array.length samples.(0).(0)) + 1)
      to min (Array.length wave.(0) - 1)
          (noisy_target_coords.(1) + (Array.length samples.(0).(0)) - 1) do
      (* Iterate across each sample *)
      wave.(i).(j)
      <- (samples |> Array.map
            (* Check that sample is viable by comparing each predicted tile 
               with partially collapsed output *)
            (sample_viable output i j));
      if (Array.mem true wave.(i).(j)) then () else (
        print_endline ""; 
        for n = 0 to Array.length samples.(0) - 1 do
          for m = 0 to Array.length samples.(0).(0) - 1 do
            match output.(i + n).(j + m) with
            | Some Room.Wall a -> print_string " w "
            | Some Room.Floor a -> print_string " f "
            | None -> print_string " _ ";
            | other -> print_string " ? " 
          done; print_endline "";
        done
      )
    done;
  done;

  (* Recalculate minimum entropy *)
  let new_min_ent = ref (Float.max_float) in
  for i = 0 to (Array.length wave - 1) do
    for j = 0 to (Array.length wave.(i) - 1) do
      (* Check that section has not been collapsed *)
      let fully_collapsed = ref true in
      for n = 0 to (Array.length samples.(0) - 1) do
        for m = 0 to (Array.length samples.(0).(n) - 1) do
          (match output.(i + n).(j + m) with
           | None -> fully_collapsed := false
           | Some other -> ())
        done
      done;

      (* If uncollapsed and non-contradictory, calculate entropy and 
         compare to minimum *)
      if (!fully_collapsed || ((get_entropy wave.(i).(j)) < 0.))
      then ()
      else (noisy_target_coords.(0) <- i;
            noisy_target_coords.(1) <- j;
            new_min_ent := min !new_min_ent (get_entropy wave.(i).(j)));
    done
  done;

  (*TODO: Remove debug prints bundle *)
  let ctiles = ref 0 in
  for i = 0 to Array.length output - 1 do
    for j = 0 to Array.length output.(0) - 1 do
      match output.(i).(j) with
      | None -> ()
      | Some t -> ctiles := !ctiles + 1
    done;
  done;
  iters := !iters + 1;
  print_string ("[" ^ (string_of_float (Sys.time ()) ^ "]"));
  print_string ("\t Steps: " ^ (string_of_int !iters));
  print_string ("\t Collapsed Tiles: " ^ (string_of_int !ctiles) ^ " / " ^ (string_of_int ((Array.length output) * (Array.length output.(0)))));
  (* print_string ("\t MinEnt: " ^ (string_of_float !new_min_ent)); *)
  print_endline "";

  (* Check if any uncollapsed tiles remain *)
  if (Array.exists (Array.mem None) output)
  (* If incomplete, keep going *)
  then (collapse_loop samples wave output (Random.int 20010827) !new_min_ent)
  (* If complete, unbind tiles from options and return *)
  else (for i = 0 to Array.length output - 1 do
          for j = 0 to Array.length output.(0) - 1 do
            match output.(i).(j) with
            | None -> output.(i).(j) <- Some samples.(0).(0).(0)
            | Some t -> ()
          done;
        done;
        (output |> Array.map
           (fun row -> row |> Array.to_list |> List.filter_map
                         (fun o -> o) |> Array.of_list)))

let remove_diagonals (r : Room.tile array array) : unit =	
  for i = 0 to Array.length r - 2 do	
    for j = 0 to Array.length r.(0) - 2 do	
      match r.(i).(j), r.(i + 1).(j), r.(i).(j + 1), r.(i + 1).(j + 1) with	
      | Floor f1, Wall w1, Wall w2, Floor f2 -> 	
        (r.(i + 1).(j) <- Floor f1;	
         r.(i).(j + 1) <- Floor f1;)	
      | Wall w1, Floor f1, Floor f2, Wall w2 -> 	
        (r.(i).(j) <- Floor f1;	
         r.(i + 1).(j + 1) <- Floor f1;)	
      | _ -> ()	
    done	
  done	
(** [cleam_room r] returns the room r with well connected spaces.- *)	
let clean_room (r : Room.tile array array) : unit =	
  (* open up diagonal gaps *)	
  remove_diagonals r	
let generate_wave (output_rows : int) (output_cols : int) (sample_dim : int) 	
    (samples : Room.tile array array array)	
    (output : Room.tile option array array) (seed_spacing : int)	
  : bool array array array =	
  Array.init (output_rows - sample_dim + 1)	
    (fun i -> Array.init (output_cols - sample_dim + 1) 	
        (fun j -> Array.init (Array.length samples) 	
            (fun n -> sample_viable output i j samples.(n))))

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
  let time_cap = Sys.time () +. 1000. in

  (* Generate sample space list *)
  let samples = sample_space input sample_dim rotations_on reflections_on in

  (* Weights *)
  weight_ref := samples |> Array.map (count_instances samples);

  (* Empty tile array for the final layout *)
  let output = Array.make output_rows (Array.make output_cols None) in

  (* Seed floor tiles *)	
  for i = 0 to ((min output_rows output_cols) - 1 ) / 5 do	
    output.(i * 5).(i * 5) <- Some (!ft_ref.(0))	
  done;	

  (* 3D boolean array represents the wave *)	
  let wave =	
    generate_wave output_rows output_cols sample_dim samples output 5	
  in

  (* Seed should vary between attempts *)
  Random.init seed;
  let attempt_seed = ref (Random.bits ())in

  (* Generative loop *)
  let tiles : Room.tile array array ref = ref [||] in
  print_string ("\ninitial tiles array length: "^(string_of_int (Array.length !tiles)^"\n"));
  for attempt = 1 to attempts do
    if (Array.length !tiles = 0)
    then
      ((* Make an attempt *)
        print_endline "attempt";
        let w_cop = wave |> Array.map (Array.map Array.copy) in
        let o_cop = output |> Array.map Array.copy in
        try tiles := collapse_loop (samples) (w_cop) (o_cop) (!attempt_seed) (get_entropy wave.(0).(0))
        with Failure f -> (iters := 0; Random.init !attempt_seed; attempt_seed := Random.bits ();
                           print_string ("Attempt " ^ (string_of_int attempt)
                                         ^ " failed: " ^ f ^ "\n"));

          (* Ensure limits have not been exceeded *)
          if (Sys.time () > time_cap || attempt = attempts)
          then failwith ("Timed out after attempt " ^ (string_of_int attempt)
                         ^ " and elapsed time "
                         ^ (string_of_float (Sys.time ()-. time_cap +. 100.)))
          else ())
    else ();
  done;
  print_endline "success!!!";

  (* Add bounding walls *)
  let bounded_tiles = 
    Array.make_matrix (Array.length !tiles + 2) (Array.length !tiles.(0) + 2)
      (Room.Wall (Animations.load_image "./sprites/room/wall.bmp"
                    (Window.get_renderer window)))
  in
  for i = 0 to Array.length !tiles - 1 do
    for j = 0 to Array.length !tiles.(0) - 1 do
      bounded_tiles.(i + 1).(j + 1) <- !tiles.(i).(j)
    done
  done;
  tiles := bounded_tiles;

  (* TODO: Clean floating rooms *)
  clean_room !tiles;

  (* TODO: Place entrance and exit *)
  let entry_coords = ref (0., 0.) in
  for i = 0 to Array.length !tiles - 1 do
    for j = 0 to Array.length !tiles.(0) - 1 do
      if (!entry_coords = (0., 0.))
      then (match !tiles.(i).(j) with
          | Floor f -> entry_coords := (float_of_int i, float_of_int j)
          | other -> ())
      else ()
    done
  done;

  (* TODO: Place enemies *)
  let enemies = [] in

  (* TODO: Place items *)
  (* TODO: respond to enemy density and/or entrance/exit distance *)

  let items = 
    Random.init seed;
    let counts_as_wall tiles x y = 
      try match tiles.(y).(x) with |Room.Wall _ -> 1 |_ -> 0 
      with e -> 1 in
    let count_walls_around tiles x y = 
      counts_as_wall tiles (x-1) (y-1) +
      counts_as_wall tiles (x) (y-1) +
      counts_as_wall tiles (x+1) (y-1) +
      counts_as_wall tiles (x+1) (y) +
      counts_as_wall tiles (x+1) (y+1) +
      counts_as_wall tiles (x) (y+1) +
      counts_as_wall tiles (x-1) (y+1) +
      counts_as_wall tiles (x-1) (y) in
    let rec place_items tiles accu x y =
      let next_x = if x + 1 = Array.length tiles.(0) then 0 else x + 1 in
      let next_y = if next_x = 0 then y+1 else y in
      if y > Array.length tiles then accu
      else if Random.float 1. > GameVars.item_spawn_probability && 
              counts_as_wall tiles x y = 0 &&
              count_walls_around tiles x y > GameVars.item_spawn_threshold then 
        let id, lst = accu in 
        place_items tiles 
          ((id + 1),
           ((Item.make_item 
               !attempt_seed 
               (fst accu) 
               window 
               (float_of_int x) 
               (float_of_int y)) :: lst))
          next_x next_y
      else place_items tiles accu next_x next_y in
    place_items !tiles (0, []) 0 0 |> snd in

  let basic_player = 
    (Player.make_player "link" 0 window (fst !entry_coords) (snd !entry_coords))
  in

  {
    seed = seed;
    player = basic_player;
    enemies = enemies;
    items = items;
    tiles = !tiles;
  }

let simple_gen (seed : int) (window : Window.window): Room.t =
  let f = Room.Floor (Animations.load_image "./sprites/room/floor.bmp" (Window.get_renderer window)) in
  let w = Room.Wall (Animations.load_image "./sprites/room/wall.bmp" (Window.get_renderer window)) in
  ft_ref := [|f|];
  let big_chungus_input = 
    [|
      [| w ; w ; w ; f ; w ; w ; w ; w ; w ; w ; w ; w ; f ; w ; w ; w |];
      [| w ; w ; w ; f ; w ; w ; w ; w ; w ; f ; f ; f ; f ; f ; f ; w |];
      [| w ; f ; f ; f ; f ; f ; w ; w ; w ; f ; f ; f ; f ; f ; f ; w |];
      [| w ; f ; f ; f ; f ; f ; w ; w ; w ; f ; f ; f ; f ; f ; f ; w |];
      [| w ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; w |];
      [| f ; f ; f ; f ; f ; f ; w ; w ; w ; f ; f ; f ; f ; f ; f ; f |];
      [| w ; f ; f ; f ; f ; f ; w ; w ; w ; f ; f ; f ; f ; f ; f ; w |];
      [| w ; w ; f ; w ; w ; w ; w ; w ; w ; f ; f ; f ; f ; f ; f ; w |];
      [| w ; w ; f ; w ; w ; w ; w ; w ; w ; w ; w ; f ; w ; w ; w ; w |];
      [| w ; w ; f ; w ; w ; w ; w ; w ; w ; w ; w ; f ; w ; w ; w ; w |];
      [| w ; w ; f ; f ; f ; f ; f ; f ; w ; w ; f ; f ; f ; f ; w ; w |];
      [| f ; f ; f ; f ; f ; f ; f ; f ; w ; w ; f ; f ; f ; f ; f ; f |];
      [| w ; w ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; w ; w |];
      [| w ; w ; f ; f ; f ; f ; f ; f ; w ; w ; f ; f ; f ; f ; w ; w |];
      [| w ; w ; f ; f ; f ; f ; f ; f ; w ; w ; w ; w ; f ; w ; w ; w |];
      [| w ; w ; w ; f ; w ; w ; w ; w ; w ; w ; w ; w ; f ; w ; w ; w |];
    |]
  in
  generate_room seed (big_chungus_input) (3) (20) (20) (0) (window)
  |> (fun room -> print_endline ""; print_int seed; print_endline "";

       (room.tiles |> Array.iter
          (fun row ->
             row |> Array.iter
               (fun elem ->
                  match elem with
                  | Room.Floor a -> print_string "___"
                  | Room.Wall a -> print_string "WWW"
                  | other -> print_string "???");
             print_string "\n")); room)
