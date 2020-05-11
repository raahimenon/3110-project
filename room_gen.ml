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

(** [basic_sample_space input sample_dim] returns an array of samples of size 
    [sample_dim] * [sample_dim] taken from [input].
    Note: the input is assumed to be non-periodic, so samples will not wrap 
    around the ends of the input. *)
let basic_sample_space (input : Room.tile array array) (sample_dim : int)
  : Room.tile array array array =
  Array.init (Array.length input - sample_dim + 1)
    (fun i -> Array.init (Array.length input.(0) - sample_dim + 1)
        (fun j -> Array.init sample_dim
            (fun n -> Array.init sample_dim
                (fun m -> input.(i+n).(j+m)))))

  (* Flatten top level of array *)
  |> Array.to_list |> Array.concat

(** [add_reflections samples] returns an array with all elements of [samples] 
    in addition to their reflected forms. *)
let add_reflections (samples : Room.tile array array array)
  : Room.tile array array array =
  Array.concat [samples; (Array.map reflect_sample_x samples);
                (Array.map (fun e ->
                     e |> rotate_sample_90 |> rotate_sample_90
                     |> reflect_sample_x) samples)]

(** [add_rotations samples] returns an array with all elements of [samples] 
    in addition to their rotated forms. *)
let add_rotations (samples : Room.tile array array array)
  : Room.tile array array array =
  let rot90 = Array.map rotate_sample_90 samples in
  let rot180 = Array.map rotate_sample_90 rot90 in
  let rot270 = Array.map rotate_sample_90 rot180 in
  Array.concat [samples; rot90; rot180; rot270]

(** [sample_space input sample_dim rotations_on reflections_on] generates 
    all possible samples based on inputs. Include rotations and reflections 
    if desired (may significantly decrease generator efficiency). *)
let sample_space (input : Room.tile array array) (sample_dim : int)
    (rotations_on : bool) (reflections_on : bool)
  : Room.tile array array array = 
  (* Create base sample space *)
  basic_sample_space input sample_dim

  (* Conditional addition of reflected samples *)
  |> (if reflections_on
      then add_reflections
      else (fun s -> s))

  (* Conditional addition of rotated samples *)
  |> (if rotations_on 
      then add_rotations
      else (fun s -> s))

(** [weight_ref] contains the weights of each sample in the sample space. 
    Guaranteed to be intialized before being called. *)
let weight_ref : int array ref = ref [||]
(** [noisy_target_coords] contains the coordinates of the next wave section 
    selected for collapsing. Initialized with values [0, 0] to ensure that the 
    WFC algorithm will begin in the upper-left corner of the room. While this 
    reduces the number of possible rooms, it ensures that the resulting room 
    will be well-connected. *)
let noisy_target_coords : int array = [|0; 0|]
(** [get_entropy elem] returns the Shannon Entropy (SE) of a given wave 
    section. SE is calculated based on the probability for each sample that 
    the slice will actually collapse to it. *)
let get_entropy (elem : bool array) : float =
  (* Total number of possibilities *)
  let total_pos =
    Array.fold_left (fun prev b -> prev +. (Bool.to_float b)) 0. elem
  in
  (* Sum of all probabilities *)
  let total_SE = ref 0. in
  Array.iter2 (fun valid weight ->
      if (not valid)
      then ()
      else (let p = (float_of_int weight) /. total_pos in
            total_SE :=
              !total_SE -. (p *. (log p) /. (log 2.) /. (float_of_int weight))))
    elem !weight_ref;
  (* Contradictory slices are given a SE of -1. This is because, ordinarily, 
     both contradictory and collapsed slices have a SE of 0, but our 
     implementation requires that we ignore contradictory slices, so we need 
     to separate the two. *)
  if (Array.mem true elem) then !total_SE +. Random.float 1e-6 else -1.0

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

(** [iters] tracks the number of iterations that the WFC algorithm has 
    run through *)
let iters = ref 0
(** [count_instances collection elem] counts the number of elements in 
    [collection] that are equal to [elem]. Comparison uses structural
    equality. *)
let count_instances (collection : 'a array) (elem : 'a) : int =
  let count = ref 0 in
  for i = 0 to Array.length collection - 1 do
    if collection.(i) = elem then count := !count + 1 else ()
  done;
  !count

(** [observe wave samples output] 'observes' a randomly selected section of 
    the wave, collapsing it to a viable sample. *)
let observe (wave : bool array array array)
    (samples : Room.tile array array array)
    (output : Room.tile option array array) : unit =
  (* Randomly select a sample *)
  let sample_indices =
    wave.(noisy_target_coords.(0)).(noisy_target_coords.(1)) |> get_indices (fun s -> s)
  in
  let sample_index = 
    Array.get sample_indices (Random.int (Array.length sample_indices))
  in

  (* Manifest the collapse in the output array *)
  for i = 0 to Array.length samples.(sample_index) - 1 do
    for j = 0 to Array.length samples.(sample_index).(i) - 1 do
      output.(noisy_target_coords.(0) + i).(noisy_target_coords.(1) + j)
      <- Some samples.(sample_index).(i).(j)
    done
  done

(** [fully_collapsed samples output i j] returns whether or not the wave at 
    coordinates [i], [j] is collapsed. *)
let fully_collapsed (samples : Room.tile array array array)
    (output : Room.tile option array array) (i : int) (j : int) : bool =
  let status = ref true in
  for n = 0 to (Array.length samples.(0) - 1) do
    for m = 0 to (Array.length samples.(0).(n) - 1) do
      (match output.(i + n).(j + m) with
       | None -> status := false
       | Some other -> ())
    done
  done;
  !status

(** [wave_propogate s w o] recalculates the viabilities of samples in 
    [s] across the wave [w] based on changes to output [o]. This refactoring 
    is meant to be called after [observe] as a part of the propogation step 
    of the WFC algorithm. *)
let wave_propogate (s : Room.tile array array array)
    (w : bool array array array) (o : Room.tile option array array) : unit =
  (* Check that each sample is viable by comparing each predicted tile with 
     partially collapsed output *)
  for i = max 0 (noisy_target_coords.(0) - (Array.length s.(0)) + 1)
    to min (Array.length w - 1)
        (noisy_target_coords.(0) + (Array.length s.(0)) - 1) do
    for j = max 0 (noisy_target_coords.(1) - (Array.length s.(0).(0)) + 1)
      to min (Array.length w.(0) - 1)
          (noisy_target_coords.(1) + (Array.length s.(0).(0)) - 1) do
      w.(i).(j) <- (s |> Array.map (sample_viable o i j));
    done;
  done

(** [propogate_entropy wave samples output] recalculates the entropies across 
    the wave to identify the section with the lowest entropy, which will be 
    collapsed on the next step of the WFC algorithm. *)
let propogate_entropy (wave: bool array array array)
    (samples : Room.tile array array array)
    (output : Room.tile option array array) : unit =
  let new_min_ent = ref (Float.max_float) in
  for i = 0 to (Array.length wave - 1) do
    for j = 0 to (Array.length wave.(i) - 1) do
      (* Check that section has not been collapsed *)
      let collapsed = fully_collapsed samples output i j in

      (* If uncollapsed and non-contradictory, calculate entropy and 
         compare to minimum *)
      if (collapsed || ((get_entropy wave.(i).(j)) < 0.))
      then ()
      else (noisy_target_coords.(0) <- i;
            noisy_target_coords.(1) <- j;
            new_min_ent := min !new_min_ent (get_entropy wave.(i).(j)));
    done
  done

(** [print_progress output] prints information on the state of the algorithm 
    after another step through the core loop. *)
let print_progress (output : Room.tile option array array) : unit = 
  let ctiles = ref 0 in
  for i = 0 to Array.length output - 1 do
    for j = 0 to Array.length output.(0) - 1 do
      match output.(i).(j) with
      | None -> ()
      | Some t -> ctiles := !ctiles + 1
    done;
  done;
  iters := !iters + 1;
  print_endline ("[" ^ (string_of_float (Sys.time ()) ^ "]") ^ "\t Steps: "
                 ^ (string_of_int !iters) ^ "\t Collapsed Tiles: "
                 ^ (string_of_int !ctiles) ^ " / "
                 ^ (string_of_int ((Array.length output)
                                   * (Array.length output.(0)))))

(** [unbind output samples] returns the contents of [output], removed from the 
    option monad. *)
let unbind (output : Room.tile option array array)
    (samples : Room.tile array array array) : Room.tile array array =
  for i = 0 to Array.length output - 1 do
    for j = 0 to Array.length output.(0) - 1 do
      match output.(i).(j) with
      | None -> output.(i).(j) <- Some samples.(0).(0).(0)
      | Some t -> ()
    done;
  done;
  (output |> Array.map
     (fun row -> row |> Array.to_list |> List.filter_map
                   (fun o -> o) |> Array.of_list))

(** [collapse_loop samples wave output seed] collapses sections of the wave 
    until the entire wave has been collapsed. Executes the core loop of the 
    WFC algorithm. *)
let rec collapse_loop (samples : Room.tile array array array)
    (wave : bool array array array) (output : Room.tile option array array)
    (seed : int) : Room.tile array array =
  (* Initialize RNG *)
  Random.init seed;

  (* Observe a wave section *)
  observe wave samples output;

  (* Adjust wave possibilities *)
  wave_propogate samples wave output;

  (* Recalculate minimum entropy *)
  propogate_entropy wave samples output;

  (* Progress readout *)
  print_progress output;

  (* If uncollapsed sections remain, keep going. Otherwise, unbind tiles from 
     options and return. *)
  if (Array.exists (Array.mem None) output)
  then (collapse_loop samples wave output (Random.int 20010827))
  else (unbind output samples)

(** [remove_diagonals r] returns room [r] with all weak diagonal walls removed. 
    Weak diagonal walls are those where floor tiles on opposite sides of the 
    contiguous wall are still diagonally adjacent. *)
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
(** [cleam_room r] returns the room r with well connected spaces. *)	
let clean_room (r : Room.tile array array) : unit =	
  (* open up diagonal gaps *)	
  remove_diagonals r	

(** [generate_wave output_rows output_cols sample_dim samples output] returns 
    a wave array representing the viability of samples across the given
    output based on the various inputs. *)
let generate_wave (output_rows : int) (output_cols : int) (sample_dim : int) 	
    (samples : Room.tile array array array)	
    (output : Room.tile option array array) : bool array array array =	
  Array.init (output_rows - sample_dim + 1)	
    (fun i -> Array.init (output_cols - sample_dim + 1) 	
        (fun j -> Array.init (Array.length samples) 	
            (fun n -> sample_viable output i j samples.(n))))

(** [seed_floor_tiles output dist] seeds the output array with floor tiles at 
    the specified spacing. *)
let seed_floor_tiles (output : Room.tile option array array)
    (dist : int) : unit =
  for i = 0
    to ((min (Array.length output) (Array.length output.(0))) - 1 ) / dist
  do output.(i * dist).(i * dist) <- Some (!ft_ref.(0))	
  done

(** [validate_inputs input sample_dim output_rows output_cols difficulty] 
    ensure that none of the inputs for the WFC algorithm are out of bounds.
    Raises [failure] if any inputs are invalid. *)
let validate_inputs (input : Room.tile array array) (sample_dim : int)
    (output_rows : int) (output_cols : int) (difficulty : float) : unit =
  if (sample_dim <= 0)
  || (sample_dim > Array.length input)
  || (sample_dim > Array.length input.(0))
  || (sample_dim > output_rows)
  || (sample_dim > output_cols)
  || (difficulty > 1.0)
  || (difficulty < 0.0)
  then failwith "invalid inputs at generate_room"
  else ()

(** [point_distance_vector p0 p1] determins the distance vector between two 
    points [p0] and [p1].  *)
let point_distance_vector (p0 : float * float) (p1 : float * float)
  : float * float =
  (fst p1 -. fst p0, snd p1 -. snd p0)

(** [dot_product v0 v1] is the dot product of two vectors, [v0] and [v1]. *)
let dot_product (v0 : float * float) (v1 : float * float) : float =
  (fst v0 *. fst v1) +. (snd v0 *. snd v1)

(** [vector_magnitude] returns the magnitude of vector [v]. *)
let vector_magnitude (v : float * float) =
  sqrt ((fst v *. fst v) +. (snd v *. snd v))

(** [get_perlin_weight v0 v1 v2 v3] calculates the interpolated dot products 
    for four vectors. Due to the coarse-grained nature of a tile-based system, 
    a simple weighted average is used for its computational efficiency, even 
    though typical perlin noise generation utilizes more sophisticated 
    interpolation methods. *)
let get_perlin_weight (v0 : float * float) (v1 : float * float)
    (v2 : float * float) (v3 : float * float) : float =
  (* Get random sample point *)
  let spt = (Random.float 1.0, Random.float 1.0) in

  (* Get distance vectors *)
  let d0 = point_distance_vector (0.0, 1.0) spt in
  let d1 = point_distance_vector (1.0, 1.0) spt in
  let d2 = point_distance_vector (0.0, 0.0) spt in
  let d3 = point_distance_vector (1.0, 0.0) spt in

  (* Get dot products of gradients and distance vectors *)
  let p0 = dot_product v0 d0 in let p1 = dot_product v1 d1 in
  let p2 = dot_product v2 d2 in let p3 = dot_product v3 d3 in

  (* Take weighted average *)
  let avg =
    ((p0 *. vector_magnitude d0) +. (p1 *. vector_magnitude d1)
     +. (p2 *. vector_magnitude d2) +. (p3 *. vector_magnitude d3))
    /. ((vector_magnitude d0) +. (vector_magnitude d1)
        +. (vector_magnitude d2) +. (vector_magnitude d3))
  in
  avg

(** [random_gradients r] returns an array of random gradient vectors based on 
    the dimensions of [r]. *)
let random_gradients (r : Room.tile array array) : (float * float) array array =
  Array.init (Array.length r + 1)
    (fun i -> Array.init (Array.length r.(0) + 1)
        (fun j -> let angle = Random.float (2.0 *. Float.pi) in
          ((Float.cos angle), (Float.sin angle))))

(** [perlin_weights vectors r] returns an array of weights calculated as in 
    the Perlin Noise algorithm based on its inputs. *)
let perlin_weights (vectors : (float * float) array array)
    (r : Room.tile array array) : float array array =
  Array.init (Array.length r)
    (fun i -> Array.init (Array.length r.(0))
        (fun j -> (get_perlin_weight
                     vectors.(i).(j)
                     vectors.(i).(j + 1)
                     vectors.(i + 1).(j)
                     vectors.(i + 1).(j + 1)
                   +. 1.0)
                  /. 2.0))

(** [perlin_coords weights r difficulty] returns the qualifying coordinate 
    pairs from the perlin noise map created with the given inputs. *)
let perlin_coords (weights : float array array) (r : Room.tile array array)
    (difficulty : float) : (float * float) array = 
  let coords = Queue.create () in
  for i = 0 to Array.length r - 1 do
    for j = 0 to Array.length r.(i) - 1 do
      match r.(i).(j), weights.(i).(j) with
      | Floor f, w when w < (Float.pow difficulty 0.7) -> Queue.add (float_of_int i, float_of_int j) coords
      | _ -> ()
    done
  done;

  coords |> Queue.to_seq |> Array.of_seq
  |> Array.to_list |> List.filter_map (fun coords ->
      if not (r.(int_of_float (fst coords)).(int_of_float (snd coords))
              <> !ft_ref.(0))
      then (Some coords)
      else (None))
  |> Array.of_list

(** [get_enemy_coords difficulty seed r] returns an array of coordinates where 
    enemies will be spawned on a given floor based on the inputs. The Perlin Noise 
    algorithm is used to select coordinates. More information on this algorithm 
    can be found at https://dl.acm.org/doi/pdf/10.1145/325165.325247. *)
let get_enemy_coords (difficulty : float) (seed : int)
    (r : Room.tile array array) : (float * float) array =
  (* Initialize RNG *)
  Random.init seed;

  (* Generate gradient vector map *)
  let vectors = random_gradients r in

  (* Calculate perlin weights *)
  let weights = perlin_weights vectors r in

  (* Filter for wall collisions and difficulty caps *)
  perlin_coords weights r difficulty

(** [add_border tiles window] surrounds the tiles in [tiles] with walls. *)
let add_border (tiles : Room.tile array array ref) (window : Window.window) 
  : unit = 
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
  tiles := bounded_tiles

let counts_as_wall tiles x y = 
  try match tiles.(y).(x) with |Room.Floor _ -> 0 |_ -> 1 
  with e -> 1
let count_walls_around tiles x y = 
  counts_as_wall tiles (x-1) (y-1) +
  counts_as_wall tiles (x) (y-1) +
  counts_as_wall tiles (x+1) (y-1) +
  counts_as_wall tiles (x+1) (y) +
  counts_as_wall tiles (x+1) (y+1) +
  counts_as_wall tiles (x) (y+1) +
  counts_as_wall tiles (x-1) (y+1) +
  counts_as_wall tiles (x-1) (y)

(** Central method. Too sleepy to document. Haven't even test it yet. *)
let generate_room (seed : int) (input : Room.tile array array)
    (sample_dim : int) (output_rows : int) (output_cols : int)
    (difficulty : float) (window : Window.window) : Room.t =
  (* Validate inputs *)
  validate_inputs input sample_dim output_rows output_cols difficulty;

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
  seed_floor_tiles output 5;

  (* 3D boolean array represents the wave *)	
  let wave = generate_wave output_rows output_cols sample_dim samples output in

  (* Seed should vary between attempts *)
  Random.init seed;
  let attempt_seed = ref (Random.bits ()) in

  (* Generative loop *)
  let tiles : Room.tile array array ref = ref [||] in
  for attempt = 1 to attempts do
    if (Array.length !tiles = 0)
    then
      ((* Make an attempt *)
        print_endline "attempt";
        let w_cop = wave |> Array.map (Array.map Array.copy) in
        let o_cop = output |> Array.map Array.copy in
        try tiles := collapse_loop (samples) (w_cop) (o_cop) (!attempt_seed)
        with Failure f -> (iters := 0; Random.init !attempt_seed;
                           attempt_seed := Random.bits (););
          (* Ensure limits have not been exceeded *)
          if (Sys.time () > time_cap || attempt = attempts)
          then failwith ("Timed out after attempt " ^ (string_of_int attempt)
                         ^ " and elapsed time "
                         ^ (string_of_float (Sys.time ()-. time_cap +. 100.)))
          else ())
    else ();
  done;

  (* Add bounding walls *)
  add_border tiles window;

  (* Clean floating rooms *)
  clean_room !tiles;

  (* Place entrance and exit *)
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

  let exit_coords =
    (* Implementation of Prim's MST algorithm *)
    let rec get_mst seen_tiles visited_tiles curr_tile = 
      let ctile_x, ctile_y, cdist = curr_tile in
      let udir = ctile_y - 1 in
      let ddir = ctile_y + 1 in
      let ldir = ctile_x - 1 in
      let rdir = ctile_x + 1 in
      let udist_rec = match Hashtbl.find_opt seen_tiles (ctile_x, udir) with 
        |Some d -> d |None -> Int.max_int in
      let ddist_rec = match Hashtbl.find_opt seen_tiles (ctile_x, ddir) with 
        |Some d -> d |None -> Int.max_int in
      let ldist_rec = match Hashtbl.find_opt seen_tiles (ldir, ctile_y) with
        |Some d -> d |None -> Int.max_int in
      let rdist_rec = match Hashtbl.find_opt seen_tiles (rdir, ctile_y) with
        |Some d -> d |None -> Int.max_int in 
      let ndist = cdist + 1 in
      Hashtbl.add visited_tiles (ctile_x, ctile_y) cdist;
      if ndist < udist_rec && 
         not (Hashtbl.mem visited_tiles (ctile_x, udir)) &&
         counts_as_wall !tiles ctile_x udir = 0 then
        Hashtbl.replace seen_tiles (ctile_x, udir) ndist;
      if ndist < ddist_rec && 
         not (Hashtbl.mem visited_tiles (ctile_x, ddir)) &&
         counts_as_wall !tiles ctile_x ddir = 0 then
        Hashtbl.replace seen_tiles (ctile_x, ddir) ndist;
      if ndist < ldist_rec && 
         not (Hashtbl.mem visited_tiles (ldir, ctile_y)) && 
         counts_as_wall !tiles ldir ctile_y = 0 then
        Hashtbl.replace seen_tiles (ldir, ctile_y) ndist;
      if ndist < rdist_rec && 
         not (Hashtbl.mem visited_tiles (rdir, ctile_y)) && 
         counts_as_wall !tiles rdir ctile_y = 0 then
        Hashtbl.replace seen_tiles (rdir, ctile_y) ndist;
      if Hashtbl.length seen_tiles <= 0 then visited_tiles
      else 
        let get_min mst = 
          Hashtbl.fold 
            (fun (x,y) d (x_min,y_min,min) -> 
               if d < min then (x, y, d) else (x_min, y_min, min)) 
            mst (0, 0, Int.max_int) in
        let x_min, y_min, min = get_min seen_tiles in
        Hashtbl.remove seen_tiles (x_min, y_min);
        get_mst seen_tiles visited_tiles (x_min, y_min, min) in
    let mst = 
      get_mst 
        (Hashtbl.create 30)
        (Hashtbl.create 30) 
        (fst !entry_coords |> int_of_float, 
         snd !entry_coords |> int_of_float, 0) in 

    (* Set the exit nodes to be the furthest reachable node from the player *)
    let x_exit, y_exit, _ = Hashtbl.fold 
        (fun (x,y) d (x_max,y_max,max) -> 
           if d > max then (x, y, d) else (x_max, y_max, max)) 
        mst (0, 0, Int.min_int) in
    (x_exit, y_exit) in

  !tiles.(snd exit_coords).(fst exit_coords) <- 
    Room.Exit (Animations.load_image "./sprites/room/exit.bmp" 
                 (Window.get_renderer window));

  (* Place enemies *)
  let enemy_coords = get_enemy_coords difficulty seed !tiles |> Array.to_list in
  let enemies = 
    Random.init seed;
    let rec place_enemies num accu = function
      | [] -> accu
      | (x,y)::t -> place_enemies (num + 1) 
                      (Enemy.make_enemy seed num window x y difficulty :: accu)
                      t in 
    place_enemies 0 [] enemy_coords in

  (* Place items *)
  let items = 
    Random.init seed;
    let rec place_items tiles accu x y =
      let next_x = if x + 1 = Array.length tiles.(0) then 0 else x + 1 in
      let next_y = if next_x = 0 then y+1 else y in
      if y > Array.length tiles then accu
      else if Random.float 1. > GameVars.item_spawn_probability && 
              counts_as_wall tiles x y = 0 &&
              !entry_coords <> (float_of_int x,float_of_int y) &&
              count_walls_around tiles x y > GameVars.item_spawn_threshold then 
        let id, lst = accu in 
        place_items tiles 
          ((id + 1),
           ((Item.make_item 
               seed 
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
  Random.init seed;
  generate_room seed (big_chungus_input) (3) (20) (20) (0.1) (window)
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