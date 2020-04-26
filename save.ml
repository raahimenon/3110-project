let player_json (p : Player.Player.t) : string =
  let pos = match p.pos with (x,y) -> [|x;y|] in
  "{"
  ^"\"id\": "^Int.to_string p.id^", "
  ^"\"x\": "^Float.to_string pos.(0)^", "
  ^"\"y\": "^Float.to_string pos.(1)^", "
  ^"\"health\": "^Int.to_string p.health
  ^"}"

let enemy_json (e : Enemy.Enemy.t) : string =
  let pos = match e.pos with (x, y) -> [|x;y|] in
  "{"
  ^"\"id\": "^Int.to_string e.id^", "
  ^"\"x\": "^Float.to_string pos.(0)^", "
  ^"\"y\": "^Float.to_string pos.(1)^", "
  ^"\"health\": "^Int.to_string e.health
  ^"}"

let item_json (i : Item.Item.t) : string =
  let pos = match i.pos with 
    | Position {x;y} -> [|x;y|]
    | Inventory -> [|-1.;-1.|]
  in let dur = match i.unique_stats with
      | Buff {max_durability;durability;effect} -> durability
      | other -> -1
  in
  "{"
  ^"\"id\": "^Int.to_string i.id^", "
  ^"\"x\": "^Float.to_string pos.(0)^", "
  ^"\"y\": "^Float.to_string pos.(1)^", "
  ^"\"health\": "^Int.to_string dur
  ^"}"

(** [room_json r] returns a [string] representing [room] [r] in json 
    form. *)
let room_json (r : Room.t) : string =
  "{"
  ^"\"seed\": 0, "
  ^"\"entities\": ["
  ^player_json r.player^", "
  ^String.concat ", " (List.map enemy_json r.enemies)^", "
  ^String.concat ", " (List.map item_json r.items)
  ^"]}"

(** [save name] creates a save file in the [saves] folder with name 
    [name] if possible. If [name] contains non-alphanumeric characters or is 
    already being used as a filename in [saves], then raise [Failure]. *)
let save (r : Room.t) (name : string) : unit =
  (* Check that [name] is alphanumeric chars only *)
  (* TODO: figure out if using Str module is worth the effort *)
  name |> String.uppercase_ascii |> String.to_seq |> List.of_seq
  |> List.map Char.code
  |> List.exists (fun i -> (i < 48) || (i > 57 && i < 65) || (i > 90))
  |> (fun (b : bool) : string -> if b
       then failwith "[name] contains non-alphanumeric characters"
       else name)

  (* Check that [name] is not already being used in [saves] *)
  (* TODO: rebuild saves folder if missing *)
  |> (fun s -> if Array.mem s (try (Sys.readdir "saves")
                               with Sys_error f -> 
                                 raise (Sys_error "saves folder not found"))
       then failwith "[name] is already being used"
       else "saves/"^s)

  (* Write save to [name] in [saves] *)
  |> Stdlib.open_out
  |> (fun channel -> Stdlib.output_string channel (room_json r))
