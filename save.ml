let player_json (p : Player.Player.t) : string =
  let pos = match p.pos with (x,y) -> [|x;y|] in
  "\t\t{\n"
  ^"\t\t\t\"id\": "^Int.to_string p.id^",\n"
  ^"\t\t\t\"x\": "^Int.to_string (int_of_float pos.(0))^",\n"
  ^"\t\t\t\"y\": "^Int.to_string (int_of_float pos.(1))^",\n"
  ^"\t\t\t\"health\": "^Int.to_string p.health
  ^"\n\t\t}"

let enemy_json (e : Enemy.Enemy.t) : string =
  let pos = match e.pos with (x, y) -> [|x;y|] in
  "\t\t{\n"
  ^"\t\t\t\"id\": "^Int.to_string e.id^",\n"
  ^"\t\t\t\"x\": "^Int.to_string (int_of_float pos.(0))^",\n"
  ^"\t\t\t\"y\": "^Int.to_string (int_of_float pos.(1))^",\n"
  ^"\t\t\t\"health\": "^Int.to_string e.health
  ^"\n\t\t}"

let item_json (i : Item.Item.t) : string =
  let pos = match i.pos with 
    | Position (x,y) -> [|x;y|]
    | Inventory -> [|-1.;-1.|]
  in let dur = match i.unique_stats with
      | Buff {max_durability;durability;effect} -> durability
      | other -> -1
  in
  "\t\t{\n"
  ^"\t\t\t\"id\": "^Int.to_string i.id^",\n"
  ^"\t\t\t\"x\": "^Int.to_string (int_of_float pos.(0))^",\n"
  ^"\t\t\t\"y\": "^Int.to_string (int_of_float pos.(1))^",\n"
  ^"\t\t\t\"health\": "^Int.to_string dur
  ^"\n\t\t}"

(** [room_json r] returns a [string] representing [room] [r] in json 
    form. *)
let room_json (r : Room.t) : string =
  "{\n"
  ^"\t\"seed\": 0,\n"
  ^"\t\"entities\": [\n"
  ^player_json r.player^
  (if List.length r.enemies > 0 then ",\n" else "")^
  String.concat ",\n" (List.map enemy_json r.enemies)^
  (if List.length r.items > 0 then ",\n" else "")^
  String.concat ",\n" (List.map item_json r.items)
  ^"\n\t]\n}"

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
       else (name^".json"))

  (* Check that [name] is not already being used in [saves] *)
  (* TODO: rebuild saves folder if missing *)
  |> (fun s -> if Array.mem s (try (Sys.readdir "saves")
                               with Sys_error f -> 
                                 raise (Sys_error "saves folder not found"))
       then failwith "[name] is already being used"
       else "saves/"^s)

  (* Write save to [name] in [saves] *)
  |> Stdlib.open_out
  |> (fun channel ->
      Stdlib.output_string channel (room_json r);Stdlib.flush channel)
