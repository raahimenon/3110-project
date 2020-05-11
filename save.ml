(** [player_json p] returns a representation of player [p] in json 
    form. *)
let player_json (p : Player.Player.t) : string =
  let pos = match p.e.pos with (x,y) -> [|x;y|] in
  "\t\t{\n"
  ^"\t\t\t\"id\": "^Int.to_string p.id^",\n"
  ^"\t\t\t\"x\": "^Float.to_string pos.(0)^"0,\n"
  ^"\t\t\t\"y\": "^Float.to_string (pos.(1))^"0,\n"
  ^"\t\t\t\"health\": "^Int.to_string p.health^",\n"
  ^"\t\t\t\"max_health\": "^Int.to_string p.max_health^",\n"
  ^"\t\t\t\"attack\": "^Float.to_string p.unique_stats.attack^"0,\n"
  ^"\t\t\t\"speed\": "^Int.to_string p.unique_stats.movement_speed
  ^"\n\t\t},\n"

(** [enemy_json e] returns a representation of enemy [e] in json 
    form. *)
let enemy_json (enemy : Enemy.Enemy.t) : string =
  let pos = match enemy.e.pos with (x, y) -> [|x;y|] in
  "\t\t{\n"
  ^"\t\t\t\"id\": "^Int.to_string enemy.id^",\n"
  ^"\t\t\t\"x\": "^Float.to_string pos.(0)^"0,\n"
  ^"\t\t\t\"y\": "^Float.to_string pos.(1)^"0,\n"
  ^"\t\t\t\"health\": "^Int.to_string enemy.health
  ^"\n\t\t}"

(** [item_json i] returns a representation of item [i] in json 
    form. *)
let item_json (i : Item.Item.t) : string =
  let pos = match i.pos with 
    | Inventory {index = i} -> [|float_of_int i *. -1. -. 1.; 
                                 float_of_int i *. -1.-.1.|]
    | Position (x,y) -> [|x;y|]
  in let dur = match i.unique_stats with
      | Buff {max_durability;durability;effect} -> durability
      | other -> -1
  in
  "\t\t{\n"
  ^"\t\t\t\"seed\": "^Int.to_string i.seed^",\n"
  ^"\t\t\t\"id\": "^Int.to_string i.id^",\n"
  ^"\t\t\t\"x\": "^Float.to_string pos.(0)^"0,\n"
  ^"\t\t\t\"y\": "^Float.to_string pos.(1)^"0,\n" 
  ^"\t\t\t\"durability\": "^Int.to_string dur
  ^"\n\t\t}"

(** [room_json r] returns a [representation of room [r] in json 
    form. *)
let room_json (r : Room.t) : string =
  " {\n"
  ^ "\t\"seed\": "^ (string_of_int r.seed) ^",\n"
  ^ "\t\"player\": \n"
  ^ player_json r.player
  ^ "\t\"items\": [\n"
  ^ String.concat ",\n" (List.map item_json r.items)
  ^ "\n\t],\n"
  ^ "\t\"enemies\": [\n"
  ^ String.concat ",\n" (List.map enemy_json r.enemies)
  ^ "\n\t]\n}"

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
  |> (fun s -> if Array.mem s (try (Sys.readdir "saves")
                               with Sys_error f -> 
                                 raise (Sys_error "saves folder not found"))
       then failwith "[name] is already being used"
       else "saves/"^s)

  (* Write save to [name] in [saves] *)
  |> Stdlib.open_out
  |> (fun channel ->
      Stdlib.output_string channel (room_json r);Stdlib.flush channel)
