open Entity
open Player
open Room
open Gamestate
open Animations
open Combat
open Item
open Enemy

let main () = 

  print_string "JSON File to Load in Saves Folder (If unsure, leave blank):";
  let load_file = read_line () in

  let window = Window.create_window "3110 Project" 
      (GameVars.width * (int_of_float GameVars.tile_size)) 
      (GameVars.height * (int_of_float GameVars.tile_size)) in

  (* let default_enemy = make_enemy "link" 3 window in  *)

  (* let default_room = {(Load.load load_file window)  with items =[default_item; default_item2]; enemies = [default_enemy;]} in *)

  let rm_ref : Room.t option ref = ref None in
  Thread.create (fun seed -> rm_ref := 
                    Some (try Load.load load_file window 
                          with e -> Room_gen.simple_gen (Random.int 1073741823) 
                                      window)) () |> ignore;
  let frame = ref 0 in
  let loading = Animations.load_animation 
      "./sprites/loading/" "loading" 
      (Window.get_renderer window) in
  let time = Window.get_time () |> ref in
  while (!rm_ref = None) do 
    Window.clear window;
    Window.draw_image window (Animations.frame loading !frame) 
      (GameVars.hrad -. 2.) (GameVars.vrad -. 2.);
    Window.render window;
    if Window.get_time () - !time > GameVars.anim_spf_in_milli then begin
      time := Window.get_time ();
      frame := (!frame + 1) mod (snd loading |> Array.length) end;
  done;

  let () = Window.clear window in
  let () = Window.render window in
  let time = Window.get_time () in
  Gamestate.game_loop 
    {running = true;
     current_room = Option.get !rm_ref;
     window = window;
     input = [];
     icons = Animations.load_icons (Window.get_renderer window);
     last_anim_frame = time
    } time;
  Window.wait 5;
  Window.exit_window window

let () = main ()