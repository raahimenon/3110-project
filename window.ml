type window = Sdlwindow.t * Sdlrender.t
type input = Sdlkeycode.t list(*(Sdlkeycode.t * int) option*)

let q = Sdlkeycode.Q
let esc = Sdlkeycode.Escape
let w = Sdlkeycode.W
let a = Sdlkeycode.A
let s = Sdlkeycode.S
let d = Sdlkeycode.D
let e = Sdlkeycode.E
let enter = Sdlkeycode.Return
let space = Sdlkeycode.Space

let create_window (name : string) (width : int) (height : int) : window = 
  let s = int_of_float GameVars.scale in
  let window = Sdlwindow.create 
      ~pos: (`centered, `centered) 
      ~dims: (width * s, height * s)
      ~title: name
      ~flags: [] in
  let rndr = Sdlrender.create_renderer window (-1) [Sdlrender.Accelerated] in
  ignore(Sdlrender.set_scale rndr (GameVars.scale, GameVars.scale));
  (window, rndr)

let clear (win : window) : unit =
  let w,h = Sdlwindow.get_size (fst win) in
  Sdlrender.clear (snd win);
  Sdlrender.set_draw_color (snd win) (0, 0, 0) 0;
  Sdlrender.fill_rect (snd win) (Sdlrect.make4 0 0 w h)

let clear_white (win : window) : unit = 
  let w,h = Sdlwindow.get_size (fst win) in
  Sdlrender.set_draw_color (snd win) (255, 255, 255) 0;
  Sdlrender.fill_rect (snd win) (Sdlrect.make4 0 0 w h)

let draw_image (win : window) (im : Animations.image) (x : float) (y : float) =
  let x = x *. (GameVars.tile_size) |> int_of_float in
  let y = y *. (GameVars.tile_size) |> int_of_float in
  let w, h, texture = im in
  let dst_rect = 
    Sdlrect.make4 x y w h in
  Sdlrender.copy (snd win) ~texture ~dst_rect ()

let render (win : window) : unit = 
  Sdlrender.render_present (snd win)

let exit_window (win : window) : unit =
  Sdlwindow.destroy (fst win);
  Sdl.quit ();
  exit 0

let rec remove elt lst = 
  match lst with 
  | h :: t -> if elt == h || elt = h then remove elt t else h :: remove elt t
  | [] -> []

let rec print_list lst = match lst with
  | [] -> print_endline ""
  | h::t -> print_string (Sdlkeycode.to_string h); print_list t

let rec input_query lst : input = 
  let rec event (found : input) = match Sdlevent.poll_event () with
    | None -> print_list found; found
    | Some (KeyDown({keycode = k; ke_repeat =r;}))when r = 0-> print_endline ("adding " ^ Sdlkeycode.to_string k); event (k :: (remove k lst))
    | Some (KeyUp({keycode = k; ke_repeat =r;}))when r = 0-> print_endline ("removing " ^ Sdlkeycode.to_string k); event (remove k lst)
    | Some ev -> event found in
  event lst

let get_renderer w = snd w

let get_time = Sdltimer.get_ticks

let wait  = Sdltimer.delay