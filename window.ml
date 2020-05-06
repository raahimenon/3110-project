type window = Sdlwindow.t * Sdlrender.t

type input = 
  | Key of Sdlkeycode.t
  | MWheel of int
  | MClick of int

let q = Key Sdlkeycode.Q
let esc = Key Sdlkeycode.Escape
let w = Key Sdlkeycode.W
let a = Key Sdlkeycode.A
let s = Key Sdlkeycode.S
let d = Key Sdlkeycode.D
let e = Key Sdlkeycode.E
let enter = Key Sdlkeycode.Return
let space = Key Sdlkeycode.Space
let x = Key Sdlkeycode.X
let up = Key Sdlkeycode.Up
let down = Key Sdlkeycode.Down
let left = Key Sdlkeycode.Left
let right = Key Sdlkeycode.Right
let lclick = MClick 1
let rclick = MClick 3

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
  Sdlrender.clear (snd win)

let clear_white (win : window) : unit = 
  let w,h = Sdlwindow.get_size (fst win) in
  Sdlrender.set_draw_color (snd win) (255, 255, 255) 0;
  Sdlrender.fill_rect (snd win) (Sdlrect.make4 0 0 w h)

let draw_image (win : window) (im : Animations.image) (x : float) (y : float) =
  let w, h, texture = im in
  let x = x *. (GameVars.tile_size) |> int_of_float in
  let y = (y *. (GameVars.tile_size) |> int_of_float) - 
          (h mod int_of_float GameVars.tile_size) in
  let dst_rect = 
    Sdlrect.make4 x y w h in
  Sdlrender.copy (snd win) ~texture ~dst_rect ()

let render (win : window) : unit = 
  Sdlrender.render_present (snd win)

let exit_window (win : window) : unit =
  Sdlwindow.destroy (fst win);
  Sdl.quit ();
  exit 0

let rec print_list lst = match lst with
  | [] -> print_endline ""
  | h::t -> print_string (Sdlkeycode.to_string h); print_list t


let  remove elt lst = 
  let rec rec_remove  elt lst = 
    match lst with 
    | h :: t -> if elt = h then t else h :: rec_remove elt t
    | [] -> [] in 
  rec_remove elt lst

let rec input_query lst : input list = 
  let lst = MWheel 0 :: List.filter (function |MWheel _ -> false |MClick _ -> false | _ -> true) lst in
  let rec event (found : input list) = match Sdlevent.poll_event () with
    | None -> found
    | Some (KeyDown({keycode = k; }))->  
      event ((Key k)::(remove (Key k) found))
    | Some (KeyUp({keycode = k; })) -> event (remove (Key k) found)
    | Some Mouse_Wheel {mw_y = y} -> 
      List.map (function |MWheel old -> MWheel (old + y) |e -> e) found
    | Some (Mouse_Button_Down {mb_button = b}) -> event ((MClick b)::(remove (MClick b) found))
    | Some ev -> event found in
  event lst

let collision pos1 size1 pos2 size2 =
  (fst pos1 <= fst pos2 + fst size2) &&
  (fst pos1 + fst size1 >= fst pos2) &&
  (snd pos1 <= snd pos2 + snd size2) &&
  (snd pos1 + snd size1 >= snd pos2) 

let get_renderer w = snd w

let get_time = Sdltimer.get_ticks

let wait  = Sdltimer.delay

let draw_hud_box (win : window) slot : unit =
  let rndr = snd win in
  let ts = int_of_float GameVars.tile_size in
  let w,h = Sdlwindow.get_size (fst win) in
  let s = int_of_float GameVars.scale in
  let w,h = w/s,h/s in
  Sdlrender.fill_rect rndr (Sdlrect.make4 0 0 (ts + 2 * GameVars.hud_bezel_px) h);
  Sdlrender.fill_rect rndr (Sdlrect.make4 (w - ts - 2*GameVars.hud_bezel_px) 0 (ts + 2 * GameVars.hud_bezel_px) h);
  Sdlrender.set_draw_color rndr (255, 255, 255) 1;
  Sdlrender.draw_line2 rndr (ts + 2 * GameVars.hud_bezel_px, 0) (ts + 2 * GameVars.hud_bezel_px, h);
  Sdlrender.draw_rect rndr (Sdlrect.make4 (GameVars.hud_bezel_px - 3) ((2 * slot + 1) * (ts) - 2) (ts + 6) (ts + 6));
  Sdlrender.draw_line2 rndr (w - ts - 2*GameVars.hud_bezel_px, 0) (w - ts - 2*GameVars.hud_bezel_px, h);
  Sdlrender.draw_line2 rndr (w - ts - 2*GameVars.hud_bezel_px, h / 2) (w, h / 2);
  Sdlrender.set_draw_color rndr (0, 0, 0) 1

let health_col_ratio ratio = 
  let r = if ratio <= 0.5 then 255 else (255. *. 2. *. (1. -. ratio) |> int_of_float) in
  let g = if ratio >= 0.5 then 255 else (255. *. 2. *. (ratio -. 0.5) |> int_of_float) in
  (r, g, 0)

let draw_rect_col (win : window) (r,g,b) (x,y) (w,h) = 
  let rndr = snd win in
  let x = GameVars.tile_size *. x |> int_of_float in
  let y = GameVars.tile_size *. y |> int_of_float in
  let w = GameVars.tile_size *. w |> int_of_float in
  let h = GameVars.tile_size *. h |> int_of_float in
  Sdlrender.set_draw_color rndr (r,g,b) 1;
  Sdlrender.fill_rect rndr (Sdlrect.make4 x y w h);
  Sdlrender.set_draw_color rndr (0,0,0) 1