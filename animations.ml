type image = int * int * Sdltexture.t
type animation = string * image array

let load_image f rndr: image = 
  try 
    let surf = Sdlsurface.load_bmp f in 
    let w,h = Sdlsurface.get_dims surf in 
    w,h,Sdltexture.create_from_surface rndr surf
  with _ -> failwith (f ^ " not found")

let load_animation f name rndr : animation = 
  let files = Sys.readdir f in
  name, Array.map (fun file -> load_image (f^"/"^file) rndr) files

let load_direction f d rndr : animation list = 
  let animations = Sys.readdir f |> Array.to_list in
  List.map (fun anim -> load_animation (f^anim) (d^"*"^anim) rndr) animations

let load_directions o rndr: animation list = 
  let o = "./sprites/"^o^"/" in
  let up = try load_direction (o^"up/") "up" rndr with e -> [] in
  let down = try load_direction (o^"down/") "down" rndr with e -> [] in
  let right = try load_direction (o^"right/") "right" rndr with e -> [] in
  let left = try load_direction (o^"left/") "left" rndr with e -> [] in
  down @ up @ left @ right

let next_frame i anim = 
  let anim = snd anim in
  if i + 1 >= Array.length anim then 0 else (i + 1)

let curr_frame i anim = (snd anim).(i)

(*let im_to_str (im : image) : string = 
  let pix_to_str (pix : Graphics.color) : string =
    if pix = Graphics.transp then "0,0,0,0"
    else 
      let b = pix mod (16*16) in
      let g = (pix - b) / (16 * 16) mod (16 * 16) in
      let r = ((pix - b) / (16 * 16) - g)/(16*16) mod (16*16) in 
      string_of_int r ^ "," ^ string_of_int g ^ "," ^ string_of_int b ^",255" in

  let row_to_str (row : Graphics.color array) : string = 
    Array.map pix_to_str row |> Array.to_list |> String.concat " " in
  Array.map row_to_str im |> Array.to_list |> String.concat "\n"*)

let size_im (im : image) : (int * int) = 
  let w,h,_ = im in w,h

let size (a : animation) : (int * int) = 
  size_im (snd a).(0)

let name (anim : animation) : string = 
  fst anim

let anims_from_direction (anims : animation list) (dir : string) = 
  try
    List.filter 
      (fun anim -> name anim |> String.split_on_char '*' |> List.hd = dir)
      anims
  with e -> anims

let anim_from_name (anims : animation list) (n : string) = 
  try
    List.filter 
      (fun anim -> name anim = n)
      anims |> List.hd
  with e -> List.hd anims

let anim_from_dir_name (anims : animation list) (dir : string) (n : string) = 
  let n = dir^"*"^n in
  anim_from_name anims n

let frame anim i =
  (snd anim).(i)