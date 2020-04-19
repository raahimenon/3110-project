type image = Graphics.color array array
type animation = image array

let load_image f : image = 
  let in_image = try open_in f with _ -> failwith (f ^ " not found") in
  let rec read_file in_image accu : Graphics.color array list =
    try
      let accu2 =
        ( 
          input_line in_image
          |> String.trim
          |> String.split_on_char ' '
          |> Array.of_list
          |> Array.map 
            (fun x -> String.split_on_char ',' x 
                      |> List.map int_of_string
                      |> function |[r;g;b;a] -> 
                        if a <= 0 then Graphics.transp
                        else Graphics.rgb r g b |_ -> failwith "unformatted")
        ) :: accu in
      read_file in_image accu2

    with _ -> accu
  in
  match read_file in_image [] |> List.rev with
  | [] -> [||]
  | h::t -> h::t |> Array.of_list

let load_animation f : animation = 
  let files = Sys.readdir f in
  Array.map (fun file -> load_image (f^"/"^file)) files

let load_animations f : animation list = 
  let animations = Sys.readdir f |> Array.to_list in
  List.map (fun anim -> load_animation (f^"/"^anim)) animations

let next_frame i anim = 
  if i + 1 >= Array.length anim then anim.(0) else anim.(i+1)

let curr_frame i anim = anim.(i)

let im_to_str (im : image) : string = 
  let pix_to_str (pix : Graphics.color) : string =
    if pix = Graphics.transp then "0,0,0,0"
    else 
      let b = pix mod (16*16) in
      let g = (pix - b) / (16 * 16) mod (16 * 16) in
      let r = ((pix - b) / (16 * 16) - g)/(16*16) mod (16*16) in 
      string_of_int r ^ "," ^ string_of_int g ^ "," ^ string_of_int b ^",255" in

  let row_to_str (row : Graphics.color array) : string = 
    Array.map pix_to_str row |> Array.to_list |> String.concat " " in
  Array.map row_to_str im |> Array.to_list |> String.concat "\n"