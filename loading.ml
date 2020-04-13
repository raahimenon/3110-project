type pixel_matrix = Graphics.color array array

let background = Graphics.black

let load_image fname : pixel_matrix =
  let in_image = try open_in fname with _ -> failwith (fname ^ " not found") in
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

let im_to_str (im:pixel_matrix) : string = 
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
