open Entity

type t = float * float

type s = int * int 

let to_int (x,y) = (int_of_float x, int_of_float y)

let from_int (x,y) = (float_of_int x, float_of_int y)

let floor (x,y) = (Float.floor x, Float.floor y)

let add (a,b) (c,d) = (a+.c,b+.d)

let greater (a,b) (c,d) = (a>c)&& (b>d)

let print (a,b) = (string_of_float a ^ "," ^ string_of_float b)

let add_ints (a,b) (c,d) = (a+c,b+d)

let vec_of_dir = function 
  |Up -> (0.,-1.)
  |Down ->  (0.,+1.)
  |Left ->  (-1.,0.)
  |Right -> (1.,0.)

let scale_vec sc (a,b)= (a*.sc,b*.sc) 

let subtract x y = add x (scale_vec (-1.) y)
