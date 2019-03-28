(** Working with colors *)
(* color == R * 256 * 256  +  G * 256  +  B  *)
# let from_rgb (c : Graphics.color)  = 
   let r = c / 65536  and  g = c / 256 mod 256  and  b = c mod 256 
   in (r,g,b);;
val from_rgb : Graphics.color -> int * int * int = <fun>
# let inv_color (c : Graphics.color) = 
    let (r,g,b) = from_rgb c 
    in Graphics.rgb (255-r) (255-g) (255-b);;
val inv_color : Graphics.color -> Graphics.color = <fun>

(** Drawing a polygon *)

# let draw_rect x0 y0 w h = 
   let (a,b) = Graphics.current_point() 
   and x1 = x0+w and y1 = y0+h 
   in
     Graphics.moveto x0 y0; 
     Graphics.lineto x0 y1; Graphics.lineto x1 y1;  
     Graphics.lineto x1 y0; Graphics.lineto x0 y0; 
     Graphics.moveto a b;;
val draw_rect : int -> int -> int -> int -> unit = <fun>

# let draw_poly r =
   let (a,b) = Graphics.current_point () in 
   let (x0,y0) = r.(0) in Graphics.moveto x0 y0; 
     for i = 1 to (Array.length r)-1 do
       let (x,y) = r.(i) in Graphics.lineto x y
     done;
     Graphics.lineto x0 y0;
     Graphics.moveto a b;;
val draw_poly : (int * int) array -> unit = <fun>

