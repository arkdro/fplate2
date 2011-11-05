open Rinit
open Point
(* open Point2 *)
open Plate
module P1 = Plate (Point)
(* module P2 = Plate (Point2) *)

let main () =
	let point_size = 8 in
	let width = 3 in
	let height = 3 in
	let _ = Rinit.r_init in
	let res = P1.gen point_size width height in
	(* let res = P2.gen point_size width height in *)
 	P1.to_string res 

let _ = main ()
