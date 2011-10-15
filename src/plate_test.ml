(* --- tests for random filling using integer data ---------------------- *)
open Rinit
open Point
open Plate
module P1 = Plate (Point)
module Plate_test : sig
  val main : unit
end = struct
(* ---------------------------------------------------------------------- *)
  module Loop_test : sig
    val loop : P1.a -> int -> int -> int -> unit
  end = struct
    let rec loop2 data psz cnt x =
      let point = Point.create psz in
      Printf.printf "loop2 data: cnt=%d\n" cnt;
      Printf.printf "point=%s\n" (Point.to_string point);
      match cnt with
        | 0 -> ()
        | _ -> P1.fill_step data x 0 point;
          Printf.printf "%s\n" (P1.to_string data);
          loop2 data psz (cnt-1) x

    let loop data psz w h =
      Printf.printf "loop init:\npoint size = %d\n" psz;
      Printf.printf "%s\n" (P1.to_string data);
      let start_x = Random.int w in
      Printf.printf "loop w=%d, x=%d\n" w start_x;
      loop2 data psz (w*psz) start_x;
      Printf.printf "loop end data:\n%s\n" (P1.to_string data)
  end
(* ---------------------------------------------------------------------- *)
  let main =
    let point_size = 5 in
    let width = 30 in
    let height = 3 in
    (* let _ = Rinit.r_init in *)
    let data = P1.gen point_size width height in
    Loop_test.loop data point_size width height
end
(* ---------------------------------------------------------------------- *)
