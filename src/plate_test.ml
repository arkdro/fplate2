(* --- tests for random filling using integer data ---------------------- *)
open Rinit
open Point
open Plate
module P1 = Plate (Point)
module Plate_test : sig
  val main : int -> int -> int -> unit
end = struct
(* ---------------------------------------------------------------------- *)
  module Loop_test : sig
    val loop : P1.a -> int -> int -> int -> unit
  end = struct
    let loop data psz w h =
      let rec loop2 data psz cnt x =
        let point = Point.create psz in
        Printf.printf "loop2 data: cnt=%d\n" cnt;
        Printf.printf "point=%s\n" (Point.to_string point);
        match cnt with
          | 0 -> ()
          | _ ->
            let _ = P1.fill_step data x 0 point in
            Printf.printf "cur plate after fill_step:\n%s\n" (P1.to_string data);
            let stat_cnt = P1.stat data x 0 in
            Printf.printf "cur plate stat: %d\n" stat_cnt;
            Printf.printf "cur plate after fill_step_count:\n%s\n"
              (P1.to_string data);
            (
              if stat_cnt = w * h then
                ()
              else
                loop2 data psz (cnt-1) x
            )
      in
      Printf.printf "loop init:\npoint size = %d\n" psz;
      Printf.printf "%s\n" (P1.to_string data);
      let start_x = Random.int w in
      Printf.printf "loop w=%d, x=%d\n" w start_x;
      loop2 data psz (w*psz) start_x
  end
(* ---------------------------------------------------------------------- *)
  let main width height point_size =
    (* let _ = Rinit.r_init in *)
    Printf.printf "main: %d, %d, %d\n" width height point_size;
    let data = P1.gen point_size width height in
    Printf.printf "begin data:\n%s\n" (P1.to_string data);
    Loop_test.loop data point_size width height;
    Printf.printf "main end, data:\n%s\n" (P1.to_string data)
end
(* ---------------------------------------------------------------------- *)
