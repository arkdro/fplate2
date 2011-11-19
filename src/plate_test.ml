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
    val loop : P1.p -> int -> int -> int -> unit
  end = struct
    let loop data psz w h =
      let rec loop2 data point cnt x =
        Printf.printf "loop2 data: cnt=%d\n" cnt;
        Printf.printf "loop2 point: %s\n" (Point.to_string point);
        match cnt with
          | 0 -> ()
          | _ ->
            let (res_cnt, res_stat) = P1.fill_step data x 0 point in
            Printf.printf "cur plate after fill_step:\n%s\n" (P1.to_string data);
            Printf.printf "cur plate res_cnt: %d\n" res_cnt;
            Printf.printf "cur plate res_stat:\n%s\n" (P1.c_to_string res_stat);
            if res_cnt = w * h then (
              Printf.printf "cur plate done: cnt=%d\n%s\n"
                res_cnt (P1.to_string data);
              ()
            ) else (
              let sums = P1.next_step_sums_sorted data x 0 res_stat in
              let (next_p, max) = match sums with
                | [] ->
                  Printf.printf "no max found\n";
                  Point.create psz, 0
                | (p, n) :: t ->
                  p, n
              in
              Printf.printf "next cell: %s, n=%d\n"
                (Point.to_string next_p) max;
              loop2 data next_p (cnt-1) x
            )
      in
      Printf.printf "loop init:\npoint size = %d\n" psz;
      Printf.printf "%s\n" (P1.to_string data);
      let start_x = Random.int w in
      Printf.printf "loop w=%d, x=%d\n" w start_x;
      let point = Point.create psz in
      loop2 data point (w*psz) start_x
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
