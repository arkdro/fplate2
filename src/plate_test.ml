(* --- tests for random filling using integer data ---------------------- *)
open Rinit
open Point
open Plate
module P1 = Plate (Point)
module Plate_test : sig
  val main : int -> int -> int -> int -> unit
end = struct
(* ---------------------------------------------------------------------- *)
  module Loop_test : sig
    val loop : P1.p -> int -> int -> int -> unit
  end = struct
    let loop data psz w h =
      let get_init_stat data x y =
        Printf.printf "get_init_stat, x=%d, y=%d\n" x y;
        let (_cnt, stat) = P1.get_stat data x y in
        Printf.printf "get_init_stat, stat:\n%s\n" (P1.c_to_string stat);
        stat
      in
      let get_next_cell data x y stat =
        IFDEF DEBUG THEN (
          Printf.printf "next cell start\n"
        ) ENDIF;
        let sums = P1.next_step_sums_sorted data x 0 stat in
        let (next_p, max) = match sums with
          | [] ->
            Printf.printf "no max found\n";
            Point.create psz, 0
          | (p, n) :: t ->
            p, n
        in
        IFDEF DEBUG THEN (
          Printf.printf "next cell res: %s, n=%d\n" (Point.to_string next_p) max
        ) ENDIF;
        next_p
      in
      let rec loop2 data cnt x y stat =
        Printf.printf "loop2 data: cnt=%d\n" cnt;
        match cnt with
          | 0 -> ()
          | _ ->
            let point = get_next_cell data x y stat in
            Printf.printf "loop2 point: %s\n" (Point.to_string point);
            let (res_cnt, res_stat) = P1.fill_step data x y point in
            IFDEF DEBUG THEN (
              Printf.printf "cur plate after fill_step:\n%s\n"
                (P1.to_string data);
              Printf.printf "cur plate res_stat:\n%s\n"
                (P1.c_to_string res_stat)
            ) ENDIF;
            Printf.printf "cur plate res_cnt: %d\n" res_cnt;
            if res_cnt = w * h then (
              Printf.printf "cur plate done: cnt=%d\n%s\n"
                res_cnt (P1.to_string data);
              ()
            ) else (
              loop2 data (cnt-1) x y res_stat
            )
      in
      Printf.printf "loop init:\npoint size = %d\n" psz;
      Printf.printf "%s\n" (P1.to_string data);
      let start_x = Random.int w in
      let start_y = 0 in
      let init_stat = get_init_stat data start_x start_y in
      Printf.printf "loop w=%d, x=%d, y=%d\n" w start_x start_y;
      loop2 data (w*psz*5) start_x start_y init_stat
  end
(* ---------------------------------------------------------------------- *)
  let main width height point_size spread_ratio =
    (* let _ = Rinit.r_init in *)
    Printf.printf "main: %d, %d, %d\n" width height point_size;
    let data = P1.gen point_size width height spread_ratio in
    Printf.printf "begin data:\n%s\n" (P1.to_string data);
    Loop_test.loop data point_size width height;
    Printf.printf "main end, data:\n%s\n" (P1.to_string data)
end
(* ---------------------------------------------------------------------- *)
