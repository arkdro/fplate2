(* --- tests for ccl ---------------------------------------------------- *)
open Point
open Plate
open Ccl
module P1 = Plate (Point)
module C1 = Ccl (Point)
module Ccl_test : sig
  val main : int -> int -> int -> int -> int -> unit
end = struct
(* ---------------------------------------------------------------------- *)
  module Loop_test : sig
    val loop : Point.t list -> int -> int -> int -> int -> unit
  end = struct
    let loop data psz w h ccl_test =
      let points = Point.all psz in
      let res_ccl_list = C1.ccl points w h data in
      Printf.printf "ccl_test, loop, ccl result:\n";
      let f ccl_item = C1.dump_one_ccl w h ccl_item in
      List.iter f res_ccl_list
  end
(* ---------------------------------------------------------------------- *)
  let main width height point_size spread_ratio ccl_test =
    (* let _ = Rinit.r_init in *)
    Printf.printf "main: %d, %d, %d, %d\n" width height point_size ccl_test;
    let data = P1.gen point_size width height spread_ratio in
    Printf.printf "begin data:\n%s\n" (P1.to_string data);
    let data_list = P1.get_data_list data in
    Loop_test.loop data_list point_size width height ccl_test;
    Printf.printf "main end\n"
end
(* ---------------------------------------------------------------------- *)
