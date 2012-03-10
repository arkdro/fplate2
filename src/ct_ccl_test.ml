(* --- tests for ccl ---------------------------------------------------- *)
open Domains
open Point
open Plate
open Ccl
open Ct_ccl
module D1 = Domains (Point)
module P1 = Plate (Point)
module C1 = Ct_ccl (Point)
module Ct_ccl_test : sig
  val main : int -> int -> int -> int -> int -> int -> int -> unit
end = struct
(* ---------------------------------------------------------------------- *)
  module Loop_test : sig
    val loop : Point.t list -> int -> int -> int -> int -> int ->
      (* (Point.t list) * (int option Bigarray.Array2.t list) *)
      (Point.t list) * (unit list)
  end = struct
    let loop data psz w h ccl_type verbose =
      let points = Point.all psz in
      let res_ccl_list = match ccl_type with
        | 4 ->
          C1.ccl4 points w h data verbose
        | 8 ->
          C1.ccl8 points w h data verbose
        | _ ->
          C1.ccl6 points w h data verbose
      in
      points, res_ccl_list
  end
  (* ---------------------------------------------------------------------- *)
  let one_step width height point_size spread_ratio ccl_type verbose =
    if verbose > 0 then (
      Printf.printf "one_step: %d, %d, %d, %d\n%!"
        width height point_size ccl_type);
    let data = P1.gen point_size width height spread_ratio in
    if verbose > 2
    then (
      Printf.printf "begin data:\n";
      P1.dump_plate data
    );
    let data_list = P1.get_data_list data in
    let points, res = Loop_test.loop data_list point_size width
      height ccl_type verbose in
    IFDEF DOMAIN_DEBUG THEN (
      let domains = D1.create_domains points res in
      Printf.printf "one_step, fused domains:\n";
      D1.dump_plate domains
    ) ENDIF;
    if verbose > 0 then (
      Printf.printf "one_step end\n"
    )
  (* ---------------------------------------------------------------------- *)
  let main width height point_size spread_ratio ccl_type verbose loops =
    Printf.printf "main: %d, %d, %d, %d\n" width height point_size ccl_type;
    for i = 0 to loops-1 do
      Printf.printf "main: i=%d\n" i;
      one_step width height point_size spread_ratio ccl_type verbose
    done;
    Printf.printf "main end\n"
(* ---------------------------------------------------------------------- *)
end
