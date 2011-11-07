module type PointSig = sig
  (* type point *)
  type t (* = int *) (* type exposed for debugging only *)
  val create    : int -> t
  val create_uniq : t
  val cmp       : t -> t -> bool
  val to_string : t -> string
  val add_push  : t -> t
  val add_iter: t -> t
  val clean     : t -> t

  type c_cnt
  type c_key
  val c_empty : c_cnt
  val c_set   : c_key -> int -> c_cnt -> c_cnt
  val c_inc_n : c_key -> int -> c_cnt -> c_cnt
  val c_inc   : c_key -> c_cnt -> c_cnt
end
module rec Point : PointSig = struct
  type t = { color: int;
             (* the following fields are used for debugging, optimization *)
             pushed: bool; (* true if pushed in queue for current iteration *)
             iter: int (* last involved iteration *)
           }
  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (* this part must be a separate functor *)
  (* work with map: color -> amount *)
  module type Cmp_sig = sig
    type t = Point.t
    val compare : t -> t -> int
  end
  module Cmp : Cmp_sig = struct
    type t = Point.t
    let compare a b = compare a.color b.color
  end

  module Cnt = Map.Make (Cmp)

  type c_key = Cmp.t
  type c_cnt = int Cnt.t
  let c_empty = Cnt.empty
  let c_set point count map = Cnt.add point count map
  let c_inc_n point increment map =
    match Cnt.mem point map with
      | true ->
        let cur_cnt = Cnt.find point map in
        Cnt.add point (cur_cnt + increment) map
      | false ->
        Cnt.add point increment map
  let c_inc point map = c_inc_n point 1 map
  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  let create limit = {color = Random.int limit; pushed = false; iter = 0}
  let cmp p1 p2 = p1.color = p2.color
  let to_string point = string_of_int point.color
  let clean p = {p with pushed=false; iter=0}
  let add_push p = {p with pushed=true}
  let add_iter p = {p with iter = p.iter+1}
  let create_uniq = {color = -1; pushed = false; iter = 0}
end
