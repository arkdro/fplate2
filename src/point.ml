module type PointSig = sig
  (* type point *)
  type t (* = int *) (* type exposed for debugging only *)
  val create    : int -> t
  val create_uniq : t
  val cmp       : t -> t -> bool
  val cmp_iter  : t -> t -> bool
  val to_string : t -> string
  val to_string2 : t -> string
  val add_push  : t -> t
  val set_iter: t -> int -> t
  val copy_iter: t -> t -> t
  val add_iter: t -> t
  val clean     : t -> t

  type c_cnt
  type c_key = t
  val c_empty : c_cnt
  val c_set   : c_key -> int -> c_cnt -> c_cnt
  val c_inc_n : c_key -> int -> c_cnt -> c_cnt
  val c_inc   : c_key -> c_cnt -> c_cnt
  val c_inc_iter        : int -> c_key -> c_cnt -> c_cnt
  val c_inc_iter_target : c_key -> c_key -> c_cnt -> c_cnt
  val c_to_string       : c_cnt -> string
  val separate_item     : c_key -> c_cnt -> (int * c_cnt)
  val get_max           : c_cnt -> (c_key * int)
  (* val map               : (int -> int) -> c_cnt -> c_cnt *)
  val keys              : c_cnt -> c_key list
end
module rec Point : PointSig = struct
  type t = { color: int;
             (* the following fields are used for debugging, optimization *)
             pushed: bool; (* true if pushed in queue for current iteration *)
             iter: int (* last involved iteration *)
           }
  let create limit = {color = Random.int limit; pushed = false; iter = -1}
  let cmp p1 p2 = p1.color = p2.color
  (* compare colors and check if target iteration is newer *)
  let cmp_iter {iter=iter_target; color=color_target}
      {iter=iter_cell; color=color_cell} =
    IFDEF DEBUG THEN
      Printf.printf "cmp_iter: (i=%d, c=%d), (i=%d, c=%d)\n"
      iter_target color_target iter_cell color_cell
      ENDIF;
    iter_target > iter_cell && color_target = color_cell
  let to_string2 point = string_of_int point.color
  let to_string point =
    "i=" ^ string_of_int point.iter ^ " " ^
      "c=" ^ string_of_int point.color ^ ""
  let clean p = {p with pushed=false; iter = -1}
  let add_push p = {p with pushed=true}
  let set_iter p iter = {p with iter = iter}
  let copy_iter src dest = {dest with iter = src.iter}
  let add_iter p = {p with iter = p.iter+1}
  let create_uniq = {color = -1; pushed = false; iter = -1}
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

  type c_key = t
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

  (* increment counter if current iteration newer than point iteration *)
  let c_inc_iter cur_iter ({iter=iter} as point) map =
    if cur_iter > iter then
      c_inc point map
    else
      map
  (* increment counter if target point iteration newer than point iteration *)
  let c_inc_iter_target {iter=target_iter} ({iter=iter} as point) map =
    if target_iter > iter then
      c_inc point map
    else
      map
  let c_to_string map =
    let item_to_string (k, v) =
      to_string k ^ ": " ^ string_of_int v
    in
    let lst = Cnt.bindings map in
    let str_list = List.map item_to_string lst in
    String.concat "\n" str_list
  (* let find item map = Cnt.find item map *)
  let separate_item item map =
    let cur_sum = Cnt.find item map in
    let new_stat = Cnt.remove item map in
    (cur_sum, new_stat)
  let get_max map =
    let f (_, a) (_, b) = compare b a in
    let list = Cnt.bindings map in
    let sorted = List.sort f list in
    List.hd sorted
  let map f map = Cnt.map f map
  let keys map =
    let lst = Cnt.bindings map in
    let res_lst, _ = List.split lst in
    res_lst

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

end
