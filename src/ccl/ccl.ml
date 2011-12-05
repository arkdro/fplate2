(* connected component labeling
   based on (iciap99lu.pdf)
   "A simple and efficient connected component labeling algorithm"
   by Luidgi Di Stefano, Andrea Bulgarelli
 *)
(* ---------------------------------------------------------------------- *)
module type ItemSig = sig
  type t
  val create      : int -> t
  val create_uniq : t
  val to_string   : t -> string
  val to_string2   : t -> string
  val cmp         : t -> t -> bool
  val cmp_iter    : t -> t -> bool
  val add_push    : t -> t
  val set_iter    : t -> int -> t
  val copy_iter   : t -> t -> t
  val add_iter    : t -> t
  val clean       : t -> t

  type c_cnt
  type c_key = t
  val c_empty     : c_cnt
  val c_set       : c_key -> int -> c_cnt -> c_cnt
  val c_inc_n     : c_key -> int -> c_cnt -> c_cnt
  val c_inc       : c_key -> c_cnt -> c_cnt
  val c_inc_iter  : int -> c_key -> c_cnt -> c_cnt
  val c_inc_iter_target : c_key -> c_key -> c_cnt -> c_cnt
  val c_to_string : c_cnt -> string
  val separate_item     : c_key -> c_cnt -> (int * c_cnt)
  val get_max           : c_cnt -> (c_key * int)
  val keys              : c_cnt -> c_key list
end
(* ---------------------------------------------------------------------- *)
module Ccl (Item : ItemSig) = struct

end
