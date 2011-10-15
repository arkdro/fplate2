module type PointSig = sig
  (* type point *)
  type t (* = int *) (* type exposed for debugging only *)
  val create    : int -> t
  val cmp       : t -> t -> bool
  val to_string : t -> string
end
module Point : PointSig = struct
  type t = int
  let create limit = Random.int limit
  let cmp p1 p2 = p1 = p2
  let to_string = string_of_int
end
