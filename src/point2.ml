module type Point2 = sig
  (* type point *)
  type t = float (* type exposed for debugging only *)
  val create : float -> t
  val cmp    : t -> t -> bool
  val to_string : t -> string
end
module Point2 : Point2 = struct
  type t = float
  let create limit = Random.float limit
  let cmp p1 p2 = p1 = p2
  let to_string = string_of_float
end
