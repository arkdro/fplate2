module type PointSig = sig
  (* type point *)
  type t (* = int *) (* type exposed for debugging only *)
  val create    : int -> t
  val cmp       : t -> t -> bool
  val to_string : t -> string
end
module Point : PointSig = struct
  type t = { color: int;
             (* the following fields are used for debugging *)
             pushed: bool; (* true if pushed in queue for current iteration *)
             amount: int (* a number of pushes in queue for current iteration *)
           }
  let create limit = {color = Random.int limit; pushed = false; amount = 0}
  let cmp p1 p2 = p1.color = p2.color
  let to_string point = string_of_int point.color
end
