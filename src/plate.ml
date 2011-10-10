module type Item = sig
  type t
  val create : t -> t
  val to_string : t -> string
end

module Plate (Item : Item) : sig
  type a
  val gen : Item.t -> int -> int -> a
  val to_string : a -> string array array
end = struct
  type a = Item.t array array
  type t = Item.t
  let gen_line f psz len =
    Array.init len (fun _ -> f psz)
  let rec gen_matrix2 acc f psz w = function
    | 0 -> acc
    | h -> let row = gen_line f psz w in
           gen_matrix2 (row::acc) f psz w (h-1)
  let gen_matrix f psz w h = 
    let lst = gen_matrix2 [] f psz w h in
    Array.of_list lst
  let gen psz w h = gen_matrix Item.create psz w h
  let to_string_row row = Array.map Item.to_string row
  let to_string p = Array.map to_string_row p
  let rehash plate x y n =
    (* plate - plate, n - new value, (x;y) - coordinates *)
    ()
end
