(* ---------------------------------------------------------------------- *)
module type Item = sig
  type t
  val create : int -> t
  val to_string : t -> string
  val cmp : t -> t -> bool
end
(* ---------------------------------------------------------------------- *)
module Fill (Item : Item) = struct
  type 'a row = Norow | Row of 'a
  type 'a cell = Nocell | Cell of 'a
  let prev_row plate y =
    if y = 0 then
      Norow
    else
      Row plate.(y-1)
  let next_row plate y =
    if y >= (Array.length plate)-1 then
      Norow
    else
      Row plate.(y+1)
  let prev_cell row x =
    if x = 0 then
      Nocell
    else
      Cell row.(x-1)
  let next_cell row x =
    if x >= (Array.length row)-1 then
      Nocell
    else
      Cell row.(x+1)
  let rec find_leftmost row i data =
    match prev_cell row i with
      | Cell x when Item.cmp x data ->
        find_leftmost row (i-1) data
      | _ ->
        i
  let rec find_rightmost row i data =
    match next_cell row i with
      | Cell x when Item.cmp x data ->
        find_rightmost row (i+1) data
      | _ ->
        i
  let rec update_cells row left right new_data =
    if left > right then
      ()
    else
      begin
        row.(left) <- new_data;
        update_cells row (left+1) right new_data
      end
  let update_cur_row row i cur_data new_data =
    let left = find_leftmost row i cur_data in
    let right = find_rightmost row i cur_data in
    update_cells row left right new_data

    (* plate: plate, n: new item, (x;y): coordinates *)
  let fill_step plate x y new_data =
    let cur_data = plate.(y).(x) in
    let new_cur_row = update_cur_row plate.(y) x cur_data new_data
    in new_cur_row
end
(* ---------------------------------------------------------------------- *)
module Plate (Item : Item) : sig
  type a
  val gen : int -> int -> int -> a
  val to_string : a -> string
  val to_string_array : a -> string array array
  val fill_step : a -> int -> int -> Item.t -> unit
end = struct
  type a = Item.t array array
  type t = Item.t
  (* let cur_point_size x = ??? static var *)
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
  let to_string_array p = Array.map to_string_row p
  (* let str_row row = Array.map (fun x -> (Item.to_string x) ^ ";") row *)
  let row_to_string row =
    let a1 = to_string_row row in
    let lst = Array.to_list a1 in
    String.concat ";" lst
  let to_string p =                     (* array -> string *)
    let a1 = Array.map row_to_string p in
    let lst = Array.to_list a1 in
    (String.concat "\n" lst) ^ "\n"
  module F1 = Fill(Item)
  let fill_step plate x y n = F1.fill_step plate x y n

end
(* ---------------------------------------------------------------------- *)
