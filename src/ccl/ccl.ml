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

  exception Row_length_mismatch
  exception Column_length_mismatch
  type 'a row = Norow | Row of 'a
  type 'a cell = Nocell | Cell of 'a

  (* fill a 2d matrix with data from a flat list *)
  let init_ccl_matrix w h list =
    let rec aux0 cnt acc_cells acc_rows = function
      | head :: tail when cnt > 0 ->
        aux0 (cnt-1) (head::acc_cells) acc_rows tail
      | (_ :: _) as rest ->
        if List.length acc_cells <> w
        then raise Row_length_mismatch
        else aux0 w [] (acc_cells::acc_rows) rest
      | [] ->
        let rows = acc_cells :: acc_rows in
        if List.length rows <> h
        then raise Column_length_mismatch
        else List.map (fun row -> List.rev row) rows
    in
    let lst2 = aux0 w [] [] list in
    let a1 = Array.of_list lst2 in
    Array.map (fun row -> Array.of_list row) a1

  let even x = (x mod 2) = 0

  let prev_row plate y =
    if y > 0 && y <= Array.length plate
    then Row plate.(y-1)
    else Norow

  let next_row plate y =
    if y >= -1 && y < (Array.length plate) - 1
    then Row plate.(y+1)
    else Norow

  let prev_cell row x =
    if x > 0 && x <= Array.length row
    then Cell row.(x-1)
    else Nocell

  let cur_cell row x =
    if x >= 0 && x < Array.length row
    then Cell row.(x)
    else Nocell

  let next_cell row x =
    if x >= -1 && x < (Array.length row) - 1
    then Cell row.(x+1)
    else Nocell

  (* returns cells and coordinates *)
  let up_cells plate x y =
    match prev_row plate y with
      | Norow -> []
      | Row row ->
        if even y
        then
          (* x-1; x *)
          [prev_cell row x, x-1, y; cur_cell row x, x, y]
        else
          (* x; x+1 *)
          [cur_cell row x, x, y; next_cell row x, x+1, y]

  let adj_cells plate x y =
    let prev = prev_cell plate.(y) x, x-1, y in
    let up = up_cells plate x y in
    let all = prev :: up in
    let f = function
      | Cell _, _, _ -> true
      | Nocell, _, _ -> false
    in
    let filled = List.filter f all in
    let unpack = function
      | Cell cell, x1, y1 -> cell, x1, y1
      | _ -> assert false
    in
    List.map unpack filled

  let adj_fg_cells plate x y =
    let cell = plate.(y).(x) in
    let all = adj_cells plate x y in
    let f (c, cx, cy) = Item.cmp cell c in
    List.filter f all

  let labeling cell plate =
    (* let adj = adj_fg_cells plate x y in *)
    ()


  (* do a connected component labeling *)
    let ccl avail_cells w h list =
      let plate = init_ccl_matrix w h list in
      let f cell = labeling plate cell in
      List.map f avail_cells
end
