(* ---------------------------------------------------------------------- *)
module type Item = sig
  type t
  val create : int -> t
  val create_uniq : t
  val to_string : t -> string
  val cmp : t -> t -> bool
  val add_push  : t -> t
  val add_iter: t -> t
  val clean : t -> t

  type c_cnt
  type c_key
  val c_empty : c_cnt
  val c_set   : c_key -> int -> c_cnt -> c_cnt
  val c_inc_n : c_key -> int -> c_cnt -> c_cnt
  val c_inc   : c_key -> c_cnt -> c_cnt
  val c_to_string : c_cnt -> string
end
(* ---------------------------------------------------------------------- *)
module Fill (Item : Item) = struct
  type 'a row = Norow | Row of 'a
  type 'a cell = Nocell | Cell of 'a

  let to_string_row row = Array.map Item.to_string row
  let to_string_array p = Array.map to_string_row p
  (* let str_row row = Array.map (fun x -> (Item.to_string x) ^ ";") row *)
  let row_to_string row =
    let a1 = to_string_row row in
    let lst = Array.to_list a1 in
    String.concat ";" lst
  let print_row row = Printf.printf "%s\n" (row_to_string row)
  let print_coord (x, y) = Printf.printf "(%d, %d)" x y

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
  let rec update_cells row left right new_data ((iter, cnt, c_stat) as stat) =
    if left > right then
      stat
    else
      begin
        row.(left) <- new_data;
        update_cells row (left+1) right new_data (iter, (cnt+1), c_stat)
      end
  let rec clean_row2 row = function
    | -1 -> ()
    | x ->
      row.(x) <- Item.clean row.(x);
      clean_row2 row (x-1)
  let clean_row row =
    clean_row2 row ((Array.length row)-1)
  let clean_plate plate =
    Array.iter clean_row plate
  let even y = (y mod 2) = 0
  let check_cur_cell x y cell target =
      if Item.cmp target cell then
        [(x,y)]
      else
        []
  let check_prev_cell row x y target =
    match prev_cell row x with
      | Cell c when Item.cmp target c ->
        [(x-1, y)]
      | _ ->
        []
  let check_next_cell row x y target =
    match next_cell row x with
      | Cell c when Item.cmp target c ->
        [(x+1, y)]
      | _ ->
        []
  let rec check_row_to_queue2 row orig_y left right y target acc =
    if left > right then
      acc
    else
      let cur_add = check_cur_cell left orig_y row.(left) target in
      let adj_add =
        if even y then
          check_prev_cell row left orig_y target
        else
          check_next_cell row left orig_y target
      in
      check_row_to_queue2 row orig_y (left+1) right y target (cur_add @ adj_add @ acc)

  let check_row_to_queue row r_y left right y target =
    match row with
      | Norow -> []
      | Row a ->
        check_row_to_queue2 a r_y left right y target []
  let find_cells_to_queue plate target left right y =
    let prev_row = prev_row plate y in
    let prev_cells = check_row_to_queue prev_row (y-1) left right y target in
    Printf.printf "queued prev_cells: ";
    List.iter print_coord prev_cells;
    Printf.printf "\n";
    let next_row = next_row plate y in
    let next_cells = check_row_to_queue next_row (y+1) left right y target in
    Printf.printf "queued next_cells: ";
    List.iter print_coord next_cells;
    Printf.printf "\n";
    prev_cells @ next_cells

  let fill_step_row plate target x y new_data ((iter, cnt, c_stat) as stat) =
    if Item.cmp plate.(y).(x) target then
      let row = plate.(y) in
      let left = find_leftmost row x target in
      let right = find_rightmost row x target in
      Printf.printf "fill_step_row: left=%d, right=%d\n" left right;
      Printf.printf "fill_step_row: row before update\n";
      print_row row;
      let new_stat = update_cells row left right new_data stat in
      let (_, add_cnt, new_c_stat) = new_stat in
      Printf.printf "fill_step_row: row after update, add=%d\n" add_cnt;
      print_row row;
      plate.(y) <- row;
      let list = find_cells_to_queue plate target left right y in
      list, (iter, add_cnt, new_c_stat)
    else
      (
        Printf.printf "fill_step_row: x:y != target\n";
        [], (iter, 0, c_stat)
      )
  (* cnt: counter of matching items for current iteration *)
  let rec fill_step_loop plate target new_item ((iter, cnt, c_stat) as stat) = function
    | [] -> [], stat
    | (x, y) :: t ->
      Printf.printf "fill_step_loop: x=%d, y=%d\n" x y;
      let (add_cells, new_stat) = fill_step_row plate target x y new_item stat in
      let (_, add_cnt, new_c_stat) = new_stat in
      Printf.printf "fill_step_loop add_cnt=%d, added list:\n" add_cnt;
      List.iter print_coord add_cells;
      Printf.printf "\n";
      let n_stat = (iter, cnt + add_cnt, new_c_stat) in
      fill_step_loop plate target new_item n_stat (t @ add_cells)

  let fill_step (prev_iter, plate) x y new_item =
    Printf.printf "fill_step: x=%d, y=%d, new=%s\n" x y
      (Item.to_string new_item);
    let iter = prev_iter + 1 in
    let cur_cnt = 0 in
    let stat = (iter, cur_cnt, Item.c_empty) in
    Printf.printf "fill_step cleaned\n";
    let target = plate.(y).(x) in
    if Item.cmp new_item target then
      (
        Printf.printf "fill_step target = replacement, exiting\n";
        stat
      )
    else
      let _, res_all_stat = fill_step_loop plate target new_item stat [(x,y)] in
      let (_, res_cnt, res_c_stat) = res_all_stat in
      Printf.printf "fill_step result: iter=%d, cnt=%d,\nc_stat=%s\n"
        iter res_cnt (Item.c_to_string res_c_stat);
      (iter, res_cnt, res_c_stat)

  (* needs DEEP copy to operate on !!! *)
  let fill_step_count (iter, plate) x y =
    let new_plate = plate in
    let new_item = Item.create_uniq in
    let res = fill_step (iter, new_plate) x y new_item in
    res
end
(* ---------------------------------------------------------------------- *)
module Plate (Item : Item) : sig
  type a
  val gen : int -> int -> int -> a
  val to_string : a -> string
  val c_to_string : Item.c_cnt -> string
  val to_string_array : a -> string array array
 (* exposed for tests only *)
  val fill_step : a -> int -> int -> Item.t -> int * int * Item.c_cnt
  val stat : a -> int -> int -> int * int * Item.c_cnt
end = struct
  type a = (int * Item.t array array)
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
  let gen psz w h =
    Printf.printf "plate gen: psz=%d, w=%d, h=%d\n" psz w h;
    let plate = gen_matrix Item.create psz w h in
    let iter = -1 in
    (iter, plate)
  let to_string_row row = Array.map Item.to_string row
  let to_string_array (_, p) = Array.map to_string_row p
  (* let str_row row = Array.map (fun x -> (Item.to_string x) ^ ";") row *)
  let row_to_string row =
    let a1 = to_string_row row in
    let lst = Array.to_list a1 in
    String.concat ";" lst
  let to_string (_, p) =
    let a1 = Array.map row_to_string p in
    let lst = Array.to_list a1 in
    (String.concat "\n" lst) ^ "\n"
  let c_to_string map =
    Item.c_to_string map
  module F1 = Fill(Item)
  let fill_step plate x y n = F1.fill_step plate x y n
  let deep_copy (iter, plate) =
    let deep_copy_row row = Array.copy row in
    let new_plate_outer = Array.copy plate in
    let new_plate_data = Array.map deep_copy_row new_plate_outer in
    (iter, new_plate_data)

  (* counts items of the same color starting from x,y *)
  let stat plate x y =
    let temp_plate = deep_copy plate in
    F1.fill_step_count temp_plate x y

end
(* ---------------------------------------------------------------------- *)
