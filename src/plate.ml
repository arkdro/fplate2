(* ---------------------------------------------------------------------- *)
module type Item = sig
  type t
  val create : int -> t
  val create_uniq : t
  val to_string : t -> string
  val cmp : t -> t -> bool
  val add_push  : t -> t
  val set_iter: t -> int -> t
  val add_iter: t -> t
  val clean : t -> t

  type c_cnt
  type c_key = t
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
  let print_stat c =
    Printf.printf "c_stat:\n%s\n" (Item.c_to_string c)

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

  (* add edges to stat, put iter counter to edges *)
  let mark_edges iter row left right c_stat =
    let c_stat_left =(
      match prev_cell row left with
        | Nocell ->
          c_stat
        | Cell cell ->
          Item.c_inc cell c_stat)
    in
    let c_stat_right =(
      match next_cell row right with
        | Nocell ->
          c_stat_left
        | Cell cell ->
          Item.c_inc cell c_stat_left)
    in
    c_stat_right

  (* update cells in row in place: from left to right *)
  let update_cells iter row left right new_data (cnt, c_stat) =
    let rec aux_update_cells iter row left right idx new_data (cnt, c_stat) =
      if idx > right then (
        let new_c_stat = mark_edges iter row left right c_stat in
        Printf.printf "update_cells done, left=%d, right=%d, idx=%d, cnt=%d\n"
          left right idx cnt;
        print_row row;
        print_stat new_c_stat;
        (cnt, new_c_stat)
      ) else (
        row.(idx) <- new_data;
        let new_c_stat = c_stat in
        Printf.printf "update_cells:\n";
        print_row row;
        print_stat new_c_stat;
        aux_update_cells iter row left right (idx+1) new_data (cnt+1, new_c_stat)
      )
    in
    aux_update_cells iter row left right left new_data (cnt, c_stat)


  let clean_plate plate =
    let clean_row row =
      let rec aux_clean_row row = function
        | -1 -> ()
        | x ->
          row.(x) <- Item.clean row.(x);
          aux_clean_row row (x-1)
      in
      aux_clean_row row ((Array.length row)-1)
    in
    Array.iter clean_row plate

  (* if node color to north/south of n is target color, add node to queue *)
  let check_row_to_queue row r_y left right y target =
    let even y = (y mod 2) = 0
    in
    let check_cur_cell x y cell target =
      if Item.cmp target cell then
        [(x,y)]
      else
        []
    in
    let check_prev_cell row x y target =
      match prev_cell row x with
        | Cell c when Item.cmp target c ->
          [(x-1, y)]
        | _ ->
          []
    in
    let check_next_cell row x y target =
      match next_cell row x with
        | Cell c when Item.cmp target c ->
          [(x+1, y)]
        | _ ->
          []
    in
    let rec aux_check_row_to_queue row orig_y left right y target acc =
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
        aux_check_row_to_queue row orig_y (left+1) right y target (cur_add @ adj_add @ acc)
    in
    match row with
      | Norow -> []
      | Row a ->
        aux_check_row_to_queue a r_y left right y target []

  let find_cells_to_queue (iter, plate) target left right y =
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

  let fill_step_row (iter, plate) target x y new_data ((cnt, c_stat) as stat) =
    if Item.cmp plate.(y).(x) target then
      let row = plate.(y) in
      let left = find_leftmost row x target in
      let right = find_rightmost row x target in
      Printf.printf "fill_step_row: left=%d, right=%d\n" left right;
      Printf.printf "fill_step_row: row before update\n";
      print_row row;
      print_stat c_stat;
      let new_stat = update_cells !iter row left right new_data stat in
      let (add_cnt, new_c_stat) = new_stat in
      Printf.printf "fill_step_row: row after update, add=%d\n" add_cnt;
      print_row row;
      print_stat new_c_stat;
      plate.(y) <- row;
      let list = find_cells_to_queue (iter, plate) target left right y in
      list, (add_cnt, new_c_stat)
    else
      (
        Printf.printf "fill_step_row: x:y != target\n";
        [], (0, c_stat)
      )

  (* fill_step_loop -> [], Item.c_cnt *)
  let rec fill_step_loop (iter, plate) target new_item
      ((cnt, c_stat) as stat) cell_list =
    let aux_print a_cnt a_add_cnt a_add_cells a_new_c_stat =
      Printf.printf "fill_step_loop cnt=%d, add_cnt=%d, added list:\n"
        a_cnt a_add_cnt;
      List.iter print_coord a_add_cells;
      Printf.printf "\nnew_";
      print_stat a_new_c_stat
    in
    match cell_list with
      | [] -> [], stat
      | (x, y) :: t ->
        Printf.printf "fill_step_loop: x=%d, y=%d\n" x y;
        let (add_cells, new_stat) = fill_step_row (iter, plate) target x
          y new_item stat in
        let (add_cnt, new_c_stat) = new_stat in
        aux_print cnt add_cnt add_cells new_c_stat;
        let n_stat = (cnt + add_cnt, new_c_stat) in
        fill_step_loop (iter, plate) target new_item n_stat (t @ add_cells)

  let fill_step (iter, plate) x y new_item =
    Printf.printf "fill_step: x=%d, y=%d, new=%s\n" x y
      (Item.to_string new_item);
    iter := !iter + 1;
    let cur_cnt = 0 in
    let stat = (cur_cnt, Item.c_empty) in
    Printf.printf "fill_step cleaned\n";
    let target = plate.(y).(x) in
    if Item.cmp new_item target then
      (
        Printf.printf "fill_step target = replacement, exiting\n";
        stat
      )
    else
      let _, res_all_stat = fill_step_loop (iter, plate) target
        new_item stat [(x,y)] in
      let (res_cnt, res_c_stat) = res_all_stat in
      Printf.printf "fill_step result: iter=%d, cnt=%d,\nc_stat=\n%s\n"
        !iter res_cnt (Item.c_to_string res_c_stat);
      (res_cnt, res_c_stat)

  (* needs DEEP copy to operate on !!! *)
  let fill_step_count (iter, plate) x y =
    let new_plate = plate in
    let new_item = Item.create_uniq in
    let res = fill_step (iter, new_plate) x y new_item in
    res
end
(* ---------------------------------------------------------------------- *)
module Plate (Item : Item) : sig
  type p
  val gen : int -> int -> int -> p
  val to_string : p -> string
  val c_to_string : Item.c_cnt -> string
  val to_string_array : p -> string array array
 (* exposed for tests only *)
  val fill_step : p -> int -> int -> Item.t -> int * Item.c_cnt
  val stat : p -> int -> int -> int * Item.c_cnt
end = struct
  type a = Item.t array array
  type p = (int ref * a)
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
    (ref iter, plate)
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

  (* modifies iter and plate in place, returns filled statistic for plate *)
  let fill_step (iter, plate) x y n_item =
    let stat1 = Item.c_empty in
    let stat2 = Item.c_inc n_item stat1 in
    Printf.printf "\nfill_step stat2:\n%s\n" (Item.c_to_string stat2);

    let item = Item.set_iter n_item !iter in
    F1.fill_step (iter, plate) x y item
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
