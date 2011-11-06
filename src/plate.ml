(* ---------------------------------------------------------------------- *)
module type Item = sig
  type t
  val create : int -> t
  val create_uniq : t
  val to_string : t -> string
  val cmp : t -> t -> bool
  val add_push  : t -> t
  val add_amount: t -> t
  val clean : t -> t
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
  let rec update_cells row left right new_data cnt =
    if left > right then
      cnt
    else
      begin
        row.(left) <- new_data;
        update_cells row (left+1) right new_data (cnt+1)
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
  (* cnt: counter of matching items for current iteration *)
  let fill_step_row plate target x y new_data =
    if Item.cmp plate.(y).(x) target then
      let row = plate.(y) in
      let left = find_leftmost row x target in
      let right = find_rightmost row x target in
      Printf.printf "fill_step_row: left=%d, right=%d\n" left right;
      Printf.printf "fill_step_row: row before update\n";
      print_row row;
      let add_cnt = update_cells row left right new_data 0 in
      Printf.printf "fill_step_row: row after update, add=%d\n" add_cnt;
      print_row row;
      plate.(y) <- row;
      let list = find_cells_to_queue plate target left right y in
      list, add_cnt
    else
      (
        Printf.printf "fill_step_row: x:y != target\n";
        [], 0
      )
  (* cnt: counter of matching items for current iteration *)
  let rec fill_step_loop plate target new_item cnt = function
    | [] -> [], cnt
    | (x, y) :: t ->
      Printf.printf "fill_step_loop: x=%d, y=%d\n" x y;
      let (add, add_cnt) = fill_step_row plate target x y new_item in
      Printf.printf "fill_step_loop add_cnt=%d, added list:\n" add_cnt;
      List.iter print_coord add;
      Printf.printf "\n";
      fill_step_loop plate target new_item (cnt + add_cnt) (t @ add)
  (* plate: plate, n: new item, (x;y): coordinates *)
  let fill_step plate x y new_item =
    Printf.printf "fill_step: x=%d, y=%d, new=%s\n" x y (Item.to_string new_item);
    clean_plate plate;
    Printf.printf "fill_step cleaned\n";
    let target = plate.(y).(x) in
    if Item.cmp new_item target then
      (
        Printf.printf "fill_step target = replacement, exiting\n";
        0
      )
    else
      let _, cnt = fill_step_loop plate target new_item 0 [(x,y)] in
      Printf.printf "fill_step result cnt=%d\n" cnt;
      cnt
  let fill_step_count plate x y =
    let new_plate = plate in            (* need DEEP copy !!! *)
    let new_item = Item.create_uniq in
    let res = fill_step new_plate x y new_item in
    res
end
(* ---------------------------------------------------------------------- *)
module Plate (Item : Item) : sig
  type a
  val gen : int -> int -> int -> a
  val to_string : a -> string
  val to_string_array : a -> string array array
  val fill_step : a -> int -> int -> Item.t -> int (* exposed for tests only *)
  val stat : a -> int -> int -> int
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
  let gen psz w h =
    Printf.printf "plate gen: psz=%d, w=%d, h=%d\n" psz w h;
    gen_matrix Item.create psz w h
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
  let deep_copy plate =
    let deep_copy_row row = Array.copy row in
    let new_plate = Array.copy plate in
    Array.map deep_copy_row new_plate

  (* counts available items starting from x,y *)
  let stat plate x y =
    let temp_plate = deep_copy plate in
    F1.fill_step_count temp_plate x y

end
(* ---------------------------------------------------------------------- *)
