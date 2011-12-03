(* ---------------------------------------------------------------------- *)
module type Item = sig
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
(* flood fill *)
module Fill (Item : Item) = struct
  type 'a row = Norow | Row of 'a
  type 'a cell = Nocell | Cell of 'a

  (* debug print *)
  let to_string_row row = Array.map Item.to_string row
  let to_string_array p = Array.map to_string_row p
  let coord_list_to_string lst =
    let lst1 = List.map (fun (x,y) -> Printf.sprintf "(%d, %d)" x y) lst in
    String.concat ";" lst1

  let row_to_string row =
    let a1 = to_string_row row in
    let lst = Array.to_list a1 in
    String.concat ";" lst
  let to_string (_, p) =
    let a1 = Array.map row_to_string p in
    let lst = Array.to_list a1 in
    (String.concat "\n" lst) ^ "\n"

  let print_row row =
    IFDEF DEBUG THEN
      Printf.printf "%s\n" (row_to_string row)
      ENDIF
  let print_coord (x, y) =
    IFDEF DEBUG THEN
      Printf.printf "(%d, %d)" x y
      ENDIF
  let print_stat c =
    IFDEF DEBUG THEN
      Printf.printf "c_stat:\n%s\n" (Item.c_to_string c)
      ENDIF

  let prev_row plate y =
    if y = 0 then
      Norow
    else (
      print_row plate.(y-1);
      Row plate.(y-1)
    )

  let next_row plate y =
    if y >= (Array.length plate)-1 then
      Norow
    else (
      print_row plate.(y+1);
      Row plate.(y+1)
    )

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

  let even y = (y mod 2) = 0

  (* get two cells (and their coordinates) from a row above the current
     row *)
  let up_cells plate x y =
    let x1, x2 =
      if even y then
        let x1 = x - 1 in
        let x2 = x in
        (x1, x2)
      else
        let x1 = x in
        let x2 = x + 1 in
        (x1, x2)
    in
    match prev_row plate y with
      | Norow -> ((Nocell, x1, y-1), (Nocell, x2, y-1))
      | Row row ->
        let c1, c2 =
          if even y then
            prev_cell row x1, prev_cell row (x+1) (* prev, cur *)
          else
            prev_cell row (x+1), next_cell row x1 (* cur, next *)
        in
        ((c1, x1, y-1), (c2, x2, y-1))

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

  (* add edges to stat, put iter counter to edges (changes in place) *)
  let mark_edges iter row left right c_stat =
    let c_stat_left =
      (
        match prev_cell row left with
          | Nocell ->
            c_stat
          | Cell cell ->
            row.(left-1) <- Item.set_iter cell iter;
            Item.c_inc_iter iter cell c_stat
      )
    in
    let c_stat_right =
      (
        match next_cell row right with
          | Nocell ->
            c_stat_left
          | Cell cell ->
            row.(right+1) <- Item.set_iter cell iter;
            Item.c_inc_iter iter cell c_stat_left
      )
    in
    c_stat_right

  (* update cells in row in place: from left to right *)
  let update_cells iter row left right new_data stat =
    let rec aux_update_cells iter row left right idx new_data (cnt, c_stat) =
      if idx > right then (
        IFDEF DEBUG THEN (
          Printf.printf "update_cells done, left=%d, right=%d, idx=%d, cnt=%d\n"
            left right idx cnt;
          Printf.printf "row before mark_edges\n";
          print_row row
        ) ENDIF;
        let new_c_stat = mark_edges iter row left right c_stat in
        IFDEF DEBUG THEN (
          Printf.printf "row after mark_edges\n";
          print_row row;
          print_stat new_c_stat
        ) ENDIF;
        (cnt, new_c_stat)
      ) else (
        row.(idx) <- new_data;
        IFDEF DEBUG THEN (
          Printf.printf "update_cells:\n";
          print_row row;
          print_stat c_stat
        ) ENDIF;
        aux_update_cells iter row left right (idx+1) new_data (cnt+1, c_stat)
      )
    in
    aux_update_cells iter row left right left new_data stat


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

  (* if node color to north/south of n is target color, add node to queue.
     Otherwise add statistic *)
  let check_row_to_queue row r_y left right y target stat =
    let even y = (y mod 2) = 0
    in
    let check_cur_cell row x y cell target c_stat =
      if Item.cmp_iter target cell then
        [(x,y)], c_stat
      else
        let new_cell = Item.copy_iter target cell in
        let new_stat = Item.c_inc_iter_target target cell c_stat in
        row.(x) <- new_cell;
        [], new_stat
    in
    let check_prev_cell row x y target c_stat =
      match prev_cell row x with
        | Cell c when Item.cmp_iter target c ->
          [(x-1, y)], c_stat
        | Cell c ->
          let new_cell = Item.copy_iter target c in
          let new_stat = Item.c_inc_iter_target target c c_stat in
          row.(x-1) <- new_cell;
          [], new_stat
        | _ ->
          [], c_stat
    in
    let check_next_cell row x y target c_stat =
      match next_cell row x with
        | Cell c when Item.cmp_iter target c ->
          [(x+1, y)], c_stat
        | Cell c ->
          let new_cell = Item.copy_iter target c in
          let new_stat = Item.c_inc_iter_target target c c_stat in
          row.(x+1) <- new_cell;
          [], new_stat
        | _ ->
          [], c_stat
    in
    let rec aux_check_row_to_queue row orig_y left right y target acc c_stat =
      if left > right then
        acc, c_stat
      else
        let (cur_add, new_c_stat) = check_cur_cell row left orig_y
          row.(left) target c_stat  in
        let (adj_add, adj_c_stat) =
          if even y then
            check_prev_cell row left orig_y target new_c_stat
          else
            check_next_cell row left orig_y target new_c_stat
        in
        aux_check_row_to_queue row orig_y (left+1) right y target
          (cur_add @ adj_add @ acc) adj_c_stat
    in
    match row with
      | Norow -> [], stat
      | Row a ->
        aux_check_row_to_queue a r_y left right y target [] stat

  let find_cells_to_queue (iter, plate) target left right y (_x, stat) =
    IFDEF DEBUG THEN (
      Printf.printf "find_cells_to_queue, target: %s\n" (Item.to_string target);
      print_stat stat;
      Printf.printf "find_cells_to_queue, prev row:\n"
    ) ENDIF;
    let prev_row = prev_row plate y in
    let (prev_cells, pr_stat) = check_row_to_queue prev_row (y-1) left
      right y target stat in
    IFDEF DEBUG THEN (
      Printf.printf "queued prev_cells: ";
      List.iter print_coord prev_cells;
      Printf.printf "\n";
      print_stat pr_stat;
      Printf.printf "find_cells_to_queue, next row:\n"
    ) ENDIF;
    let next_row = next_row plate y in
    let (next_cells, n_stat) = check_row_to_queue next_row (y+1) left
      right y target pr_stat in
    IFDEF DEBUG THEN (
      Printf.printf "queued next_cells: ";
      List.iter print_coord next_cells;
      Printf.printf "\n";
      print_stat n_stat
    ) ENDIF;
    prev_cells @ next_cells, (_x, n_stat)

  let fill_step_row (iter, plate) target x y new_data ((cnt, c_stat) as stat) =
    let update_plate_iter u_plt u_target list =
      IFDEF DEBUG THEN (
        let lst1 = List.map
          (fun (x, y) ->
            Printf.sprintf "%d, %d" x y
          )
          list
        in
        let lst2 = String.concat "; " lst1 in
        Printf.printf "update_plate_iter list:\n%s\n" lst2
      ) ENDIF;
      List.iter
        (fun (x1, y1) ->
          let new_cell = Item.copy_iter u_target u_plt.(y1).(x1) in
          u_plt.(y1).(x1) <- new_cell
        )
        list
    in
    IFDEF DEBUG THEN (
      Printf.printf "fill_step_row, target: %s\n" (Item.to_string target);
      Printf.printf "fill_step_row, plate:\n%s\n" (to_string (0, plate))
    ) ENDIF;
    if Item.cmp plate.(y).(x) target then
      let row = plate.(y) in
      let left = find_leftmost row x target in
      let right = find_rightmost row x target in
      IFDEF DEBUG THEN (
        Printf.printf "fill_step_row: left=%d, right=%d\n" left right;
        Printf.printf "fill_step_row: row before update\n";
        print_row row;
        print_stat c_stat
      ) ENDIF;
      let upd_stat = update_cells !iter row left right new_data stat in
      IFDEF DEBUG THEN (
        let (_add_cnt, _upd_c_stat) = upd_stat in
        Printf.printf "fill_step_row: row after update, add=%d\n" _add_cnt;
        print_row row;
        print_stat _upd_c_stat
      ) ENDIF;
      let (list, q_stat) = find_cells_to_queue (iter, plate) target
        left right y upd_stat in
      IFDEF DEBUG THEN (
        Printf.printf "update_plate_iter before:\n%s\n" (to_string (0, plate))
      ) ENDIF;
      update_plate_iter plate target list;
      IFDEF DEBUG THEN (
        Printf.printf "update_plate_iter after:\n%s\n" (to_string (0, plate))
      ) ENDIF;
      list, q_stat
    else
      (
        IFDEF DEBUG THEN (
          Printf.printf "fill_step_row: x:y != target\n"
        ) ENDIF;
        [], stat
      )

  (* fill_step_loop -> [], Item.c_cnt; updates plate in place *)
  let rec fill_step_loop (iter, plate) target new_item
      ((cnt, c_stat) as stat) cell_list =
    IFDEF DEBUG THEN (
      Printf.printf "fill_step_loop, target: %s\n" (Item.to_string target)
    ) ENDIF;
    let _aux_print a_cnt a_add_cells (a_add_cnt, a_new_c_stat) =
      Printf.printf "fill_step_loop cnt=%d, add_cnt=%d, added list:\n"
        a_cnt a_add_cnt;
      List.iter print_coord a_add_cells;
      Printf.printf "\nnew_";
      print_stat a_new_c_stat
    in
    match cell_list with
      | [] -> stat
      | (x, y) :: t ->
        IFDEF DEBUG THEN (
          Printf.printf "fill_step_loop: x=%d, y=%d\n" x y
        ) ENDIF;
        let (add_cells, new_stat) = fill_step_row
          (iter, plate) target x y new_item stat in
        IFDEF DEBUG THEN (
          _aux_print cnt add_cells new_stat
        ) ENDIF;
        fill_step_loop (iter, plate) target new_item new_stat
          (t @ add_cells)

  let fill_step (iter, plate) x y input_item =
    iter := !iter + 1;
    let new_item = Item.set_iter input_item !iter in
    IFDEF DEBUG THEN (
      Printf.printf "fill_step: x=%d, y=%d, new: %s\n" x y
        (Item.to_string new_item)
    ) ENDIF;
    let cur_cnt = 0 in
    let stat = (cur_cnt, Item.c_empty) in
    let target_0 = plate.(y).(x) in
    let target = Item.set_iter target_0 !iter in
    if Item.cmp new_item target then
      (
        IFDEF DEBUG THEN (
          Printf.printf "fill_step target = replacement, exiting\n"
        ) ENDIF;
        stat
      )
    else
      let (res_cnt, res_c_stat) = fill_step_loop (iter, plate) target
        new_item stat [(x,y)] in
      IFDEF DEBUG THEN
        Printf.printf "fill_step result: iter=%d, cnt=%d,\nc_stat=\n%s\n"
        !iter res_cnt (Item.c_to_string res_c_stat)
        ENDIF;
      (res_cnt, res_c_stat)

end
(* ---------------------------------------------------------------------- *)
(* plate generation *)
module Plate (Item : Item) : sig
  type p
  val gen : int -> int -> int -> int -> p
  val to_string : p -> string
  val c_to_string : Item.c_cnt -> string
  val to_string_array : p -> string array array
  (* exposed for tests only *)
  val fill_step : p -> int -> int -> Item.t -> int * Item.c_cnt
  val next_step_sums : p -> int -> int -> Item.c_cnt -> (Item.c_key * int) list
  val next_step_sums_sorted : p -> int -> int -> Item.c_cnt -> (Item.c_key * int) list
  val get_stat  : p -> int -> int -> int * Item.c_cnt
end = struct
  type a = Item.t array array
  type p = (int ref * a)
  type t = Item.t

  module F1 = Fill(Item)

  let to_string_row row = Array.map Item.to_string row
  let to_string_array (_, p) = Array.map to_string_row p
  (* let str_row row = Array.map (fun x -> (Item.to_string x) ^ ";") row *)
  let row_to_string row =
    let a1 = to_string_row row in
    let lst = Array.to_list a1 in
    let res = String.concat ";" lst in
    res
  let to_string (_, p) =
    let a1 = Array.map row_to_string p in
    let lst = Array.to_list a1 in
    (String.concat "\n" lst) ^ "\n"
  let c_to_string map =
    Item.c_to_string map

  (* iterate over the plate in an attemt to increase size of same
     color domains: using the ratio, assign a cell the same color as the
     random adjacent cell. Ratio 0 - no color duplication, 100 -
     always do duplication *)
  let rand_spread plate w h ratio =
    let adj_cells_cur_row row x y =
      let prev = F1.prev_cell row x in
      let next = F1.next_cell row x in
      [prev; next]
    in
    let adj_cells_adj_row in_row x y =
      match in_row with
        | F1.Norow -> []
        | F1.Row row ->
          let cur = F1.Cell row.(x) in
          let add =
            if (y mod 2) = 0 then F1.prev_cell row x
            else F1.next_cell row x
          in
          [cur; add]
    in
    (* returns list of cells (and/or Nocell's) that are adjacent to (x,y) *)
    let adj_list plate x y =
      if y >= Array.length plate then []
      else if y < 0 then []
      else
        let cur = adj_cells_cur_row plate.(y) x y in
        let prev = adj_cells_adj_row (F1.prev_row plate y) x y in
        let next = adj_cells_adj_row (F1.next_row plate y) x y in
        prev @ cur @ next
    in
    (* choose random cell from the list of (adjacent) cells *)
    let adj_random_select list =
      let f = function
        | F1.Nocell -> false
        | F1.Cell _ -> true
      in
      let cleared = List.filter f list in
      let len = List.length cleared in
      let rand = Random.int len in
      List.nth cleared rand
    in
    let rec change_row_aux plt x y = match x with
      | -1 -> ()
      | _ ->
        let list = adj_list plt x y in
        (
          match adj_random_select list with
            | F1.Cell cell ->
              if Random.int 100 < ratio then
                plt.(y).(x) <- cell
              else ()
            | _ -> ()
        );
        change_row_aux plt (x-1) y
    in
    let change_row plt y =
      change_row_aux plt (w-1) y in
    let rec change_plate_aux plt y = match y with
      | -1 -> ()
      | _ ->
        change_row plt y;
        change_plate_aux plt (y-1)
    in
    let change_plate plt = change_plate_aux plt (h-1) in
    (
      match ratio with
        | r when r <= 0 -> ()
        | r when r > 100 -> ()
        | _ -> change_plate plate
    );
    plate

  (* generate matrix based on cell create function, point size, width,
     height *)
  let gen_matrix f psz w h = 
    let gen_line f psz len = Array.init len (fun _ -> f psz)
    in
    let rec gen_matrix2 acc f psz w = function
      | 0 -> acc
      | h -> let row = gen_line f psz w in
             gen_matrix2 (row::acc) f psz w (h-1)
    in
    let lst = gen_matrix2 [] f psz w h in
    Array.of_list lst
      
  (* generate plate based on point size, width, height, domain ratio *)
  let gen psz w h ratio =
    IFDEF DEBUG THEN (
      Printf.printf "plate gen: psz=%d, w=%d, h=%d, r=%d\n" psz w h ratio
    ) ENDIF;
    let pure_plate = gen_matrix Item.create psz w h in
    IFDEF DEBUG THEN (
      Printf.printf "plate gen pure_plate1:\n%s\n"
        (to_string (ref 0, pure_plate))
    ) ENDIF;
    let plate = rand_spread pure_plate w h ratio in
    IFDEF DEBUG THEN (
      Printf.printf "plate gen pure_plate2:\n%s\n"
        (to_string (ref 0, pure_plate))
    ) ENDIF;
    let iter = -1 in
    (ref iter, plate)

  (* make a deep copy of a plate *)
  let deep_copy (iter, plate) =
    let deep_copy_row row = Array.copy row in
    let new_plate_outer = Array.copy plate in
    let new_plate_data = Array.map deep_copy_row new_plate_outer in
    (iter, new_plate_data)

  (* modifies iter and plate in place, returns filled statistic for plate *)
  let fill_step (iter, plate) x y n_item =
    let item = Item.set_iter n_item !iter in
    let _cnt, _c_stat = F1.fill_step (iter, plate) x y item in
    let tmp_item = Item.create_uniq in
    let tmp_data = deep_copy (iter, plate) in
    IFDEF DEBUG THEN (
      Printf.printf "point.fill_step step2\n"
    ) ENDIF;
    let (res_cnt, res_c_stat) = F1.fill_step tmp_data x y tmp_item in
    IFDEF DEBUG THEN (
      Printf.printf "point.fill_step step2 done\n"
    ) ENDIF;
    (res_cnt, res_c_stat)

  (* for every color in stat perform fill_step and count newly filled
     area. Then choose max of them *)
  let next_step_sums data x y stat =
    let f tmp_item =
      IFDEF DEBUG THEN (
        Printf.printf "next_step_sums, f, item: %s\n"
          (Item.to_string tmp_item)
      ) ENDIF;
      let tmp_data = deep_copy data in
      let (cnt, t_stat) = fill_step tmp_data x y tmp_item in
      IFDEF DEBUG THEN (
        Printf.printf "next_step_sums, f res, ";
        Printf.printf "item: %s, cnt=%d, stat:\n%s\n"
          (Item.to_string tmp_item) cnt (Item.c_to_string t_stat)
      ) ENDIF;
      (tmp_item, cnt)
    in
    let keys = Item.keys stat in
    List.map f keys

  (* get the list of colors for the next step, sort them, choose
     the max of them *)
  let next_step_sums_sorted data x y stat =
    let _print_list lst =
      let f (i, c) =
        Printf.sprintf "item: %s, sum=%d" (Item.to_string i) c
      in
      Printf.printf "next_step_sums_sorted list:\n";
      let lst2 = List.map f lst in
      let lst3 = String.concat "\n" lst2 in
      Printf.printf "%s\n" lst3
    in
    let lst = next_step_sums data x y stat in
    let res = List.sort (fun (_, a) (_, b) -> compare b a) lst in
    IFDEF DEBUG THEN (
      _print_list res
    ) ENDIF;
    res

  (* calculate filled area stats by filling it with unique item *)
  let get_stat data x y =
    let tmp_item = Item.create_uniq in
    let tmp_data = deep_copy data in
    F1.fill_step tmp_data x y tmp_item

  (* connected components labeling *)
  let label plate psz w h =
    let assign_same_label labels (src_x, src_y) (dst_x, dst_y) =
      let lab = labels.(src_y).(src_x) in
      labels.(dst_y).(dst_x) <- lab
    in
    let check_2 plate labels x y pcell =
      let (_up1, _up2) = F1.up_cells plate x y in
      ()
    in
    let check_1 plate labels x y =
      match F1.prev_cell plate.(y) x with
        | F1.Cell cell when Item.cmp cell plate.(y).(x) ->
          assign_same_label labels (x-1, y) (x, y)
        | pcell ->
          check_2 plate labels x y pcell
    in
    let iter_row plate labels row y =
      let rec iter_row_aux plate labels row x y =
        if x >= Array.length row then
          check_1 plate labels x y
        else (
          iter_row_aux plate labels row (x+1) y
        )
      in
      iter_row_aux plate labels row 0 y
    in
    let rec pass1 plate labels y =
      if y >= Array.length plate then
        labels
      else (
        iter_row plate labels plate.(y) y;
        pass1 plate labels (y+1)
      )
    in
    let (_, labels) = deep_copy (-1, plate) in
    pass1 plate labels 0

end
(* ---------------------------------------------------------------------- *)
