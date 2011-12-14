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
  type label = Empty | Label of int
  type coord = Coord_wrong | Coord_ok of int * int

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
      | _ -> assert false (* should not happen *)
    in
    List.map unpack filled

  let adj_fg_cells plate x y =
    let cell = plate.(y).(x) in
    let all = adj_cells plate x y in
    let f (c, cx, cy) = Item.cmp cell c in
    List.filter f all

  (* ccl for one particular cell (color) *)
  let labeling cell plate w h =
    let labels = Array.make_matrix w h Empty in
    let classes = Array.init (w*h) (fun i -> i) in
    let single_flags = Array.make (w*h) true in

    (* calculate coordinates of next cell *)
    let next_coord x y =
      if x >= w-1
      then if y >= h-1
        then Coord_wrong
        else Coord_ok (0, y+1)
      else Coord_ok (x+1, y)
    in

    let is_single = function
      | Label label ->
        let cls = classes.(label) in
        single_flags.(cls)
      | _ -> assert false               (* should not happen *)
    in

    let unpack_label = function
      | Label x -> x
      | _ -> assert false               (* should not happen *)
    in

    (* mark classes for equality. Input: cnt, [label] *)
    let mark_equiv label_cnt list =
      let martyr surv_class m =
        let idx = unpack_label m in
        classes.(idx) <- surv_class
      in

      (* mark martyrs with single item in class array *)
      let mark_s surv_class martyrs =
        List.iter (martyr surv_class) martyrs
      in

      (* run over class array and assign a survivor class to items
         that have class equal to the current martyr class *)
      let merging_classes surv_class martyr =
        let m_idx = unpack_label martyr in
        let m_class = classes.(m_idx) in
        let rec aux_merg = function
          | cnt when cnt = label_cnt ->
            ()
          | cnt when classes.(cnt) = m_class ->
            classes.(cnt) <- surv_class;
            aux_merg (cnt+1)
          | cnt ->
            aux_merg (cnt+1)
        in
        aux_merg 0
      in

      (* mark martyrs with several items in class array *)
      let mark_m surv_class martyrs =
        List.iter (merging_classes surv_class) martyrs
      in

      let single, not_single = List.partition is_single list in
      let martyrs_s, martyrs_m, survivor =
        match not_single with
          | [] ->
            List.tl single, [], List.hd single
          | h :: t ->
            single, t, h
      in
      let surv_label = unpack_label survivor in
      let surv_class = classes.(surv_label) in
      single_flags.(surv_class) <- false;
      mark_s surv_class martyrs_s;
      mark_m surv_class martyrs_m
    in

    (* choose the label from a list, mark classes for equality *)
    let choose_label label_cnt = function
      | (c, cx, cy) :: [] ->
        (
          match labels.(cy).(cx) with
            | (Label l) as ll -> ll
            | _ -> assert false (* should not happen *)
        )
      | list ->
        let f (_c, cx, cy) = labels.(cy).(cx) in
        let label_list = List.map f list in
        mark_equiv label_cnt label_list;
        let aux lst = List.hd lst in  (* stub *)
        let res_label = aux label_list in
        res_label
    in

    (* pass 1 of ccl *)
    let rec pass1 x y label_cnt =
      match next_coord x y with
        | Coord_wrong -> ()      (* pass 1 done *)
        | Coord_ok (x2, y2) ->
          match adj_fg_cells plate x y with
            | [] ->
              labels.(y).(x) <- Label label_cnt;
              pass1 x2 y2 (label_cnt + 1)
            | list ->
              let adj_label = choose_label label_cnt list in
              labels.(y).(x) <- adj_label;
              pass1 x2 y2 label_cnt
    in
    pass1 0 0 0
    

  (* do a connected component labeling *)
    let ccl avail_cells w h list =
      let plate = init_ccl_matrix w h list in
      let f cell = labeling plate cell w h in
      List.map f avail_cells
end
