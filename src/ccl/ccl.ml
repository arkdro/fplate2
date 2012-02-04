(* connected components labeling
 * based on (iciap99lu.pdf)
 * "A simple and efficient connected components labeling algorithm"
 * by Luidgi Di Stefano, Andrea Bulgarelli
 *)

open Next_item.Next_item

(* ---------------------------------------------------------------------- *)
module type ItemSig = sig
  type t
  val cmp         : t -> t -> bool
    IFDEF DEBUG THEN
  val to_string   : t -> string
    ENDIF
end
(* ---------------------------------------------------------------------- *)
module Ccl (Item : ItemSig) = struct

  exception Row_length_mismatch
  exception Column_length_mismatch
  type label = Empty | Label of int
  type coord = Coord_wrong | Coord_ok of int * int

      IFDEF DEBUG THEN
  let dump_last_label = function
    | None -> ()
    | Some cnt -> Printf.printf "label cnt: %d\n" cnt

  let dump_cell = function
    | None -> ()
    | Some cell -> Printf.printf "cell: %s\n" (Item.to_string cell)

  let dump_cell2 = function
        | (Nocell, x, y) ->
          Printf.printf "%d, %d, no cell\n" x y
        | (Cell c, x, y) ->
          Printf.printf "%d, %d, %s\n" x y (Item.to_string c)

  let string_of_sflags w h flags =
      let bstr = function
        | true -> "t"
        | false -> "f"
      in
      let f (str, i) x =
        let sep =
          if ((i+1) mod w) = 0 && i > 0
          then "\n"
          else " "
        in (str ^ bstr x ^ sep, i + 1)
      in
      let (res_str, _) = Array.fold_left f ("", 0) flags in
      res_str

  let dump_sflags w h f =
    let res = match f with
      | None -> ""
      | Some flags ->
        string_of_sflags w h flags
    in
    Printf.printf "single flags:\n%s\n" res

  let string_of_classes w h classes =
    let cstr c =
      let pwidth = String.length (string_of_int (w * h)) in
      Printf.sprintf "%*d" pwidth c
    in
    let f (str, i) x =
      let sep =
        if ((i+1) mod w) = 0 && i > 0
        then "\n"
        else " "
      in (str ^ cstr x ^ sep, i + 1)
    in
    let (res_str, _) = Array.fold_left f ("", 0) classes in
    res_str

  let dump_classes w h c =
    let res = match c with
      | None -> ""
      | Some classes ->
        string_of_classes w h classes
    in
    Printf.printf "classes:\n%s\n" res

  let string_of_labels w h l =
    let pwidth = String.length (string_of_int (w * h)) in
    let str_one_label = function
      | Empty ->
        Printf.sprintf "%*s" pwidth "_"
      | Label x ->
        Printf.sprintf "%*d" pwidth x
    in
    let str_row row =
      let l_list = Array.to_list row in
      let str_list = List.map str_one_label l_list in
      String.concat " " str_list
    in
    let rows = Array.to_list l in
    let rows_str = List.map str_row rows in
    String.concat "\n" rows_str

  let dump_labels w h labels =
    let res = match labels with
      | None -> ""
      | Some l ->
        string_of_labels w h l
    in
    Printf.printf "labels:\n%s\n" res

  let string_of_label_list list =
    let str_list =
      List.map (
        function x -> match x with
          | Empty ->
            Printf.sprintf "_"
          | Label l ->
            Printf.sprintf "%d" l
      ) list
    in
    String.concat " " str_list

  let dump_label_list list =
    Printf.printf "%s\n" (string_of_label_list list)
      ENDIF

  let dump_one_ccl w h data = ()
  let dump_one_ccl2 w h data =
    let pwidth = String.length (string_of_int (w * h)) in
    let str_one_cell = function
      | None ->
        Printf.sprintf "%*s" pwidth "_"
      | Some x -> 
        Printf.sprintf "%*d" pwidth x
    in
    let str_row row =
      let row_list = Array.to_list row in
      let str_list = List.map str_one_cell row_list in
      String.concat " " str_list
    in
    Printf.printf "dump_one_ccl, labels:\n";
    let p_row row =
      let str = str_row row in
      Printf.printf "%s\n" str
    in
    Array.iter p_row data

      IFDEF DEBUG THEN
  let dump_all ?(labels = None) ?(classes = None)
      ?(flags = None) ?(cell = None) ?(cnt = None) w h =
    dump_cell cell;
    dump_last_label cnt;
    dump_labels w h labels;
    dump_classes w h classes;
    dump_sflags w h flags
      ENDIF

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
        else
          let rv1 = List.map (fun row -> List.rev row) rows in
          List.rev rv1
    in
    let lst2 = aux0 w [] [] list in
    let a1 = Array.of_list lst2 in
    Array.map (fun row -> Array.of_list row) a1

  let even x = (x mod 2) = 0

  (* returns cells and coordinates *)
  let up_cells conn_ways plate x y =
    match prev_row plate y with
      | Norow -> []
      | Row row when conn_ways = 4 ->   (* 4-connectivity *)
        [(cur_cell row x, x, y-1)]
      | Row row when conn_ways = 8 ->   (* 8-connectivity *)
        [prev_cell row x, x-1, y-1;
         cur_cell row x, x, y-1;
         next_cell row x, x+1, y-1]
      | Row row ->                      (* 6-connectivity, default *)
        if even y
        then
          (* x-1; x *)
          [prev_cell row x, x-1, y-1; cur_cell row x, x, y-1]
        else
          (* x; x+1 *)
          [cur_cell row x, x, y-1; next_cell row x, x+1, y-1]

  let adj_cells conn_ways plate x y =
    let prev = prev_cell plate.(y) x, x-1, y in
    let up = up_cells conn_ways plate x y in
    IFDEF DEBUG THEN (
      Printf.printf "adj: %d, %d\nprev: " x y;
      dump_cell2 prev;
      Printf.printf "adj, up:\n";
      List.iter dump_cell2 up;
      Printf.printf "adj end\n"
    ) ENDIF;
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

  (* for the given cell get adjacent cells with the same color *)
  let adj_fg_cells conn_ways plate x y cell =
    let all = adj_cells conn_ways plate x y in
    let f (c, cx, cy) =
      let res = Item.cmp cell c in
      IFDEF DEBUG THEN (
        Printf.printf
          "adj_fg_cells: %B, x=%d, y=%d\ncurr cell: %s\nbase cell: %s\n"
          res cx cy (Item.to_string c) (Item.to_string cell)
      ) ENDIF;
      res
    in
    List.filter f all

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
  (* ccl for one particular cell (color) *)
  let labeling cell conn_ways plate w h =
    let labels = Array.make_matrix h w Empty in
    let classes = Array.init (w*h) (fun i -> i) in
    let single_flags = Array.make (w*h) true in

    (* calculate coordinates of next cell, moving to the next row
       if necessary *)
    let next_coord x y =
      if x >= w-1
      then if y >= h-1
        then Coord_wrong
        else Coord_ok (0, y+1)
      else Coord_ok (x+1, y)
    in

    (* whether the current cell has the same color as a base cell *)
    let is_fg_cell plate x y cell = Item.cmp cell plate.(y).(x) in

    (* whether class for the given label contains only one label or more *)
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

    (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
    (* mark classes for equality. Input: cnt, [label] *)
    let mark_equiv label_cnt list =
      IFDEF DEBUG THEN (
        Printf.printf "mark_equiv: %d\n" label_cnt;
        Printf.printf "mark_equiv list:\n";
        dump_label_list list;
        Printf.printf "mark_equiv labels:\n";
        dump_labels w h (Some labels)
      ) ENDIF;
      let martyr surv_class m =
        let idx = unpack_label m in
        classes.(idx) <- surv_class
      in

      (* mark martyrs with a single item in class array *)
      let mark_s surv_class martyrs =
        List.iter (martyr surv_class) martyrs
      in

      (* run over the class array and assign a survivor class to items
         that have a class equal to the current martyr class *)
      let merging_classes surv_class martyr =
        let m_idx = unpack_label martyr in
        let m_class = classes.(m_idx) in
        IFDEF DEBUG THEN (
          Printf.printf "merging_classes: surv cls: %d, mrt l: %d, mrt c: %d\n"
            surv_class m_idx m_class;
          dump_labels w h (Some labels);
          dump_classes w h (Some classes)
        ) ENDIF;
        let rec aux_merg = function
          | cnt when cnt = label_cnt ->
            IFDEF DEBUG THEN (
              Printf.printf "merging_classes: surv cls: %d, mrt l: %d, mrt c: %d\n"
                surv_class m_idx m_class;
              dump_labels w h (Some labels);
              dump_classes w h (Some classes)
            ) ENDIF;
            ()
          | cnt when classes.(cnt) = m_class ->
            classes.(cnt) <- surv_class;
            aux_merg (cnt+1)
          | cnt ->
            aux_merg (cnt+1)
        in
        if surv_class = m_class         (* gives 10x speed boost *)
        then ()
        else aux_merg 0
      in

      (* mark martyrs with several items in the class array *)
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
    (* - - - mark_equiv - - - - - - - - - - - - - - - - - - - - -*)
    in

    let make_uniq_label_list list =
      let uniq_aux acc item =
        if List.mem item acc
        then acc
        else item :: acc
      in
      List.fold_left uniq_aux [] list
    in

    (* choose the label from a list, mark classes for equality *)
    let choose_label label_cnt = function
      | (c, cx, cy) :: [] ->
        (
          IFDEF DEBUG THEN (
            Printf.printf "choose_label, one: %d, %d\n" cx cy;
            dump_labels w h (Some labels)
          ) ENDIF;
          match labels.(cy).(cx) with
            | (Label l) as ll -> ll
            | _ -> assert false (* should not happen *)
        )
      | list ->
        let f (_c, cx, cy) =
          IFDEF DEBUG THEN (
            Printf.printf "choose_label, f: %d, %d\n" cx cy;
          ) ENDIF;
          labels.(cy).(cx)
        in
        let label_list = List.map f list in
        let uniq_label_list = make_uniq_label_list label_list in
        IFDEF DEBUG THEN (
          Printf.printf "choose_label, list\n";
          dump_label_list label_list;
          Printf.printf "choose_label, uniq list\n";
          dump_label_list uniq_label_list
        ) ENDIF;
        mark_equiv label_cnt uniq_label_list;
        let aux lst = List.hd lst in  (* stub *)
        let res_label = aux uniq_label_list in
        res_label
    in

    (* pass 1 of ccl *)
    let rec pass1 conn_ways x y label_cnt =
      IFDEF DEBUG THEN (
        Printf.printf "pass1, x=%d, y=%d, lcnt=%d, c: %s\n"
          x y label_cnt (Item.to_string plate.(y).(x));
        dump_labels w h (Some labels)
      ) ENDIF;
      let next_label_cnt =
        if is_fg_cell plate x y cell
        then match adj_fg_cells conn_ways plate x y cell with
          | [] ->                         (* new label *)
            labels.(y).(x) <- Label label_cnt;
            label_cnt + 1
          | list ->                       (* existing labels *)
            let adj_label = choose_label label_cnt list in
            labels.(y).(x) <- adj_label;
            label_cnt
        else
          (* skip background cell *)
          label_cnt
      in
      match next_coord x y with
        | Coord_wrong ->
          next_label_cnt    (* pass 1 done *)
        | Coord_ok (x2, y2) ->
          pass1 conn_ways x2 y2 next_label_cnt
    in
    let _label_cnt = pass1 conn_ways 0 0 0 in
    IFDEF DEBUG THEN (
      Printf.printf "pass1 done\n";
      dump_all ~labels:(Some labels) ~classes:(Some classes)
        ~flags:(Some single_flags) ~cell:(Some cell)
        ~cnt:(Some _label_cnt) w h;
      Printf.printf "pass1 dump done\n"
    ) ENDIF;
    let pass2 =
      let res = Array.make_matrix h w None in
      for y = 0 to h-1 do
        for x = 0 to w-1 do
          match labels.(y).(x) with
            | Empty -> ()
            | Label cur_label ->
              res.(y).(x) <- Some classes.(cur_label)
        done
      done;
      res
    in
    let res = pass2 in
    IFDEF DEBUG THEN (
      Printf.printf "pass2 done, cell: %s\n" (Item.to_string cell);
      dump_one_ccl w h res;
      Printf.printf "pass2 dump done\n"
    ) ENDIF;
    res

  (* - - - labeling - - - - - - - - - - - - - - - - - - - - - -*)

  (* do a connected components labeling *)
  let ccl avail_cells conn_ways w h list =
    let plate = init_ccl_matrix w h list in
    let f cell = labeling cell conn_ways plate w h in
    List.map f avail_cells
   
  let ccl4 avail_cells w h list =
    ccl avail_cells 4 w h list
  let ccl6 avail_cells w h list =
    ccl avail_cells 6 w h list
  let ccl8 avail_cells w h list =
    ccl avail_cells 8 w h list
end
