(* 
 * A Linear-Time Component-Labeling Algorithm
 * Using Contour Tracing Technique
 * Fu Chang, Chun-Jen Chen, and Chi-Jen Lu
 *)

open Bigarray
open Array2

open Ct_next_item.Ct_next_item

(* ---------------------------------------------------------------------- *)
module type ItemSig = sig
  type t
  (* val cmp         : t -> t -> bool *)
  val value       : t -> int
  val filler      : int
  val empty       : int
  val bg_mark     : int
    IFDEF CTCCL_DUMP_DEBUG THEN
  val to_string   : t -> string
    ENDIF
end
(* ---------------------------------------------------------------------- *)

module Ct_ccl (Item : ItemSig) = struct
  exception Array_size_mismatch

  let ct_ccl_to_string a =
    let w = Array2.dim1 a in
    let h = Array2.dim2 a in
    let row_to_str y =
      let rec aux x acc =
        if x >= 0
        then
          let item_str = Printf.sprintf "%d" a.{x, y} in
          aux (x-1) (item_str :: acc)
        else acc
      in
      let lst = aux (w-1) [] in
      String.concat " " lst
    in
    let rec aux_rows y racc =
      if y >= 0
      then aux_rows (y-1) ((row_to_str y) :: racc)
      else racc
    in
    let rlst = aux_rows (h-1) [] in
    String.concat "\n" rlst

  let dump_ct_ccl a =
    Printf.printf "dump_ct_ccl:\n%s\n" (ct_ccl_to_string a)

  (* fill a 2d matrix with data from a flat list. Return matrix and
     extra border vectors *)
  let init_ccl_matrix w h list =

    (* transforms linear index to x,y coordinates *)
    let coor i =
      let y = i / w in
      if y >= h then raise Array_size_mismatch;
      let x = i mod w in
      (x,y)
    in

    (* extra borders for marking surrounding background pixels *)
    let up = Array1.create int c_layout (w+2) in
    let _ = Array1.fill up Item.filler in
    let down = Array1.create int c_layout (w+2) in
    let _ = Array1.fill down Item.filler in
    let left = Array1.create int c_layout h in
    let _ = Array1.fill left Item.filler in
    let right = Array1.create int c_layout h in
    let _ = Array1.fill right Item.filler in

    let a1 = Array2.create int c_layout w h in
    let rec aux0 idx = function
      | _ when idx >= w*h -> ()
      | head :: tail ->
        let x, y = coor idx in
        let item = Item.value head in
        a1.{x, y} <- item;
        aux0 (idx+1) tail
      | [] -> ()
    in
    aux0 0 list;
    (a1, up, right, down, left)

    (* delta coordinates and tracer index *)
    let delta_coord_tracer_index =
      let dcoor2idx = Hashtbl.create 8 in
      let idx2dcoor = Hashtbl.create 8 in
      let dat = [
        (-1,  0), 0;
        (-1, -1), 1;
        (0, -1), 2;
        (1, -1), 3;
        (1,  0), 4;
        (1,  1), 5;
        (0,  1), 6;
        (-1,  1), 7
      ] in
      List.iter (fun (k, v) -> Hashtbl.add dcoor2idx k v) dat;
      List.iter (fun (v, k) -> Hashtbl.add idx2dcoor k v) dat;
      dcoor2idx, idx2dcoor

    let dcoord_to_tracer_index dx dy tab = Hashtbl.find tab (dx, dy)
    let tracer_index_to_dcoord idx tab = Hashtbl.find tab idx

  (* - - - labeling- - - - - - - - - - - - - - - - - - - - - - - - - *)
  let labeling cell conn_ways data w h =
    let (plate, b_up, b_right, b_down, b_left) = data in
    let labels = Array2.create int c_layout w h in
    let _ = Array2.fill labels Item.empty in
    let dcoor2idx, idx2dcoor = delta_coord_tracer_index in
    let is_bg = function
      | Some (x, y) when x >= 0 && x < w && y >= 0 && y < h ->
        plate.{x, y} <> cell
      | Some _ -> true
      | None -> true
    in
    let is_fg = function
      | Some (x, y) when x >= 0 && x < w && y >= 0 && y < h ->
        plate.{x, y} = cell
      | Some _ -> false
      | None -> false
    in
    let has_label = function            (* fg cell *)
      | Some (x, y) when x >= 0 && x < w && y >= 0 && y < h ->
        labels.{x, y} <> Item.empty
      | Some (x, y) -> assert false     (* should not happen *)
      | None -> false                   (* borders are behind the plate *)
    in
    let has_mark = function             (* bg cell *)
      | Some (x, y) when x >= 0 && x < w && y >= 0 && y < h ->
        labels.{x, y} = Item.bg_mark
      | Some (x, y) when y < 0 && x >= -1 && x <= w ->
        b_up.{x+1} = Item.bg_mark       (* b_up begins from -1 *)
      | Some (x, y) when y >= h && x >= -1 && x <= w ->
        b_down.{x+1} = Item.bg_mark     (* b_down begins from -1 *)
      | Some (x, y) when x < 0 && y >= 0 && y < h ->
        b_left.{y} = Item.bg_mark
      | Some (x, y) when x >= w && y >= 0 && y < h ->
        b_right.{y} = Item.bg_mark
      | _ -> assert false     (* should not happen *)
      (* | None -> false *)
    in

    let mark_bg x y = labels.{x, y} <- Item.bg_mark in

    let assign_label label x y =
      labels.{x, y} <- label            (* check for allowed x, y? *)
    in

    (* given prev and cur coordinates calculate tracer index of prev point *)
    let coord_to_tracer_index px py x y =
      let dx = x - px in
      let dy = y - py in
      dcoord_to_tracer_index dx dy dcoor2idx
    in

    (* find initial point for external tracer *)
    let ext_init_point x y = function
      | None -> 7                       (* start of external contour *)
      | Some (px, py) ->
        let idx = coord_to_tracer_index px py x y in
        (idx + 2) mod 8
    in

    (* find following foreground point for the given point in
       external contour *)
    let ext_tracer x y prev =
      (* goes clockwise *)
      let init = ext_init_point x y prev in
      let rec aux = function
        | 8 -> None
        | add ->
          let idx = (init + add) mod 8 in
          let dx, dy = tracer_index_to_dcoord idx idx2dcoor in
          if is_fg (Some (x+dx, y+dy))
          then Some (x+dx, y+dy)
          else (
            mark_bg (x+dx) (y+dy);
            aux (add+1)
          )
      in aux 0
    in

    let int_tracer x y = ()             (* stub *)
    (* goes counter(!!!) clockwise, disregarding algo description *)
    in

    let trace_external_contour x0 y0 label =
      let start = Some (x0, y0) in
      let rec aux (x, y) prev second_point =
        let cur_point = Some (x, y) in
        match ext_tracer x y prev with
          | None -> ()                  (* standalone point *)
          | Some (x2, y2) as next_point when second_point = None ->
            aux (x2, y2) cur_point next_point
          | Some (x2, y2) when prev = start &&
                            cur_point = second_point ->
            ()                          (* contour done *)
          | Some (x2, y2) ->
            aux (x2, y2) cur_point second_point
      in aux (x0, y0) None None

    in
    let trace_internal_contour x y = () (* stub *)
    in
    let step_1 label x y =
      if (not (has_label (Some (x, y)))) && (is_bg (up_coord w h x y))
      then
        (
          assign_label label x y;
          trace_external_contour x y label;
          true, label + 1;
        )
      else false, label
    in
    let copy_prev_cell_label x y =
      let px, py = prev_in_line_coor x y in
      let label = labels.{px, py} in
      labels.{x, y} <- label
    in
    let step_2aux x y =
      (
        if labels.{x, y} = 0
        then copy_prev_cell_label x y
      );
      trace_internal_contour x y
    in
    let step_2 x y =
      if is_bg (down_coord w h x y) && (not (has_mark (down_coord w h x y)))
      then
        (step_2aux x y;
         true)
      else false
    in
    let step_3 x y =
      copy_prev_cell_label x y
    in
    let rec aux label_0 = function
      | None -> ()
      | Some (x, y) ->
        let flag_1, new_label = step_1 label_0 x y in
        let flag_2 = step_2 x y in
        (if flag_1 = false && flag_2 = false
         then step_3 x y);
        let next = next_coord w h x y in
        aux new_label next
    in
    aux 1 (Some (0, 0))
  (* - - - labeling- - - - - - - - - - - - - - - - - - - - - - - - - *)

  (* do a connected components labeling *)
  let ccl avail_cells conn_ways w h list =
    let data = init_ccl_matrix w h list in
    IFDEF CTCCL_DUMP_DEBUG THEN (
      let (plate, _b_up, _b_right, _b_down, _b_left) = data in
      dump_ct_ccl plate
    ) ENDIF;
    let f cell =
      let cval = Item.value cell in
      labeling cval conn_ways data w h
    in
    List.map f avail_cells
   
  let ccl4 avail_cells w h list =
    ccl avail_cells 4 w h list
  let ccl6 avail_cells w h list =
    ccl avail_cells 6 w h list
  let ccl8 avail_cells w h list =
    ccl avail_cells 8 w h list
end
