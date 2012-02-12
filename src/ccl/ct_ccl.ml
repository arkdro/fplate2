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
  val cmp         : t -> t -> bool
  val value       : t -> int
  val filler      : int
  val empty       : int
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
    let up = Array1.fill (Array1.create int c_layout (w+2)) Item.filler in
    let down = Array1.fill (Array1.create int c_layout (w+2)) Item.filler in
    let left = Array1.fill (Array1.create int c_layout (h+2)) Item.filler in
    let right = Array1.fill (Array1.create int c_layout (h+2)) Item.filler in

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

  (* - - - labeling- - - - - - - - - - - - - - - - - - - - - - - - - *)
  let labeling cell conn_ways data w h =
    let (plate, b_up, b_right, b_down, b_left) = data in
    let labels = Array2.fill (Array2.create int c_layout w h) Item.empty in
    let is_bg (x, y) = false in
    let has_label (x, y) = false in
    let has_mark (x, y) = false in
    let step_1 = () in
    let step_2 = () in
    let step_3 = () in
    let aux x y =
      if (not (has_label (x, y))) && (is_bg (up_coord w h x y))
      then step_1
      else if is_bg (down_coord w h x y) && (not (has_mark (down_coord w h x y)))
      then step_2
      else step_3
    in
    aux 0 0
  (* - - - labeling- - - - - - - - - - - - - - - - - - - - - - - - - *)

  (* do a connected components labeling *)
  let ccl avail_cells conn_ways w h list =
    let data = init_ccl_matrix w h list in
    IFDEF CTCCL_DUMP_DEBUG THEN (
      let (plate, _b_up, _b_right, _b_down, _b_left) = data in
      dump_ct_ccl plate
    ) ENDIF;
    let f cell = labeling cell conn_ways data w h in
    List.map f avail_cells
   
  let ccl4 avail_cells w h list =
    ccl avail_cells 4 w h list
  let ccl6 avail_cells w h list =
    ccl avail_cells 6 w h list
  let ccl8 avail_cells w h list =
    ccl avail_cells 8 w h list
end
