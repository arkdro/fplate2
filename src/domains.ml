(*
  build a simple graph-like structure based on the ccl result.
  The structure contains a list (or hash?) of nodes. Each node is:
  label, number of cells, neighbour node labels.

  Also, operations: merge two nodes.
*)

(* ---------------------------------------------------------------------- *)
module type ItemSig = sig
  type t
  val cmp         : t -> t -> bool
end
(* ---------------------------------------------------------------------- *)
module Domains (Item : ItemSig) = struct
  type label = int
  type label2 = Item.t * int
  type node = {id: label; item: Item.t; sum: int; adj: (label, bool) Hashtbl.t}
  type doms = (label, node) Hashtbl.t

  (* get list of plates, use first plate to get width and height of
     all the plates in the list *)
  let get_sizes lst = match lst with
    | h :: t ->
      let height = Array.length h in
      let width = Array.length h.(0) in
      width, height
    | [] ->
      0, 0

  let fuse_planes points planes =
    (* assume all the planes have the very same geometry *)
    let width, height = get_sizes planes in
    let acc = Array.make_matrix width height None in
    let aux (base_cell, plane) =
      for y = 0 to height-1 do
        for x = 0 to width-1 do
          match plane.(y).(x) with
            | None -> ()
            | Some label ->
              acc.(y).(x) <- Some (base_cell, label)
        done
      done
    in
    let list2 = List.combine points planes in
    List.iter aux list2;
    acc

  (* input: cells and ccl_result *)
  let create_domains points planes =
    let res_plane = fuse_planes points planes in
    res_plane

end
