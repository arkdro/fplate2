module Ct_next_item = struct
  let up_coord w h x y =
    Some (x, y-1)

  let down_coord w h x y =
    Some (x, y+1)

  let next_coord w h x y =
    if x+1 < w
    then Some (x+1, y)
    else if y+1 < h
    then Some (0, y+1)
    else None

  let prev_in_line_coor x y =
    if x > 0
    then x-1, y
    else assert false                   (* should not happen *)

end
