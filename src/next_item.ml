(*
  cell and row operations for row and plate: prev/cur/next cell/row
*)
module Next_item = struct

  type 'a row = Norow | Row of 'a
  type 'a cell = Nocell | Cell of 'a

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

end
