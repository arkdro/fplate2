module Ct_next_item = struct
  let up_coord w h x y = x, y
  let down_coord w h x y = x, y
  let next_coord w h x y = Some (x, y)
end
