module type RinSig = sig
  val r_init : unit
  val r : int -> int
end
module Rinit : RinSig = struct
  let r = Random.int
  let get_rand_init_data =
    let time = Unix.gettimeofday () in
    let tmgs = floor (time /. 1e6) in
    let ts = floor (time -. tmgs *. 1e6) in
    let tms = time -. tmgs *. 1e6 -. ts in
    let tmgs_i = int_of_float tmgs in
    let ts_i = int_of_float ts in
    let tms_i = int_of_float (floor (tms *. 1e6)) in
    let pid = Unix.getpid () in
    [|tmgs_i; ts_i; tms_i; pid|]

let r_init =
  Random.full_init get_rand_init_data
end
