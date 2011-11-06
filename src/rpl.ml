open Arg
open Plate_test
open Rinit
open Point
(* open Point2 *)
open Plate
module P1 = Plate (Point)
(* module P2 = Plate (Point2) *)

let get_pars ifile ofile wr hr pszr tr =
  let usage_text = "possible parameters:" in
  let pars = [
    ("-i", Set_string ifile, "input file");
    ("-o", Set_string ofile, "output file");
    ("-w", Set_int wr, "width");
    ("-h", Set_int hr, "height");
    ("-psz", Set_int pszr, "point size");
    ("-t", Set tr, "test loop with given w, h, psz")
  ] in
  let _ = parse pars (fun x -> ()) usage_text in
  Printf.printf "output file: %s\n" !ofile;
  Printf.printf "input file: %s\n" !ifile;
  if Sys.file_exists !ifile then
    (
      if !ofile = "" then
        Printf.printf "no output file defined\n"
      else
        (* proceed !ifile !ofile *)
        ()
    )
  else
    Printf.printf "no input file exist: %s\n" !ifile

let main () =
  let ifile = ref "" in
  let ofile = ref "" in
  let pszr = ref 8 in
  let wr = ref 3 in
  let hr = ref 3 in
  let test = ref false in
  get_pars ifile ofile wr hr pszr test;
  let _ = Rinit.r_init in
  Printf.printf "w=%d, h=%d, psz=%d, test=%b\n" !wr !hr !pszr !test;
  if !test = true then
    Plate_test.main !wr !hr !pszr
  else
    (* let res = P2.gen point_size width height in *)
    let res = P1.gen !pszr !wr !hr in
    Printf.printf "main res:\n%s\n" (P1.to_string res)

let _ = main ()
