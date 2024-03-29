open Arg
open Plate_test
open Rinit
open Point
(* open Point2 *)
open Plate
open Ccl_test
open Ct_ccl_test
module P1 = Plate (Point)
(* module P2 = Plate (Point2) *)

let get_pars ifile ofile wr hr pszr tr sr ct ctt ctype loops verbose =
  let usage_text = "possible parameters:" in
  let pars = [
    ("-i", Set_string ifile, "input file");
    ("-o", Set_string ofile, "output file");
    ("-w", Set_int wr, "width");
    ("-h", Set_int hr, "height");
    ("-psz", Set_int pszr, "point size");
    ("-sr", Set_int sr, "spread ratio (0..100)");
    ("-ct", Unit (fun x -> ct := true), "ccl test");
    ("-ctt", Unit (fun x -> ctt := true), "ct ccl test");
    ("-ctype", Set_int ctype, "ccl type - 4, 6 (default), 8");
    ("-loops", Set_int loops, "loops (default: 1)");
    ("-t", Set tr, "test loop with given w, h, psz");
    ("-v", Set_int verbose, "verbose (default: 1)")
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
  let loops = ref 1 in
  let s_ratio = ref 0 in
  let pszr = ref 8 in
  let wr = ref 3 in
  let hr = ref 3 in
  let test = ref false in
  let ccl_type = ref 0 in
  let ccl_test = ref false in
  let ct_ccl_test = ref false in
  let verbose = ref 1 in
  get_pars ifile ofile wr hr pszr test s_ratio ccl_test ct_ccl_test
    ccl_type loops verbose;
  let _ = Rinit.r_init in
  Printf.printf "w=%d, h=%d, psz=%d, test=%b, sratio=%d, ccl type=%d, ccl test=%B, ct ccl test=%B, loops=%d, verbose=%d\n"
    !wr !hr !pszr !test !s_ratio !ccl_type !ccl_test !ct_ccl_test
    !loops !verbose;
  if !test = true then
    Plate_test.main !wr !hr !pszr !s_ratio
  else if !ct_ccl_test = true then
    Ct_ccl_test.main !wr !hr !pszr !s_ratio !ccl_type !verbose !loops
  else if !ccl_test = true then
    Ccl_test.main !wr !hr !pszr !s_ratio !ccl_type !verbose !loops
  else
    (* let res = P2.gen point_size width height in *)
    let res = P1.gen !pszr !wr !hr !s_ratio in
    Printf.printf "main res:\n%s\n" (P1.to_string res)

let _ = main ()
