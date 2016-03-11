open Data1_json
open Printf

module Br = Json_type.Browse


let test_1 j = (
  try
    let a = A.t_of_json j in
    let () = Json_io.save_json "data1_test-out.json" (A.json_of_t a) in 
    let () = printf "x=%f\n" a.A.x in
    let b = A.t_of_json (Json_io.load_json "data1_test-out.json") in
    let () = printf "x=%f\n" b.A.x in
      assert (a=b) ;
  with
    | e -> printf "error in test_1\n" ; raise e
)

let test_2 j = (
  let a = B.t_of_json j in
  let () = Json_io.save_json "data1_test-out.json" (B.json_of_t a) in 
  let () = printf "x=%f\n" a.B.a.A.x in
  let b = B.t_of_json (Json_io.load_json "data1_test-out.json") in
  let () = printf "x=%f\n" b.B.a.A.x in
    assert (a=b) ;
)

let _ =
  try
    let () = Printexc.record_backtrace true in
    let j_test = Json_io.load_json "data1_test.json" in
    let j_test = Br.array j_test in

    let () = test_1 (List.hd j_test) in
    let j_test = List.tl j_test in

    let () = test_2 (List.hd j_test) in
    let j_test = List.tl j_test in


    let _ = ignore j_test in
      exit 0
  with
    | e -> 
	Printexc.print_backtrace stdout  ; 
	printf "last exception : %s\n" (Printexc.to_string e) ;
	exit 1
