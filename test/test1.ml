open Data1_json
open Printf


let _ =
  let a = A.t_of_json (Json_io.load_json "data1_test.json") in
  let () = Json_io.save_json "data1_test-out.json" (A.json_of_t a) in 
  let () = printf "x=%f\n" a.A.x in
    exit 0
