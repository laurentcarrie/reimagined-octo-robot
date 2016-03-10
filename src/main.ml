open Printf
open ExtList
module S = String
open ExtString

let (//) = Filename.concat

let log_print = ref None

let log fs =
  (match !log_print with
    (* | None -> ksprintf ( print_endline ) *)
    | None -> ksprintf ( fun _ -> ()) fs
    | Some f -> ksprintf f fs
  ) 

let int_of_string s = 
  try
    int_of_string s
  with
    | e -> printf "could not convert to int : '%s'\n" s ; flush stdout ; raise e
  


let main () =
  let filename_in = Sys.argv.(1) in
  let data = Datamodel.T.t_of_j (Json_io.load_json filename_in) in
  let module_name = Sys.argv.(2) in
  let fout = open_out (module_name ^ ".ml") in
  let fout_json = open_out (module_name ^ "_json.ml") in
  let () = Printstruct.write_file module_name fout fout_json data in
  let () = close_out fout in
  let () = close_out fout_json in
    ()


let _ = 
  try
    main () ;
    exit 0
  with
    | e -> printf "%s\n" (Printexc.to_string e) ; exit 1
	
