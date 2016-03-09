open Datamodel
open Printf
open ExtList
open ExtString


let browse_function_of_type name world = 
  match name with
    | "string" -> "Br.string"
    | "int" -> "Br.int"
    | "float" -> "Br.float"
    | "bool" -> "Br.bool"
    | t -> (
	let (module_name,type_name) = (try (String.split t ".") with | Invalid_string -> let msg = sprintf "not primitive type should be of form Module.typename : '%s'" t in failwith msg) in
	let _ = (try
	    List.find ( fun m -> m.S.name = module_name ) world.T.modules 
	  with
	    | Not_found -> let msg = sprintf "no such module : '%s'" module_name in failwith msg
	)
	in
	  sprintf "%s.t_of_json" module_name
      )
	    

let write_file fout data = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s\n" s ) fs in

    pf "
open Printf
module Br = struct
  include Json_type.Browse
  let field table name =  (
    try
      field table name
    with
      | Not_found -> let msg = sprintf \"field '%%s' not found\" name in failwith msg
  )
end;;
" 
    ;


    let write_module m = (
      pf "module %s = struct" m.S.name ;
      pf "\ttype t = {" ;
      List.iter ( fun a ->
	pf "\t\t%s : %s %s %s;" a.Attribute.name a.Attribute.a_type (if a.Attribute.list then "list" else "") (if a.Attribute.optional then "option" else "")  ;
      ) m.S.attributes ;
      pf "\t}" ;
      
      pf "\tlet t_of_json j = (" ;
      pf "\t\tlet table = Br.make_table (Br.objekt j) in" ;
      pf "\t\t{" ;
      List.iter ( fun a ->

	if a.Attribute.optional then
	  pf "\t\t\t%s = Option.map (%s %s) (Br.optfield table \"%s\") ;" a.Attribute.name (if a.Attribute.list then "Br.list" else "" ) (browse_function_of_type a.Attribute.a_type data) a.Attribute.name 
	else
	  pf "\t\t\t%s = %s %s (Br.field table \"%s\") ;" a.Attribute.name  (if a.Attribute.list then "Br.list" else "" ) (browse_function_of_type a.Attribute.a_type data)  a.Attribute.name 
      ) m.S.attributes ;
      pf "\t\t}" ;
      pf "\t)" ;
      pf "end" ;
      ()
    ) in
    

      List.iter ( fun m ->
	write_module m
      ) data.T.modules


)
