open Datamodel
open Printf
open ExtList
open ExtString


let browse_function_of_type name = 
  match name with
    | "string" -> "Br.string"
    | "int" -> "Br.int"
    | "float" -> "Br.float"
    | "bool" -> "Br.bool"
    | t -> (
	let (module_name,type_name) = (try (String.split t ".") with | Invalid_string -> 
	  (* let msg = sprintf "not primitive type should be of form Module.typename : '%s'" t in failwith msg *)
	  t,""
	) 
	in
	  (*
	let _ = (try
	    List.find ( fun m -> m.Module.name = module_name ) world.T.modules 
	  with
	    | Not_found -> let msg = sprintf "no such module : '%s'" module_name in failwith msg
	)
	in
	  *)
	  sprintf "%s.t_of_json" module_name
      )
	    
let build_function_of_type name = (
  match name with
    | "string" -> "Bu.string"
    | "int" -> "Bu.int"
    | "float" -> "Bu.float"
    | "bool" -> "Bu.bool"
    | t -> let (module_name,type_name) = (try (String.split t ".") with | Invalid_string -> t,"") in sprintf "%s.json_of_t" module_name
)

let write_file module_name fout fout_json data = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s\n" s ) fs in
  let pfj fs = ksprintf ( fun s -> fprintf fout_json "%s\n" s ) fs in

    

    pfj "
open Printf
open ExtList
open ExtString

module Br = struct
  include Json_type.Browse
  let field table name =  (
    try
      field table name
    with
      | Not_found -> let msg = sprintf \"field '%%s' not found\" name in failwith msg
  )
end;;

module Bu = struct
  include Json_type.Build
end;;
" 
    ;


    let write_module m = (
      pf "module %s = struct" m.Module.name ;

      pf "\ttype t = {" ;
      List.iter ( fun a ->
	pf "\t\t%s : %s %s %s;" a.Attribute.name a.Attribute.a_type (if a.Attribute.list then "list" else "") (if a.Attribute.optional then "option" else "")  ;
      ) m.Module.attributes ;
      pf "\t}" ;
      
      pf "end" ;
      ()
    ) in
    
    let write_module_json m = (
      pfj "module %s = struct" m.Module.name ;
      pfj "\tinclude %s.%s" (String.capitalize module_name) m.Module.name  ;
      pfj "\tlet t_of_json j = (" ;
      pfj "\t\tlet table = Br.make_table (Br.objekt j) in" ;
      pfj "\t\t{" ;
      List.iter ( fun a ->

	if a.Attribute.optional then
	  pfj "\t\t\t%s = Option.map (%s %s) (Br.optfield table \"%s\") ;" a.Attribute.name (if a.Attribute.list then "Br.list" else "" ) (browse_function_of_type a.Attribute.a_type) a.Attribute.name 
	else
	  pfj "\t\t\t%s = %s %s (Br.field table \"%s\") ;" a.Attribute.name  (if a.Attribute.list then "Br.list" else "" ) (browse_function_of_type a.Attribute.a_type)  a.Attribute.name 
      ) m.Module.attributes ;
      pfj "\t\t}" ;
      pfj "\t)" ;

      pfj "\tlet json_of_t t = (" ;
      pfj "\tBu.objekt [" ;
      List.iter ( fun a ->
	if a.Attribute.optional then
	  pfj "\t\t\"%s\",Bu.option (Option.map (%s %s) t.%s) ;" a.Attribute.name (if a.Attribute.list then "Bu.list" else "" ) (build_function_of_type a.Attribute.a_type) a.Attribute.name 
	else
	  pfj "\t\t\"%s\",%s %s t.%s ;" a.Attribute.name (if a.Attribute.list then "Bu.list" else "" ) (build_function_of_type a.Attribute.a_type) a.Attribute.name 
      ) m.Module.attributes ;
      pfj "]" ;
      pfj ")" ;

      pfj "end" ;
      ()
    ) in
    
    let write_sum m = (
      pf "module %s = struct" m.Module.name ;
      pf "\ttype t =" ;
      List.iter ( fun a ->
	pf "\t\t| %s of %s %s %s" a.Attribute.name a.Attribute.a_type (if a.Attribute.list then "list" else "") (if a.Attribute.optional then "option" else "")  ;
      ) m.Module.attributes ;
      pf "\t" ;
      pf "end" ;
      ()
    ) in
    
    let write_sum_json m = (
      pfj "module %s = struct" m.Module.name ;
      pfj "\tinclude %s.%s" (String.capitalize module_name) m.Module.name  ;

      pfj "\tlet t_of_json j = (" ;
      pfj "\t\tlet values = String.nsplit (Br.string j) \" \" in" ;
      pfj "\t\tlet (name,values) = match values with | hd::tl -> (hd,tl) | [] -> failwith \"empty string for %s\" in" m.Module.name ;
      pfj "\t\tmatch name with" ;
      List.iter ( fun a ->
	  pfj "\t\t\t| \"%s\" -> %s (\"xx\") ;" a.Attribute.name a.Attribute.name
      ) m.Module.attributes ;
      pfj "\t\t\t| _ -> failwith \"bad value for type %s\" " m.Module.name ; 
      pfj "\t)" ;

      pfj "\tlet json_of_t t = (" ;
      pfj "\t\tmatch t with " ;
      List.iter ( fun a ->
	pfj "\t\t\t| %s t -> Bu.string \"%s\" " a.Attribute.name a.Attribute.name
      ) m.Module.attributes ;
      pfj ")" ;
      
      pfj "end" ;
      ()
    ) in
    

      List.iter ( fun m ->
	if m.Module.is_sum then (
	  write_sum m ;
	  write_sum_json m ;
	)
	else (
	  write_module m ;
	  write_module_json m ;
	)
      ) data.T.modules


)
