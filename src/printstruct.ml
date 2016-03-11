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
  let pf_nonl fs = ksprintf ( fun s -> fprintf fout "%s" s ) fs in
  let pfj fs = ksprintf ( fun s -> fprintf fout_json "%s\n" s ) fs in
  let pfj_nonl fs = ksprintf ( fun s -> fprintf fout_json "%s" s ) fs in

    

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


    let write_product m = (
      pf "module %s = struct" m.Product.name ;

      pf "\ttype t = {" ;
      List.iter ( fun a ->
	pf "\t\t%s : %s %s %s;" a.Attribute.name a.Attribute.a_type (if a.Attribute.list then "list" else "") (if a.Attribute.optional then "option" else "")  ;
      ) m.Product.attributes ;
      pf "\t}" ;
      
      pf "end" ;
      ()
    ) in
    
    let write_product_json m = (
      pfj "module %s = struct" m.Product.name ;
      pfj "\tinclude %s.%s" (String.capitalize module_name) m.Product.name  ;
      pfj "\tlet t_of_json j = (" ;
      pfj "\t\tlet table = Br.make_table (Br.objekt j) in" ;
      pfj "\t\t{" ;
      List.iter ( fun a ->

	if a.Attribute.optional then
	  pfj "\t\t\t%s = (try (Option.map (%s %s) (Br.optfieldx table \"%s\")) with | e -> raise e) ;" a.Attribute.name (if a.Attribute.list then "Br.list" else "" ) (browse_function_of_type a.Attribute.a_type) a.Attribute.name 
	else
	  pfj "\t\t\t%s = (try (%s %s (Br.field table \"%s\")) with | e -> raise e) ;" a.Attribute.name  (if a.Attribute.list then "Br.list" else "" ) (browse_function_of_type a.Attribute.a_type)  a.Attribute.name 
      ) m.Product.attributes ;
      pfj "\t\t}" ;
      pfj "\t)" ;

      pfj "\tlet json_of_t t = (" ;
      pfj "\tBu.objekt [" ;
      List.iter ( fun a ->
	if a.Attribute.optional then
	  pfj "\t\t\"%s\",Bu.option (Option.map (%s %s) t.%s) ;" a.Attribute.name (if a.Attribute.list then "Bu.list" else "" ) (build_function_of_type a.Attribute.a_type) a.Attribute.name 
	else
	  pfj "\t\t\"%s\",%s %s t.%s ;" a.Attribute.name (if a.Attribute.list then "Bu.list" else "" ) (build_function_of_type a.Attribute.a_type) a.Attribute.name 
      ) m.Product.attributes ;
      pfj "\t\t]" ;
      pfj ")" ;

      pfj "end" ;
      ()
    ) in
    
    let write_sum m = (
      pf "module %s = struct" m.Sum.name ;
      pf "\ttype t =" ;
      List.iter ( fun ctor ->
	pf_nonl "\t\t| %s " ctor.Sum.ctor ;
	match ctor.Sum.args with
	  | [] -> pf " ; "
	  | l -> pf " of (%s)" (String.join "*" l)
      ) m.Sum.ctors ;
      pf "\t" ;
      pf "end" ;
      ()
    ) in
    
    let write_sum_json m = (
      pfj "module %s = struct" m.Sum.name ;
      pfj "\tinclude %s.%s" (String.capitalize module_name) m.Sum.name  ;

      pfj "\tlet t_of_json j = (" ;
      pfj "\ttry" ;
      pfj "\t\tlet table = Br.make_table (Br.objekt j) in" ;
      pfj "\t\tlet ctor = Br.string (Br.field table \"ctor\") in" ;
      pfj "\t\tlet args = Br.array (Br.field table \"args\") in" ;
      pfj "\t\tmatch ctor with" ;
      List.iter ( fun ctor ->
	pfj_nonl "\t\t\t| \"%s\" -> %s " ctor.Sum.ctor ctor.Sum.ctor ;
	match ctor.Sum.args with
	  | [] -> pfj " ;" 
	  | values -> pfj "(%s) ;" (String.join "," (List.mapi ( fun index v -> sprintf "%s (List.nth args %d)" (browse_function_of_type v) index) (values)))
      ) m.Sum.ctors ;
      pfj "\t\t\t| _ -> failwith \"bad value for type %s\" " m.Sum.name ; 
      pfj "\twith | e -> printf \"error while reading data for type %s\\n\" ; raise e" m.Sum.name ;
      pfj "\t)" ;

      pfj "\tlet json_of_t t = (" ;
      pfj "\t\tmatch t with " ;
      List.iter ( fun ctor ->
	pfj "\t\t\t| %s %s -> Bu.objekt [ (\"ctor\",Bu.string \"%s\") ; (\"args\", Bu.array [%s])]  " 
	  ctor.Sum.ctor
	  ( match ctor.Sum.args with
	    | [] -> ""
	    | args -> "(" ^ (String.join "," (List.mapi ( fun index _ -> sprintf "arg_%d" index) args)) ^ ")" ;
	  )
	  ctor.Sum.ctor
	  ( match ctor.Sum.args with
	    | [] -> ""
	    | values -> (String.join ";" (List.mapi ( fun index v -> sprintf "%s arg_%d" (build_function_of_type v) index)values ))
	  ) ;
      ) m.Sum.ctors ;
      pfj ")" ;
      
      pfj "end" ;
      ()
    ) in
    

      List.iter ( fun m ->
	match m with
	  | T.S m -> (
	      write_sum m ;
	      write_sum_json m ;
	    )
	  | T.P m -> (
	      write_product m ;
	      write_product_json m ;
	    )
      ) data.T.modules


)
