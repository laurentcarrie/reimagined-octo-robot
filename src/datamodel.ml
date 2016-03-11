open ExtList
open ExtString
open Printf


module Br = struct
  include Json_type.Browse
  let field table name =  (
    try
      field table name
    with
      | Not_found -> let msg = sprintf "field '%s' not found" name in failwith msg
  )
end;;

let (//) = Filename.concat

module Attribute = struct 

  type t = {
    name : string ;
    a_type : string ;
    optional : bool ;
    list : bool ;
  }
  let t_of_j j = (
    let j = Br.objekt j in
    let table = Br.make_table j in
      {
	name = Br.string (Br.field table "name") ;
	a_type = Br.string (Br.field table "type") ;	
	optional = Option.default false (Option.map Br.bool (Br.optfield table "option")) ;
	list = Option.default false (Option.map Br.bool (Br.optfield table "list")) ;
      }
  )
end

module Product = struct
  type t = {
    name : string ;
    attributes : Attribute.t list ;
  }
  let t_of_j j = (
    let j = Br.objekt j in
    let table = Br.make_table j in
    {
      name = Br.string (Br.field table "name") ;
      attributes = Br.list Attribute.t_of_j (Br.field table "attributes") ;
    }
  )
end    

module Sum = struct
  type tt = {
    ctor : string ;
    args : string list ;
  }
  type t = {
    name : string ;
    ctors : tt list ;
  }
  let t_of_j j = (
    let j = Br.objekt j in
    let table = Br.make_table j in
    {
      name = Br.string (Br.field table "name") ;
      ctors = (
	let ctor_of_j j = (
	  let table = Br.make_table (Br.objekt j) in
	    {
	      ctor = Br.string (Br.field table "name") ;
	      args = Br.list Br.string (Br.field table "args")  ;
	    }
	) in
	  Br.list ctor_of_j (Br.field table "values") 
      );
    }
  )
end    

module T = struct
  type tt = 
      | P of Product.t 
      | S of Sum.t 
  type t = {
    modules : tt list ;
  }
  let t_of_j j = (
    { 
      modules = Br.list ( 
	fun j -> 
	  try 
	    P (Product.t_of_j j) 
	  with 
	    | _ -> S (Sum.t_of_j j)
      ) j
    }
  )
end

  
