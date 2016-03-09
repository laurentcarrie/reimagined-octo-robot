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
  }
  let t_of_j j = (
    let j = Br.objekt j in
    let table = Br.make_table j in
      {
	name = Br.string (Br.field table "name") ;
	a_type = Br.string (Br.field table "type") ;	
	optional = Br.bool (Br.field table "option") ;
      }
  )
end

module S = struct
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

module T = struct
  type t = {
    modules : S.t list ;
  }
  let t_of_j j = (
    { 
      modules = Br.list S.t_of_j j ;
    }
  )
end

  
