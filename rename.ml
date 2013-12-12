
open Format
open Ast
open Mips

let union_size_env : (string, int * ((c_type * ident * int) list)) Hashtbl.t= 
  Hashtbl.create 17

let struct_size_env : (string, int * ((c_type * ident * int) list)) Hashtbl.t= 
  Hashtbl.create 17

let mk_node e t = {loc = t; node = e}

module VarMap = Map.Make (String)

let unique = ref 0
let next () = let v = !unique in incr unique; v 
let var_env = ref VarMap.empty

let map_of_lists vars new_vars env = 
  List.fold_left2 (
    fun acc v1 v2 ->
      let (_, id_old),(_, id_new) = v1, v2 in 
      VarMap.add id_old.node id_new.node acc)
    env vars new_vars

let rec rename_expr env e =
  match e.node with
  | Eident id ->
    let new_id = mk_node (VarMap.find id.node env) id.loc in
    mk_node (Eident new_id) e.loc
  | Edot (e, id) -> 
      mk_node (Edot (rename_expr env e, id)) e.loc
  | Eassign (e1, e2) -> 
      mk_node (Eassign (rename_expr env e1, rename_expr env e2)) e.loc
  | Eunop (unop, e1) -> 
      mk_node (Eunop (unop, rename_expr env e1)) e.loc
  | Ebinop (b, e1, e2) -> 
      mk_node (Ebinop (b, rename_expr env e1, rename_expr env e2)) e.loc
  | Ecall (id, el) -> 
      mk_node (Ecall (id, List.map (rename_expr env) el)) e.loc
  | _ -> e


let rec rename_stmt env stmt =
  match stmt.node with
    | Sexpr e -> mk_node (Sexpr (rename_expr env e)) stmt.loc
    | Sif (e, s1, s2) -> 
        mk_node (
          Sif (rename_expr env e, rename_stmt env s1, rename_stmt env s2))
          stmt.loc
    | Swhile (e, s) -> 
        mk_node (Swhile (rename_expr env e, rename_stmt env s)) stmt.loc
    | Sfor (stmts1, e, stmts2, stmt) -> 
        mk_node (Sfor (
          rename_stmts env stmts1, 
          rename_expr env e, rename_stmts env stmts2, rename_stmt env stmt)) 
          stmt.loc
    | Sblock b -> mk_node (Sblock (rename_bloc b env)) stmt.loc
    | Sreturn (Some e) -> mk_node (Sreturn (Some (rename_expr env e))) stmt.loc
    | _ -> stmt

  
and rename_stmts env = List.map (rename_stmt env)
  
      
and rename_var env var = 
  let t, id = var in
  t, mk_node ((id.node ^ "_" ^ string_of_int (next ()))) id.loc

and rename_vars env = List.map (rename_var env) 
  
and rename_bloc (vars, stmts) env = 
  let new_vars = rename_vars env vars in
  let new_env = map_of_lists vars new_vars env in
  let new_stmts = rename_stmts new_env stmts in
  new_vars, new_stmts
   
let rename_fun t id vars b env =
  let new_vars = rename_vars env vars in
  let new_env = map_of_lists vars new_vars env in
  t, id, new_vars, (rename_bloc b new_env)

    
let rename_decl d = 
  match d with 
  | Dfun (typ, id, vars, b) ->
    let typ, id, vars, b = rename_fun typ id vars b !var_env in
    Dfun (typ, id, vars, b)
  | Dstruct (id, l) -> Dstruct (id, l) 
  | Dunion (id, l) -> Dstruct (id, l)
  | Dvars vars ->
    let new_vars = rename_vars !var_env vars in 
    var_env := map_of_lists vars new_vars !var_env;
    Dvars new_vars
      

let rec rename_file ast = 
  match ast with 
  | [] -> []
  | decl :: decls -> 
    let d = rename_decl decl in
    d :: rename_file decls


let debug_struct_size () = 
  Hashtbl.iter (fun k v -> 
    let size, l = v in 
    printf "%s of size : %d@\n" k size) struct_size_env;
  Hashtbl.iter (fun k v -> 
    let size, l = v in 
    printf "%s of size : %d@\n" k size) union_size_env;
  VarMap.iter (printf "rename : %s -> %s\n") !var_env


