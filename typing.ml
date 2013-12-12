(* typing.ml *)

open Ast
open Format


(*  ######################################################  *)
(*                        Printing errors                   *)
(*                                                          *)
(*  ######################################################  *)


let main_exist = ref false


let rec print_err_type fmt = function
  | Tstruct s  -> eprintf "struct %s" s.node
  | Tunion s -> eprintf "union %s" s.node
  | Tint -> eprintf "int"
  | Tvoid -> eprintf "void"
  | Tchar -> eprintf "char"
  | Tpointer t -> eprintf "%a*" print_err_type t
  | Tnull -> eprintf "null"
  | _ -> ()

let print_err_binop fmt = function 
  | Beq -> eprintf "binary =="
  | Bneq -> eprintf "binary !="
  | Blt -> eprintf "binary <"
  | Ble -> eprintf "binary <="
  | Bgt -> eprintf "binary >"
  | Bge -> eprintf "binary >="
  | Badd -> eprintf "binary +"
  | Bsub -> eprintf "binary -"
  | Bmul -> eprintf "binary *"
  | Bdiv -> eprintf "binary /"
  | Bmod -> eprintf "binary %%"
  | Band -> eprintf "binary &&"
  | Bor -> eprintf "binary ||"

let print_err_unop fmt = function
  | Upost_inc 
  | Upre_inc -> eprintf "increment"
  | Upre_dec 
  | Upost_dec -> eprintf "decrement"
  | Ustar -> eprintf "unary '*'"
  | Uamp -> eprintf "unary '&'"
  | Unot -> eprintf "unary exclamation mark"
  | Uminus -> eprintf "unary minus"
  | Uplus -> eprintf "unary plus"

type var_typing_error = 
  | VTE_unbound of c_type
  | VTE_void 
  | VTE_array_funtype 
  | VTE_rec_type 

(* types attendus par les arg_waiter *)
type expected = EXc_type of c_type | EXnum | EXpointer | EXleft_value

let print_err_expected fmt = function 
  | EXc_type t -> print_err_type fmt t
  | EXnum -> eprintf "number"
  | EXpointer -> eprintf "pointer" 
  | EXleft_value -> eprintf "left value"

(* elements prenants des arguments *)
type arg_waiter = 
  | AWfun of int * string
  | AWwhile
  | AWfor
  | AWif
  | AWunop of unop
  | AWreturn 

let print_err_arg_waiter fmt = function
  | AWfun (i, s) -> eprintf "argument %d of '%s'" i s 
  | AWwhile -> eprintf "'while' statement"
  | AWfor -> eprintf "'for' statement"
  | AWif -> eprintf "'if' statement"
  | AWunop u -> print_err_unop fmt u
  | AWreturn -> eprintf "'return' statement"

(* les differentes erreurs *)
type error = 
  | VarAlreadyExist of string
  | Unbound_var of string
  | FunAlreadyExist of string
  | Var_typing_error of var_typing_error * string
  | TypeAlreadyExist of c_type
  | Not_a_struct of string
  | Bad_assign 
  | Unbound_member of c_type * string
  | Invalid_operand of binop * c_type * c_type
  | Not_compatible of c_type * c_type
  | Unbound_fun of string
  | Too_many_args of string
  | Missing_args of string
  | Wrong_expected_argument of arg_waiter * c_type * expected

(* le context courant des types *)
type variable_context = 
  | CTXTstruct of string
  | CTXTunion of string
  | CTXTfun
  | CTXTstd
  | CTXTpointer

let print_var_typing_error fmt = function
  | VTE_unbound ct -> eprintf "Unbound '%a'" 
      print_err_type ct
  | VTE_void -> eprintf "Forbidden type 'void'"
  | VTE_array_funtype -> eprintf "Forbidden type array"
  | VTE_rec_type -> eprintf "Forbidden recursive type"

let print_error = function  
    | FunAlreadyExist s -> 
       eprintf "Error : redefinition of the function '%s'\n@." s
    | VarAlreadyExist s -> 
       eprintf "Error : redefinition of the variable '%s'\n@." s
    | TypeAlreadyExist ct -> 
       eprintf 
        "Error : redefinition of '%a'\n@." 
        print_err_type ct
    | Var_typing_error (vte, s) ->
       eprintf 
        "Error : %a in the definition of the variable '%s'\n@."
        print_var_typing_error vte s
    | Unbound_var s -> 
       eprintf "Error : Unbound variable '%s'\n@." s
    | Not_a_struct s -> 
       eprintf "Error : invalid suffixe '%s' after a non-struct expression 
          \n@." s
    | Unbound_member (ct, s) -> 
       eprintf "Error : '%a' has no member named %s \n@." 
       print_err_type ct s 
    | Wrong_expected_argument (aw, t1, t2) -> 
       eprintf 
       "Error : invalid type argument to %a (used '%a' value where '%a' is required) \n@." 
        print_err_arg_waiter aw print_err_type t1 print_err_expected t2 
    | Invalid_operand (binop, t1, t2) -> 
        eprintf "Error : invalid operand to %a ('%a' and '%a')\n@." 
        print_err_binop binop print_err_type t1 print_err_type t2 
    | Bad_assign -> eprintf "Error : The left value cannot be assigned\n@." 
    | Not_compatible (t1, t2)-> 
        eprintf "Error : type '%a' is not compatible with '%a'\n@." 
        print_err_type t1 print_err_type t2
    | Unbound_fun s -> 
       eprintf "Error : Unbound function '%s'\n@." s
    | Too_many_args s -> 
       eprintf "Error : too many arguments to function '%s'\n@." s
    | Missing_args s -> 
       eprintf "Error : missing arguments to function '%s'\n@." s


(* Exception generique utilisee au typage *)
exception Error of error * Ast.loc

(* exception particulière concernant l'absence de main *)
exception No_main_error 

(* exception utilisee et rattrape localement *)
exception Incompatible


(*  ################# #####################################  *)
(*                       Typing algorithm                   *)
(*                                                          *)
(*  ######################################################  *)

module VarMap = Map.Make (String)

(* Les environnements globaux *)
let g_var_env : (string, c_type) Hashtbl.t = Hashtbl.create 17
let g_struct_env : (string, c_type VarMap.t) Hashtbl.t= 
  Hashtbl.create 17
let g_union_env : (string, c_type VarMap.t) Hashtbl.t= 
  Hashtbl.create 17
let g_fun_env : (string, c_type * ((c_type * ident) list)) Hashtbl.t= 
  Hashtbl.create 17

let rec type_type typ id ctxt = 
  match typ with 
    | Tint -> ()
    | Tchar -> ()
    | Tvoid -> 
        if not (ctxt = CTXTfun || ctxt = CTXTpointer) then raise (Error 
        (Var_typing_error (VTE_void, id.node), id.loc)); ()
    | Tstruct s -> 
        if not (Hashtbl.mem g_struct_env s.node) then 
        raise (Error 
        (Var_typing_error (VTE_unbound (Tstruct s), id.node), s.loc));
        begin match ctxt with 
          | CTXTstruct is when is = s.node -> 
              raise 
          (Error (Var_typing_error (VTE_rec_type, id.node), id.loc)) 
          | _ -> () end
    | Tunion s -> 
        if not (Hashtbl.mem g_union_env s.node) then 
        raise (Error 
        (Var_typing_error (VTE_unbound (Tunion s), id.node), s.loc));
        begin match ctxt with 
          | CTXTunion is when is = s.node -> raise (Error 
              (Var_typing_error (VTE_rec_type, id.node), id.loc))
          | _ -> () end
    | Tpointer typ -> type_type typ id CTXTpointer 
    | _ -> assert false 

let type_var env vd ctxt = 
  let typ, id = vd in
  type_type typ id ctxt;
  if Hashtbl.mem env id.node then 
    raise (Error ((VarAlreadyExist id.node), id.loc))
  else begin 
    Hashtbl.add env id.node typ; vd
  end

let rec type_vars env l ctxt = 
  match l with
    | [] -> []
    | vd :: vars -> let v = type_var env vd ctxt in 
                    v :: type_vars env vars ctxt

let map_of_vars vars map =
  List.fold_left (fun m (typ, id) -> VarMap.add id.node typ m) 
    map vars

let type_union id vars = 
  if Hashtbl.mem g_union_env id.node then 
    raise (Error (TypeAlreadyExist (Tunion id), id.loc));
  begin 
    let union_tbl = Hashtbl.create 17 in
    Hashtbl.add g_union_env id.node VarMap.empty; 
    ignore(type_vars union_tbl vars (CTXTunion id.node));
    Hashtbl.replace g_union_env id.node (map_of_vars vars VarMap.empty); 
    (id, vars)
  end


let type_struct id vars = 
  if Hashtbl.mem g_struct_env id.node then 
    raise (Error (TypeAlreadyExist (Tstruct id), id.loc));
  begin 
    let struct_tbl = Hashtbl.create 17 in
    Hashtbl.add g_struct_env id.node VarMap.empty; 
    ignore(type_vars struct_tbl vars (CTXTstruct id.node));
    Hashtbl.replace g_struct_env id.node (map_of_vars vars VarMap.empty); 
    (id, vars)
  end


let compatible t1 t2 = 
  match t1 with Tint | Tchar | Tnull | Tpointer _ -> 
    begin match t2 with
    | Tint | Tchar | Tnull | Tpointer _ -> true
    | _ -> false end
    | Tstruct s -> begin match t2 with Tstruct s2 when s.node = s2.node -> true
                   | _ -> false end
    | Tunion s -> begin match t2 with Tunion s2 when s.node = s2.node -> true
                   | _ -> false end
    | _ -> false


let is_num typ = 
  match typ with
    | Tchar -> true
    | Tnull -> true
    | Tint -> true
    | Tpointer _ -> true
    | _ -> false 


let value_pointer typ = 
  match typ with
      | Tpointer t -> t
      | _ -> raise Incompatible

let mk_node e t = {loc = t; node = e}


let type_left_value e = 
  match e.node with 
    | Eident _ -> true
    | Edot _ -> true
    | Eunop (unop, _) -> 
        begin match unop with 
          | Ustar -> true 
          | _ -> false
        end
    | _ -> false 

(* retourne le type attendu par un binop *)
let expected_type = function
  | Upost_inc | Upre_inc | Upre_dec 
  | Upost_dec | Uminus | Uplus 
  | Unot -> EXnum
  | Ustar -> EXpointer
  | Uamp -> EXleft_value

 
let type_unop unop e = 
  match unop with 
    | Upost_inc | Upre_inc | Upre_dec 
    | Upost_dec  -> 
        if not (is_num e.loc) then raise Incompatible; 
        begin match e.loc with Tpointer _ -> e.loc | _ -> Tint end
    | Uminus | Uplus 
    | Unot -> if not (is_num e.loc) then raise Incompatible; Tint
    | Ustar -> value_pointer e.loc
    | Uamp -> if not (type_left_value e) then raise Incompatible;
              Tpointer e.loc

let type_binop binop e1 e2 = 
  match binop with 
    | Bneq | Blt | Ble | Bgt | Bge | Beq 
    | Bmul | Bdiv | Bmod | Band | Bor -> 
        if not ((compatible e1.loc Tint) && 
                (compatible e2.loc Tint)) then raise Incompatible; 
        Tint
    | Badd ->
        begin match e1.loc, e2.loc with 
          | Tint, Tpointer t | Tpointer t , Tint ->
              Tpointer t
          | Tnull, Tpointer t | Tpointer t , Tnull ->
              Tpointer t
          | Tpointer _, Tpointer _ -> raise Incompatible
          | _ -> if not (compatible e1.loc e2.loc) then raise Incompatible;
                 Tint 
        end
    | Bsub ->
        begin match e1.loc, e2.loc with 
          | Tpointer t1, Tpointer t2 ->
              if not (compatible t1 t2) then raise Incompatible; Tint
          | Tpointer t1, t2  -> if not (is_num t2) then raise Incompatible;
                               Tpointer t1
          | t1, Tpointer t2 -> raise Incompatible
          | t1, t2 -> 
              if not (compatible t1 t2) then raise Incompatible; Tint
        end 

                                                                           


let rec type_expr env expr = 
  match expr.node with 
    (* Expression null *)
    | Enull -> mk_node Enull Tnull
    (* Expression constante entiere ou chaine de caractere *)
    | Econst cst -> 
        begin match cst with 
          (* 0 correspond au type null *)
          | Cint v when (Int32.compare v Int32.zero) = 0 -> 
              mk_node Enull Tint
          | Cint v -> mk_node (Econst (Cint v)) Tint
          (* en C une chaine est un char* *)
          | Cstring v -> mk_node (Econst (Cstring v)) (Tpointer(Tchar))
        end
    (* Expression id *)
    | Eident id ->
        (* Environnement local *)
        begin try mk_node (Eident id) (VarMap.find id.node env)
        with Not_found -> 
        (* Environnement global *)
          begin try mk_node (Eident id) (Hashtbl.find g_var_env id.node)
            with Not_found ->
              raise (Error ((Unbound_var id.node), id.loc));
          end
        end
    (* Expression sizeof *)
    | Esizeof ct -> mk_node (Esizeof ct) Tint
    (* Expression dot *)
    | Edot (e, id) -> 
        let e' = type_expr env e in 
        (* on verifie que e est une struct/union *)
        let glob_env, s = match e'.loc with 
          | Tstruct s -> g_struct_env, s
          | Tunion s -> g_union_env, s
          | _ -> raise (Error ((Not_a_struct id.node), e.loc)) in
        let loc_env = try Hashtbl.find glob_env s.node 
          with Not_found -> raise (Error (Var_typing_error (
            VTE_unbound (e'.loc), s.node), e.loc)) in
        (* et que id est un membre *)
        let t = try (VarMap.find id.node loc_env)
          with Not_found ->
          raise (Error ((Unbound_member (e'.loc, id.node)), id.loc)) in
        mk_node (Edot (e', id)) t 
    (* Expression operateur unaire *)
    | Eunop (unop, e) ->
        (* on type e *)
        let e' = type_expr env e in
        (* on verifie que unop est compatible avec e *)
        let t = try type_unop unop e' with 
          Incompatible -> raise (Error ((Wrong_expected_argument 
          (AWunop unop, e'.loc, expected_type unop), e.loc)))
        in 
          mk_node (Eunop (unop, e')) t
    (* Expression assignation *)
    | Eassign (e1, e2) -> 
          let e1', e2' = type_expr env e1, type_expr env e2 in  
          (* on verifie que e1 est une left_value *)
          if not (type_left_value e1') then raise (Error (Bad_assign, e1.loc));
          (* et que les deux types sont compatibles*)
          if not (compatible e1'.loc e2'.loc) then 
            raise (Error (Not_compatible (e1'.loc, e2'.loc), e1.loc));
          mk_node (Eassign (e1', e2')) e2'.loc
    (* Expression operateur binaire *)
    | Ebinop (binop, e1, e2) -> 
        let e1', e2' = type_expr env e1, type_expr env e2 in  
        begin try mk_node (Ebinop(binop, e1', e2')) (type_binop binop e1' e2')
        with Incompatible -> 
          raise (Error (Invalid_operand 
          (binop, e1'.loc, e2'.loc), e1.loc)) end
    (* Expression operateur binaire *)
    | Ecall (id, exprs) ->
        (* On verifie que la fonction existe*)
        let t, map = try Hashtbl.find g_fun_env id.node with 
          Not_found -> 
            raise (Error (Unbound_fun (id.node), id.loc)) in 
        (* on type les arguments en passant la liste dans l'ordre *)
        let exprs' = List.map (fun e -> type_expr env e, e.loc) exprs in
        type_args id map exprs' 1;
        (* on recreer les arguments avec leur type une fois verifie *)
        let exprs' = List.map (type_expr env) exprs in
        mk_node (Ecall(id, exprs')) t

        
and type_args id params exprs n = 
  (* On parcourt les deux listes en même temps *)
  match params, exprs with 
    | [], _::_ -> 
        (* si la liste des params est vide avant, args manquants *)
        raise (Error (Too_many_args id.node, id.loc))
    | _::_, [] -> 
        (* si la liste des params est vide avant, args en trop *)
        raise (Error (Missing_args id.node, id.loc))
    | [], [] -> ()
    | p::params, (a, loc)::exprs -> 
        let p_t, _ = p in 
        if not (compatible p_t a.loc) then
          raise (Error (
            Wrong_expected_argument (AWfun (n, id.node), 
            p_t, EXc_type a.loc), loc));
            (* on increment le compteur pour le numero d'argument *)
        type_args id params exprs (n + 1)


let rec type_stmt env stmt ret = 
  match stmt.node with 
    (* Instruction expression *)
    | Sexpr expr -> mk_node (Sexpr (type_expr env expr)) Tvoid
    (* Instruction vide *)
    | Sskip -> mk_node Sskip Tvoid
    (* Instruction if *)
    | Sif (expr, s1, s2) -> 
        let et = type_expr env expr in
        if not (is_num et.loc) then raise (Error (Wrong_expected_argument 
          (AWif, et.loc, EXnum), expr.loc));
        let ts1 = type_stmt env s1 ret in
        let ts2 = type_stmt env s2 ret in 
        mk_node (Sif(et, ts1, ts2)) Tvoid
    (* Instruction while *)
    | Swhile (expr, stmt) -> 
        let et =  type_expr env expr in
        if not (is_num et.loc) then 
          raise (Error (Wrong_expected_argument (AWwhile, et.loc, EXnum), 
          expr.loc));
        let ts = type_stmt env stmt ret in
        mk_node (Swhile(et, ts)) Tvoid
    (* Instruction for *)
    | Sfor (sl1, e, sl2, s) -> 
         let et = type_expr env e in 
         if not (is_num et.loc) then 
          raise (Error (Wrong_expected_argument (AWfor, 
          et.loc, EXnum), e.loc));
         let tsl1 = type_stmts env sl1 ret in
         let tsl2 = type_stmts env sl2 ret in
         let ts = type_stmt env s ret in 
          mk_node (Sfor(tsl1, et, tsl2, ts)) Tvoid 
    (* Instruction bloc *)
    | Sblock block -> mk_node (Sblock(type_bloc env block ret false)) ret
    (* Instruction return sans parametre *)
    | Sreturn None -> mk_node (Sreturn None) Tvoid  
    (* Instruction return avec parametre *)
    | Sreturn (Some expr) -> let et = type_expr env expr in
    if not(compatible et.loc ret) then
      raise (Error (Wrong_expected_argument (AWreturn, 
          et.loc, EXc_type ret), expr.loc));
     mk_node (Sreturn (Some et)) Tvoid


and type_stmts env stmts ret = 
  match stmts with
    | [] -> []
    | stmt :: stmts -> let s = type_stmt env stmt ret in 
                       s:: type_stmts env stmts ret

and type_bloc env b typ func = 
  let vars, stmts = b in 
  let var_tbl = Hashtbl.create 17 in
  if func then 
    VarMap.iter (fun k v -> Hashtbl.add var_tbl k v) env;
  let vars' = type_vars var_tbl vars CTXTstd in
  let stmts' = type_stmts (map_of_vars vars' env) stmts typ in
  vars', stmts' 

let is_main typ id vars =
  if id.node = "main" && typ = Tint then
    match vars with 
    | [] -> true
    | (t_argc,id_argc)::(t_argv, id_argv)::[] -> 
        t_argc = Tint && id_argc.node = "argc" && 
        t_argv = Tpointer (Tpointer (Tchar)) && id_argv.node = "argv"
    | _ -> false
  else false

let type_fun typ id vars b =
  type_type typ id CTXTfun;
  if Hashtbl.mem g_fun_env id.node then 
    raise (Error ((FunAlreadyExist id.node), id.loc));
  begin
    let arg_tbl = Hashtbl.create 17 in 
    let args = type_vars arg_tbl vars CTXTstd in
    (* On ajoute la fonction à l'env avant de traiter le bloc *)
    Hashtbl.add g_fun_env id.node (typ, args);
    let bloc = type_bloc (map_of_vars args VarMap.empty) b typ true in
    if is_main typ id vars then main_exist := true;
    typ, id, args, bloc
  end

let type_decl d = 
  match d with 
    (* Declaration de fonctions *)
    | Dfun (typ, id, vars, b) -> 
        let typ, id, vars, b = type_fun typ id vars b in
        Dfun (typ, id, vars, b) 
    (* Declaration de structure *)
    | Dstruct (id, l) ->
        let id, l = type_struct id l in 
        Dstruct (id, l)
    (* Declaration d'union *)
    | Dunion (id, l) ->
        let id, l = type_union id l in 
        Dunion (id, l)
    (* Declaration de variables *)
    | Dvars vars_decl -> 
        Dvars (type_vars g_var_env vars_decl CTXTstd)


(* let g_fun_env : (string, c_type * ((c_type * ident) list)) Hashtbl.t=  *)


(* Ajout des fonctions putchar et sbrk *)
let () = 
  Hashtbl.add g_fun_env "putchar" 
  (Tint, [Tint, (mk_node "n" (Lexing.dummy_pos, Lexing.dummy_pos))]);
  Hashtbl.add g_fun_env "_putint" 
  (Tint, [Tint, (mk_node "n" (Lexing.dummy_pos, Lexing.dummy_pos))]);
  Hashtbl.add g_fun_env "_putstring" 
  (Tint, [Tpointer Tchar, (mk_node "ps" (Lexing.dummy_pos, Lexing.dummy_pos))]);
  Hashtbl.add g_fun_env "sbrk" 
  (Tint, [Tpointer Tvoid, (mk_node "n" (Lexing.dummy_pos, Lexing.dummy_pos))])


let rec type_file l = 
  match l with
    | [] -> 
        if not !main_exist then raise No_main_error;[]
    | decl :: decls -> 
      let d = type_decl decl in d :: type_file decls





