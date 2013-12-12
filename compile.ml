
(*
  TODO : 
  - gerer les struct/union
  - gerer le dot
  - creer sbrk
*)


open Format
open Ast
open Mips


(*environnement regourpant les différentes variables des unions*)
let union_size_env : (string, int * ((c_type * ident * int) list)) Hashtbl.t= 
  Hashtbl.create 17

(*environnement regourpant les différentes variables des structures*)
let struct_size_env : (string, int * ((c_type * ident * int) list)) Hashtbl.t= 
  Hashtbl.create 17

(*environnement des fonctions*)
let fun_size_env = Hashtbl.create 17
(*environnement des variables locales*)
let var_env = Hashtbl.create 17
(*environnement des variables globales*)
let var_global_env = Hashtbl.create 17

(*
  $sp -> sommet de la pile 
  $fp -> frame pointer
*)

(*mettre sur la pile*)
let push r = 
  mips [
    Comment "push:";
    Binop (Sub, SP, SP, Oimm 4); (*réservation de l'espace*)
    Sw (r, Areg (0, SP));](*ajout sur la pile de la donnée depuis un registre*)

(*retirer de la pile*)
let pop r = 
  mips [
    Comment "pop";
    Lw (r, Areg (0, SP));(* chargemnet de la donnée dans un registre donné en paramètre*)
    Binop (Add, SP, SP, Oimm 4);]


let putchar () = 
  Hashtbl.add fun_size_env "putchar" (4, 4, 0);
  mips [Label "fun_putchar";] ++ pop A0
  ++ mips [Li (V0, 11); Syscall;]
  ++ mips [
    Label ("end_" ^ "putchar");
    Jr RA;
    Comment "End function"; 
  ]


(*affichage d'un entier*)
let _putint () = 
  Hashtbl.add fun_size_env "_putint" (4, 4, 0);
  mips [Label "fun__putint";] ++ pop A0 
  ++ mips [Li (V0, 1); Syscall;]
  ++ mips [
    Label ("end_" ^ "_putint");
    Jr RA; 
    Comment "End function"; 
  ]

let _putstring () = 
  Hashtbl.add fun_size_env "_putstring" (4, 4, 0);
  mips [Label "fun__putstring";] ++ pop A0
  ++ mips [Li (V0, 4); Syscall;
    Label ("end_" ^ "_putstring");
    Jr RA;
    Comment "End function"; 
  ]

let sbrk () =     
  Hashtbl.add fun_size_env "sbrk" (4, 4, 0);
  mips [Label "fun_sbrk";] ++ pop A0 
  ++ mips [Li (V0, 9); Syscall;
    Sw (V0, Areg (0, SP));
    Label ("end_" ^ "sbrk");
    Jr RA; 
    Comment "End function"; 
  ]
    
(* équivalent binop mips ast*)
let mips_binop_of_ast = function
  | Beq -> Eq
  | Bneq -> Ne
  | Blt -> Lt
  | Ble -> Le
  | Bgt -> Gt
  | Bge -> Ge
  | Badd -> Add
  | Bsub -> Sub
  | Bmul -> Mul
  | Bdiv -> Div
  | Bmod -> Rem
  | Band -> And
  | Bor -> Or

    
module VarMap = Map.Make (String)

let unique = ref 0
(*incrémentation pour nommage unique*)
let next () = let v = !unique in incr unique; v 


let data_list = ref []

(*Renvoie la taille des différents types*)
let sizeof t = 
  match t with 
  | Tint -> 4 
  | Tchar -> 1
  | Tvoid -> 0
  | Tstruct s -> let size,_ = Hashtbl.find struct_size_env s.node in size
  | Tunion s -> let size,_ = Hashtbl.find union_size_env s.node in size
  | Tpointer _ -> 4
  | Tarray _ -> 4
  | Tnull -> 4  


let lval_sizeof t = 
  match t with 
  | Tpointer Tvoid -> 1
  | Tpointer t -> sizeof t
  | t -> sizeof t

      
(*Revoie l'alignement des différents types*)
let aligned_sizeof t = 
  match t with 
  | Tint | Tchar | Tpointer _ | Tarray _ | Tnull -> 4  
  | Tvoid -> 0
  | Tstruct s -> 
      let size,_ = Hashtbl.find struct_size_env s.node in
      let aligned_size = size + (4 - (size mod 4)) in(*?*)
      aligned_size 
  | Tunion s -> 
      let size,_ = Hashtbl.find union_size_env s.node in
      let aligned_size = size + (4 - (size mod 4)) in
      aligned_size 

(*Permet d'aligner, si struct on calcul les différents types de la structure *)
let rec alignof t = 
  match t with 
  | Tunion _ | Tint | Tpointer _ | Tarray _ 
  | Tnull | Tchar | Tvoid -> (sizeof t)
  | Tstruct s -> 
      let _, l = Hashtbl.find struct_size_env s.node in 
      List.fold_left (
        fun acc e ->
          let t, _, _ = e in max acc (alignof t)
      ) 0 l
        
(*calcul de la taille d'une structure*)
let size_struct id l is_struct = 
  let lref = ref [] in
  let max_align = ref 0 in 
  let ht = if is_struct then struct_size_env else union_size_env in
  let size = List.fold_left (
    fun size e -> 
      let t, id = e in
      let size_t = sizeof t in
      let padding = if size mod (alignof t) = 0 then 0
        else 4 - (size mod (alignof t)) in
      max_align := max (alignof t) !max_align;
      lref := (t, id, size + padding + size_t) :: !lref;
      if is_struct then size + padding + size_t else max size size_t
  ) 0 l in 
  let align = size mod !max_align in
  let padding = if align = 0 then 0 else 4 - align in
  let total_size = size + padding in
  Hashtbl.add ht id.node (total_size, List.rev !lref)

let memcpy t = 
  let size, range = sizeof t, alignof t in 
  mips [Comment "memcpy :" ] ++
    let rec memcpy size_acc code = 
      if size = 0 || size_acc < 0 then code else
      let load =
        if size <= 1 then mips [
        Lbu (T3, Areg (size_acc, T0));
        Sb (T3, Areg (size_acc, T1));
      ]
        else mips [
          Lw (T3, Areg (size_acc, T0));
          Sw (T3, Areg (size_acc, T1));
        ] in 
      memcpy (size_acc - range) (
        code
        ++ load) in
    memcpy (size - range) nop ++ mips [Comment "end memcpy" ]
 
    
(*Formatage de DVars en mips*)
let rec compile_global (typ, id) = 
  let align = Dalign 4 in 
  let label = Dlabel id.node in 
  let espace = Dspace (aligned_sizeof typ) in 
  data_list := align::label::espace::!data_list

(*Calcule de la taille  d'une fonction*)
let comput_size fact fs vars =
  let dec = if fact > 0 then 8 else -4 in 
  List.fold_left (
    fun acc_fp (typ, id) ->
      Hashtbl.add var_env id.node acc_fp;
      acc_fp + fact * (aligned_sizeof typ) 
  ) (dec + fs) vars - dec - fs


let unary_incr_decr range unop = 
  match unop with
  | Upre_inc ->
      mips [ Binop (Add, T1, T1, Oimm range);
             Sw (T1, Areg (0, T2));
           ]
      ++ push T1
  | Upre_dec ->
      mips [
        Binop (Sub, T1, T1, Oimm range);
        Sw (T1, Areg (0, T2));
      ]
      ++ push T1
  | Upost_inc ->
      push T1
      ++ mips [
        Binop (Add, T1, T1, Oimm range);
        Sw (T1, Areg (0, T2));
      ]
  | Upost_dec ->
      push T1
      ++ mips [
        Binop (Sub, T1, T1, Oimm range);
        Sw (T1, Areg (0, T2));
      ]
  | _ -> nop

      
(*gestion des unop*)
let rec compile_unop unop e =
  match unop with
  | Upre_inc | Upost_inc | Upre_dec | Upost_dec -> 
      let range = begin match e.loc with
      | Tpointer Tvoid -> 1
      | Tpointer t -> sizeof t
      | _ -> 1 end in 
      compile_lval T2 e
      ++ mips [Lw (T1, Areg (0, T2));]
      ++ unary_incr_decr range unop
        
  | Ustar ->
      let load = begin match e.loc
        with Tpointer t ->
          if (sizeof t) <= 1 then Lbu (T1, Areg (0, T1))
          else Lw (T1, Areg (0, T1))
        | _ -> assert false end in
      compile_expr e
      ++ pop T1
      ++ mips [load;]
      ++ push T1
  | Uamp -> compile_lval T1 e ++ push T1
  | Unot ->
      compile_expr e ++
        pop T1 ++
        mips [Binop (Eq, T1, T1, Oimm 0);] ++
        push T1
  | Uminus ->
      compile_expr e ++
        pop T1 ++
        (mips [
          Binop (Sub, T1, ZERO, Oreg T1);
        ]) ++
        push T1
  | Uplus -> compile_expr e

and compile_lval r e = 
  match e.node with 
  | Eident id -> 
      begin try mips [Binop (Add, r, FP, 
                             Oimm (- (Hashtbl.find var_env id.node)));]
        with Not_found -> 
          mips [La (r, id.node)] end
  | Edot _ -> assert false (* TODO *)
  | Eunop (unop, e1) -> 
      begin match unop with 
      | Ustar ->
          compile_expr e1 ++ pop r 
      | _ -> assert false
      end
  | _ -> assert false
      
(*Compilation des expression*)
and compile_expr expr = 
  match expr.node with
  | Enull -> push ZERO
  | Esizeof t -> 
      let size = sizeof t in
      mips [
        Li (T1, size);
      ] ++ push T1
  | Econst c ->
      begin match c with 
      | Cint i -> 
          mips [
            Li32 (T1, i);
          ] ++ push T1 
      | Cstring s ->
          let lab = "string_" ^ string_of_int(next ()) in
          data_list := Dlabel lab
          :: (Dasciiz s)
          :: !data_list;
          mips [ La (T1, lab); ] ++ push T1
      end
  | Eassign (e1, e2) ->
      compile_expr e2
      ++ mips [ Move (T0, SP) ]
      ++ compile_lval T1 e1
      ++ memcpy e1.loc
  | Eunop (unop, e) -> compile_unop unop e
  | Ebinop (binop, e1, e2) -> compile_binop binop e1 e2
  | Ecall (id, el) -> 
      let sizeof_t, _, fp_bloc = Hashtbl.find fun_size_env id.node in 
      mips [Binop (Sub, SP, SP, Oimm (sizeof_t));] ++
        List.fold_left (fun acc e -> compile_expr e ++ acc) nop el ++
        mips [ Jal ("fun_" ^ id.node);]
  | Eident id -> 
      begin
        try
          let pos = Hashtbl.find var_env id.node in
          mips [
            Binop (Add, T2, FP, Oimm (-pos));
            Lw (T1, Areg (0, T2));
          ]
        with Not_found ->
          mips [
            La (T1, id.node);
            Lw (T1, Areg (0, T1));
          ]
      end
      ++ push T1
  | Edot (e, id) ->  assert false (* 
      let t = e.loc in
      let name = begin match t with Tstruct idS -> idS.node
      | _ -> assert false end in
      let _, fields = Hashtbl.find struct_size_env name in
      let t, id, pos = List.find (fun (_, id, _) -> id.node = name) fields in
      compile_expr e ++ pop T1
      ++ mips [
        Binop (Sub, T1, T1, Oimm pos);
      ]*)
      
      
and compile_binop op e1 e2 =
  match op with 
  | Beq | Bneq | Blt | Ble | Bgt | Bge | Bdiv | Bmul
  | Bmod -> 
      compile_expr e1 ++ compile_expr e2 ++ pop T2 ++ pop T1 
      ++ mips [ Binop (mips_binop_of_ast op, T1, T1, Oreg T2) ] ++ push T1
  | Badd | Bsub ->
      compile_expr e1 ++ compile_expr e2 ++ pop T2 ++ pop T1 
      ++ begin match e1.loc, e2.loc with
      | Tpointer t, Tint ->
          let size = if t = Tvoid then 1 else sizeof t in
          mips [ Binop (Mul, T2, T2, Oimm size) ]
      | Tint, Tpointer t ->
          let size = if t = Tvoid then 1 else sizeof t in
          mips [ Binop (Mul, T1, T1, Oimm size) ]
      | _, _ -> nop
      end
      ++ mips [ Binop (mips_binop_of_ast op, T1, T1, Oreg T2) ] ++ push T1
  | Band -> 
      let lab = "end_and_" ^ string_of_int (next ()) in
      compile_expr e1 ++ pop T1
      ++ mips [Beqz (T1, lab);]
      ++ compile_expr e2 ++ pop T2
      ++ mips [
        Move (T1, T2);
        Binop (Ne, T1, T1, Oreg ZERO);
        Label lab;
      ] ++ push T1
  | Bor ->
      let lab = "end_and_" ^ string_of_int (next ()) in
      compile_expr e1 ++ pop T1
      ++ mips [Bnez (T1, lab);]
      ++ compile_expr e2 ++ pop T2
      ++ mips [
        Move (T1, T2);
        Label lab;
        Binop (Ne, T1, T1, Oreg ZERO);
      ] ++ push T1

(*compilation des instructions*)

let rec compile_stmt stmt fp_res fun_name = 
  match stmt.node with
  | Sif(e, s1, s2) -> 
      let else_stmt = compile_stmt s1 fp_res fun_name in
      let then_stmt = compile_stmt s2 fp_res fun_name  in 

        (*compilation de expression et des blocs *)
      let then_la = "if_then_" ^ (string_of_int (next())) in
      let else_la = "if_else_" ^ (string_of_int (next())) in
      let end_la = "if_end_" ^ (string_of_int (next())) in  
      compile_expr e 
      ++ pop T1 ++ mips [
        Bnez (T1,else_la);
        Label then_la;
      ]
      ++ then_stmt
      ++ mips [
        Jal end_la;
        Label else_la;
      ]
      ++ else_stmt
      ++ mips [
        Label end_la;
      ]
        
  | Sfor (sl1,e,sl2,s) ->  
      let loop = "loop" ^ (string_of_int (next())) in
      let fin = "loop_end" ^ (string_of_int (next())) in
      let corp = compile_stmt s fp_res fun_name in
      let  liststmt l stmt = compile_stmt stmt fp_res fun_name ++ l in
      let result1 = List.fold_left liststmt nop sl1 in
      let result2 = List.fold_left liststmt nop sl2 in
      result1 
      ++ compile_expr e ++ pop T1
      ++ mips[Label loop;]
      ++ mips [Beqz(T1,fin);]
      ++ corp
      ++ result2
      ++ compile_expr e ++ pop T1 ++
        mips[
          Jal loop;
          Label fin;
        ]	
  | Sreturn (Some e) -> 
  	let size_ret, size_arg, _ = Hashtbl.find fun_size_env fun_name in
        let fun_name_end = "end_" ^ fun_name in
      compile_expr e 
      ++ mips [Move (T0, SP)]  
      ++ mips[  
          Binop(Add, T1, FP, Oimm(size_arg + size_ret));]
      ++ memcpy e.loc
      ++ mips [ Binop (Add, SP, SP, Oimm (size_ret)) ]
      ++ mips [ Jal fun_name_end;] 
  | Sreturn (None) ->
      let fun_name_end = "end_" ^ fun_name in
      mips[	
    	Jal fun_name_end
      ] 
        
  | Sblock b -> 
      let var_decls, statements = b in
      let fp_bloc, cb = compile_bloc var_decls statements fp_res fun_name in 
      mips [Binop (Sub, SP, SP, Oimm (fp_bloc));]
      ++ cb
      ++ mips [Binop (Add, SP, SP, (Oimm fp_bloc));]

  | Sexpr e -> compile_expr e ++
      mips [ Binop (Add, SP, SP, Oimm (aligned_sizeof e.loc))]
  | Sskip -> nop
  | Swhile (e, st)-> 
      let stmt = compile_stmt st fp_res fun_name in
      let loop = "loop_" ^ (string_of_int (next())) in
      let test = "test_" ^ (string_of_int (next())) in
        (* let exit = "exit_" ^ (string_of_int (next())) in *)
      compile_expr e ++ pop T1
      ++ mips [Jal test;]
      ++ mips [
        Label loop;
      ]
      ++ stmt
      ++ compile_expr e ++ pop T1
      ++ mips [
        Label test;
        Bnez(T1,loop);
      ]
        
(*compilation d'un block*)
and compile_bloc vars stmts fs fun_name =
  let fp_res = comput_size 1 fs vars in 
  fp_res, List.fold_left (fun cs s -> cs ++
    compile_stmt s (fp_res + fs) fun_name) nop stmts


(*
  générer le code qui créé la taille voulue pour la fonction
  * *)

let compile_fun t id vars b =
  let size_args = - (comput_size (-1) 0 vars) in
  let vars, stmts = b in
  Hashtbl.add fun_size_env id.node (aligned_sizeof t, size_args, 0);
  let fp_bloc, cb = compile_bloc vars stmts 0 id.node in
  Hashtbl.add fun_size_env id.node (aligned_sizeof t, size_args, fp_bloc);
  let lbl = (mips [
    Comment ("Begin function");
    Label ("fun_" ^ id.node); 
  ]) 
    ++ push FP
    ++ mips [Move (FP, SP);]
    ++ push RA
    ++ mips [Binop (Sub, SP, SP, Oimm fp_bloc); Comment "body";]
  in
 
  let end_fun =
    mips [Label ("end_" ^ id.node); Binop (Add, SP, SP, Oimm fp_bloc);]
    ++ pop RA
    ++ pop FP
    ++ mips [ Binop (Add, SP, SP, Oimm (size_args)) ]
    ++ mips [Jr RA; Comment "End function";]
  in
  lbl ++ cb ++ end_fun


(*compilation de déclaration*)
let compile_decl d = 
  match d with 
  | Dfun (typ, id, vars, b) ->
      compile_fun typ id vars b
  | Dstruct (id, l) -> 
      size_struct id l true; mips []
  | Dunion (id, l) ->
      size_struct id l false; mips []
  | Dvars vars_decl ->
      List.iter compile_global vars_decl; mips []

(* formatage du fichier *)
let compile_file ast =
  let init = mips [
    Comment "Begin program";
    Label "main";
    Binop (Sub, SP, SP, Oimm 4);
    Jal "fun_main";
  ] in
  let rec compile_file ast = 
    match ast with 
    | [] -> nop
    | decl :: decls ->
        let decl = compile_decl decl in 
        decl ++ compile_file decls 
  in 
  let primitives = putchar () ++ _putint () ++ _putstring () ++ sbrk() in
  let close = mips [
    Comment "Return";
    Lw (A0, Areg (0, SP));
    Li (V0, 17);
    Syscall;
    Comment "End program";
  ] in
  let c = init ++ close ++ primitives ++ compile_file ast in 
  {text = c; data = !data_list}

(*affichage pour le débugage*)
let debug_struct_size () = 
  Hashtbl.iter (fun k v -> 
    printf "%s of size : %d@\n" k v) var_env






