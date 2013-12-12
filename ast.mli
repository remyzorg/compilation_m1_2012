(* Arbres de syntaxe abstraite pour le typage *)

type ('info, 'node) node = { loc : 'info;
			     node : 'node }

type loc = Lexing.position * Lexing.position

(* Les identificateurs sont des cha�nes localis�es *)
type ident = (loc, string) node


(* Repr�sentation des types C *)
type c_type =
  | Tnull (* pour typer null *)
  | Tvoid
  | Tint
  | Tchar
  | Tarray of c_type * int
  | Tstruct of ident
  | Tunion of ident
  | Tpointer of c_type

(* Repr�sentation des constantes.
   Les caract�res sont repr�sent�s par des entiers *)
type constant =
  | Cint of int32
  | Cstring of string

(* Op�rateurs unaires *)
type unop =
  | Upre_inc | Upost_inc | Upre_dec | Upost_dec
  | Ustar | Uamp | Unot | Uminus | Uplus

(* Op�rateurs binaires *)
type binop =
  | Beq | Bneq | Blt | Ble | Bgt | Bge | Badd | Bsub | Bmul | Bdiv
  | Bmod | Band | Bor

(* Expressions *)
type 'info expr = ('info, 'info expr_node) node
and 'info expr_node =
  | Enull (* repr�sente l'entier 0 (qui est aussi le pointeur NULL) *)
  | Econst of constant
  | Eident of ident
  | Esizeof of c_type
  | Edot of  'info expr * ident
  | Eassign of 'info expr * 'info expr
  | Eunop of unop * 'info expr
  | Ebinop of binop * 'info expr * 'info expr
  | Ecall of ident * 'info expr list

(* D�claration de variable *)
type var_decl =  c_type * ident

(* Instructions *)
type 'info statement = ('info, 'info statement_node) node
and 'info statement_node =
  | Sskip
  | Sexpr of 'info expr
      (* Un seul cas pour le if. Si le else est absent,
         le parseur ins�re un Sskip pour le cas else *)
  | Sif of 'info expr * 'info statement * 'info statement
  | Swhile of  'info expr * 'info statement
  | Sfor of 'info statement list
    *  'info expr
    * 'info statement list
    * 'info statement
  | Sblock of 'info block
  | Sreturn of 'info expr option

and 'info block = var_decl list * 'info statement list

(* Les d�clarations *)
type 'info decl =
  | Dvars of var_decl list
  | Dstruct of ident * var_decl list
  | Dunion of  ident * var_decl list
  | Dfun of c_type * ident * var_decl list * 'info block

(* Un fichier *)
type 'info file =  'info decl list
