/* Parseur pour le compilateur C */

%{
  open Ast

  (* déclarateurs:
     représentation intermédiaire permettant de convertir par ex:
     int x, **y, *z[10];
     en (int, x); (int**, y), (int*[10], z)
  *)

  let mk_loc e l = { loc = l; node = e }

  let loc e =
    mk_loc e (Parsing.symbol_start_pos (),Parsing.symbol_end_pos())

  let loc_i i e =
    mk_loc e (Parsing.rhs_start_pos i, Parsing.rhs_end_pos i)

  let loc_dummy e =
    mk_loc e (Lexing.dummy_pos, Lexing.dummy_pos)


  type declarator =
    | Dident of ident
    | Dpointer of declarator
    | Darray of declarator * int

  let rec declarator ty = function
    | Dident id -> ty, id
    | Dpointer d -> declarator (Tpointer ty) d
    | Darray (d,s) -> declarator (Tarray (ty, s)) d

%}

%token <string> IDENT
%token <int32> INTEGER
%token <string> STRING

/* Mots clés */

%token FOR IF ELSE WHILE RETURN SIZEOF
%token VOID INT CHAR STRUCT UNION

/* Symboles */

%token LPAR RPAR LBRACE RBRACE LSQUARE RSQUARE
%token SEMICOLON COMMA DOT ARROW
%token EOF

/* Opérateurs */

%token EQ
%token OR
%token AND
%token <Ast.binop> EQOP
%token <Ast.binop> COMP
%token PLUS MINUS
%token STAR SLASH PERCENT
%token PLUSPLUS MINUSMINUS BANG AMPERSAND

/*s Précédences */

%nonassoc then
%nonassoc ELSE

%right EQ
%left OR /* || */
%left AND     /* && */
%left EQOP                   /* == != */
%left COMP                   /* < <= > >= */
%left PLUS MINUS             /* + - */
%left STAR SLASH PERCENT     /* * / % */
%right ustar uminus uplus PLUSPLUS MINUSMINUS BANG AMPERSAND
                             /* + - ++ -- ! & */
%left DOT ARROW LSQUARE par_expr

/*s Point d'entrée */

%start file
%type <Ast.loc Ast.file> file

%%

file:
| decl_star EOF
    { List.rev $1 }
;

/* déclarations */

decl_star:
| /* epsilon */ { [] }
| decl_star decl { $2 :: $1 }
;

decl:
| decl_vars
    { Dvars $1 }
| STRUCT ident LBRACE decl_vars_star RBRACE SEMICOLON
    { Dstruct ($2, $4) }
| UNION ident LBRACE decl_vars_star RBRACE SEMICOLON
    { Dunion ($2, $4) }
| c_type_fun LPAR parameters_opt RPAR block
    { let ty, id = $1 in Dfun (ty, id, $3, $5) }
;

c_type_fun:
| c_type var
    { match declarator $1 $2 with
    | Tarray _, _ -> raise Parsing.Parse_error
    | tid -> tid }
;

decl_vars:
| c_type vars SEMICOLON { List.map (declarator $1) $2 }
;

decl_vars_star:
| /* epsilon */ { [] }
| decl_vars decl_vars_star { $1 @ $2 }
;

parameters_opt:
| /* epsilon */ { [] }
| parameters    { $1 }
;

parameters:
| c_type var { [declarator $1 $2] }
| c_type var COMMA parameters { declarator $1 $2 :: $4 }
;

c_type:
| VOID { Tvoid }
| INT { Tint }
| CHAR { Tchar }
| STRUCT ident { Tstruct $2 }
| UNION ident  { Tunion $2 }
;

vars:
| var { [$1] }
| var COMMA vars { $1 :: $3 }
;

var:
| ident
    { Dident $1 }
| STAR var
    { Dpointer $2 }
| var LSQUARE INTEGER RSQUARE
    { Darray ($1, Int32.to_int $3) }
;

expr:
| expr EQ expr
    { loc (Eassign ($1, $3)) }
| MINUSMINUS expr
    { loc (Eunop (Upre_dec, $2)) }
| PLUSPLUS expr
    { loc (Eunop (Upre_inc, $2)) }
| expr PLUSPLUS
    { loc (Eunop (Upost_inc, $1)) }
| expr MINUSMINUS
    { loc (Eunop (Upost_dec, $1)) }
| expr OR expr
    { loc (Ebinop (Bor, $1, $3)) }
| expr AND expr
    { loc (Ebinop (Band, $1, $3)) }
| expr EQOP expr
    { loc (Ebinop ($2, $1, $3)) }
| expr COMP expr
    { loc (Ebinop ($2, $1, $3)) }
| expr PLUS expr
    { loc (Ebinop (Badd, $1, $3)) }
| expr MINUS expr
    { loc (Ebinop (Bsub, $1, $3)) }
| expr STAR expr
    { loc (Ebinop (Bmul, $1, $3)) }
| expr SLASH expr
    { loc (Ebinop (Bdiv, $1, $3)) }
| expr PERCENT expr
    { loc (Ebinop (Bmod, $1, $3)) }
| SIZEOF LPAR cplx_type RPAR
    { loc (Esizeof ($3)) }
| PLUS expr %prec uplus
    { loc (Eunop (Uplus, $2)) }
| MINUS expr %prec uminus
    { loc (Eunop (Uminus, $2)) }
| BANG expr
    { loc (Eunop (Unot, $2)) }
| AMPERSAND expr
    { loc (Eunop (Uamp, $2)) }
| INTEGER
    { if $1 = 0l then loc Enull else loc (Econst (Cint $1)) }
| STRING
    { loc (Econst (Cstring $1)) }
| ident LPAR l_expr_opt RPAR
    { loc (Ecall ($1, $3)) }
| ident
    { loc (Eident $1) }
| STAR expr %prec ustar
    { loc (Eunop (Ustar, $2)) }
| expr LSQUARE expr RSQUARE
    { 
      let sum = loc (Ebinop(Badd, $1, $3)) in
	loc (Eunop(Ustar,sum))
    }
| expr DOT ident
    { loc (Edot ($1, $3)) }

| expr ARROW ident
    {
      let star = loc (Eunop(Ustar, $1)) in
      loc (Edot (star, $3))
    }
| LPAR expr RPAR  %prec par_expr
    { $2 }
;

statement:
| SEMICOLON
   { loc Sskip }
| expr SEMICOLON
   { loc (Sexpr $1) }
| IF LPAR expr RPAR statement %prec then
   { loc (Sif ($3, $5, loc_dummy Sskip)) }
| IF LPAR expr RPAR statement ELSE statement
   { loc (Sif ($3, $5, $7)) }
| FOR LPAR l_expr_opt SEMICOLON expr_or_1 SEMICOLON l_expr_opt RPAR statement
   { let l_expr i = List.map (fun e -> loc_i i (Sexpr e)) in
     loc (Sfor (l_expr 3 $3, $5, l_expr 7 $7, $9)) }
| WHILE LPAR expr RPAR statement
   { loc (Swhile ($3, $5)) }
| block
   { loc (Sblock $1) }
| RETURN SEMICOLON
   { loc (Sreturn None) }
| RETURN expr SEMICOLON
   { loc (Sreturn (Some $2)) }
;

cplx_type:
|  c_type { $1 }
| c_type STAR { Tpointer $1 }
;
expr_or_1:
| /* epsilon */ {  loc (Econst (Cint 1l)) }
| expr { $1 }
;

l_expr_opt:
| /* epsilon */ { [] }
| l_expr        { $1 }
;

l_expr:
| expr { [$1] }
| expr COMMA l_expr { $1 :: $3 }
;

statement_star:
| /* epsilon */ { [] }
| statement statement_star { $1 :: $2 }
;

block:
| LBRACE decl_vars_star statement_star RBRACE { $2, $3 }
;

/* identificateurs localisés */

ident:
| IDENT { loc $1 }
;
