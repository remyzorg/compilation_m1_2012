type register =
  | A0 | A1 | V0 | T0 | T1 | T2 | T3 | T4 | S0 | RA | SP | FP | ZERO

type address =
  | Alab of string
  | Areg of int * register

type operand =
  | Oimm of int
  | Oreg of register

type operation = And | Or | Add | Sub | Mul | Div | Rem |
    Eq | Ne | Le | Lt | Ge | Gt

type label = string

type instruction =
  | Move of register * register
  | Li of register * int
  | Li32 of register * int32
  | La of register * label
  | Lw of register * address
  | Sw of register * address
  | Lbu of register * address
  | Sb of register * address
  | Neg of register * register
  | Binop of operation * register * register * operand
  | B of label
  | Beqz of register * label
  | Bnez of register * label
  | Jal of label
  | Jr of register
  | Jalr of register
  | Syscall
  | Label of string
  | Inline of string
  | Comment of string

type code

val nop : code
val mips : instruction list -> code
val inline : string -> code
val (++) : code -> code -> code

type data =
    | Dlabel of string
    | Dalign of int
    | Dasciiz of string
    | Dword of int32 list
    | Dbyte of int
    | Dspace of int
    | Daddress of string

type program = {
  text : code;
  data : data list;
}

val print_program : Format.formatter -> program -> unit

