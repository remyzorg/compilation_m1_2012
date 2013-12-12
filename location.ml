(* Définition des locations *)

type t = Lexing.position * Lexing.position
let dummy = Lexing.dummy_pos, Lexing.dummy_pos
let mk x l1 l2 info = { loc = l1, l2;
			node = x;
			info = info }
let mk_dummy x info = { loc = dummy;
                   node = x;
		   info = info }
