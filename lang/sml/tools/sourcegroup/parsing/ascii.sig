(* Copyright (c) 1992 by Carnegie Mellon University *)

signature ASCII =
  sig
    val SEMIcolon : int
    val caret : int
    val colon : int
    val comma : int
    val del : int
    val dollar : int
    val dot : int
    val dquote : int
    val equal : int
    val formfeed : int
    val greaterthan : int
    val isDigit : int -> bool
    val lbrace : int
    val lbracket : int
    val lc_a : int
    val lc_n : int
    val lc_t : int
    val lc_z : int
    val lessthan : int
    val lparen : int
    val minus : int
    val newline : int
    val nine : int
    val percent : int
    val plus : int
    val query : int
    val rbrace : int
    val rbracket : int
    val return : int
    val rparen : int
    val sharp : int
    val slash : int
    val space : int
    val squote : int
    val star : int
    val tab : int
    val tilde : int
    val uc_a : int
    val uc_z : int
    val underscore : int
    val zero : int
  end
