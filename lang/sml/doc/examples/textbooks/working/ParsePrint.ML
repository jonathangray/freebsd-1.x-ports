(**** ML Programs from the book

  ML for the Working Programmer
  by Lawrence C. Paulson, Computer Laboratory, University of Cambridge.
  (Cambridge University Press, 1991)

Copyright (C) 1991 by Cambridge University Press.
Permission to copy without fee is granted provided that this copyright
notice and the DISCLAIMER OF WARRANTY are included in any copy.

DISCLAIMER OF WARRANTY.  These programs are provided `as is' without
warranty of any kind.  We make no warranties, express or implied, that the
programs are free of error, or are consistent with any particular standard
of merchantability, or that they will meet your requirements for any
particular application.  They should not be relied upon for solving a
problem whose incorrect solution could result in injury to a person or loss
of property.  If you do use the programs or functions in such a manner, it
is at your own risk.  The author and publisher disclaim all liability for
direct, incidental or consequential damages resulting from your use of
these programs or functions.
****)


(*** Basic library module.  From Chapter 9.  ***)

infix mem;

signature BASIC =
  sig
  exception Lookup
  exception Nth
  val minl : int list -> int
  val maxl : int list -> int
  val take : int * 'a list -> 'a list
  val drop : int * 'a list -> 'a list
  val nth : 'a list * int -> 'a
  val mem : ''a * ''a list -> bool
  val newmem : ''a * ''a list -> ''a list
  val lookup : (''a * 'b) list * ''a -> 'b
  val filter : ('a -> bool) -> 'a list -> 'a list
  val exists : ('a -> bool) -> 'a list -> bool
  val forall : ('a -> bool) -> 'a list -> bool
  val foldleft : ('a * 'b -> 'a) -> 'a * 'b list -> 'a
  val foldright : ('a * 'b -> 'b) -> 'a list * 'b -> 'b
  end;
  

functor BasicFUN() : BASIC =
  struct
  fun minl[m] : int = m
    | minl(m::n::ns) = if m<n  then  minl(m::ns)  else  minl(n::ns);

  fun maxl[m] : int = m
    | maxl(m::n::ns) = if m>n  then  maxl(m::ns)  else  maxl(n::ns);

  fun take (n, []) = []
    | take (n, x::xs) =  if n>0 then x::take(n-1,xs)  
			 else  [];

  fun drop (_, [])    = []
    | drop (n, x::xs) = if n>0 then drop (n-1, xs) 
			       else x::xs;

  exception Nth;
  fun nth (l,n) =    (*numbers the list elements [x0,x1,x2,...] *)
	case drop(n,l) of [] => raise Nth
			| x::_ => x;

  fun x mem []  =  false
    | x mem (y::l)  =  (x=y) orelse (x mem l);

  (*insertion into list if not already there*)
  fun newmem(x,xs) = if x mem xs then  xs   else  x::xs;

  exception Lookup;
  fun lookup ([], a) = raise Lookup
    | lookup ((x,y)::pairs, a) = if a=x then y else lookup(pairs, a);

  fun filter pred [] = []
    | filter pred (x::xs) =
	if pred(x) then x :: filter pred xs  
	else  filter pred xs;

  fun exists pred []      = false
    | exists pred (x::xs) = (pred x)  orelse  exists pred xs;

  fun forall pred []      = true
    | forall pred (x::xs) = (pred x)  andalso  forall pred xs;

  fun foldleft f (e, [])    = e
    | foldleft f (e, x::xs) = foldleft f (f(e,x), xs);

  fun foldright f ([],    e) = e
    | foldright f (x::xs, e) = f(x, foldright f (xs,e));
end;


(*** Lexical Analysis -- Scanning.  From Chapter 9.  ***)

(*Formal parameter of LexicalFUN*)
signature KEYWORD =
  sig
  val alphas: string list
  and symbols: string list
  end;

(*Result signature of LexicalFUN*)
signature LEXICAL =
  sig
  datatype token = Id of string | Key of string
  val scan : string -> token list
  end;


(*All characters are covered except octal 0-41 (nul-space) and 177 (del),
  which are ignored. *)
functor LexicalFUN (structure Basic: BASIC 
		    and       Keyword: KEYWORD) : LEXICAL =
  struct
  local open Basic in
  datatype token = Key of string  |  Id of string;

  fun is_letter_or_digit c =
      "A"<=c andalso c<="Z" orelse
      "a"<=c andalso c<="z" orelse
      "0"<=c andalso c<="9";

  val specials = explode"!@#$%^&*()+-={}[]:\"|;'\\,./?`_~<>";

  (*scanning of an alphanumeric identifier or keyword*)
  fun alphanum (id, c::cs) =
	if is_letter_or_digit c then  alphanum (id^c, cs)
				else  (id, c::cs)
    | alphanum (id, []) = (id, []);

  fun tokenof a = if a mem Keyword.alphas  then  Key(a)  else  Id(a);

  (*scanning of a symbolic keyword*)
  fun symbolic (sy, c::cs) =
	if sy mem Keyword.symbols orelse not (c mem specials)
        then  (sy, c::cs)
	else  symbolic (sy^c, cs)
    | symbolic (sy, []) = (sy, []);

  fun scanning (toks, []) = rev toks    (*end of chars*)
    | scanning (toks, c::cs) =
	if is_letter_or_digit c 
	then (*identifier or keyword*)
	     let val (id, cs2) = alphanum(c, cs)
	     in  scanning (tokenof id :: toks, cs2)
	     end
	else if c mem specials
	then (*special symbol*)
	     let val (sy, cs2) = symbolic(c, cs)
	     in  scanning (Key sy :: toks, cs2)
	     end
	else (*spaces, line breaks, strange characters are ignored*)
	     scanning (toks, cs);

  (*Scanning a list of characters into a list of tokens*)
  fun scan a = scanning([], explode a);
  end
  end;


(*** Parsing functionals.  From Chapter 9.  ***)

infix 5 --;
infix 3 >>;
infix 0 ||;

signature PARSE =
  sig
  exception SynError of string
  type token
  val reader: (token list -> 'a * 'b list) -> string -> 'a
  val -- : ('a -> 'b * 'c) * ('c -> 'd * 'e) -> 'a -> ('b * 'd) * 'e
  val >> : ('a -> 'b * 'c) * ('b -> 'd) -> 'a -> 'd * 'c
  val || : ('a -> 'b) * ('a -> 'b) -> 'a -> 'b
  val $  : string -> token list -> string * token list
  val empty : 'a -> 'b list * 'a
  val id : token list -> string * token list
  val infixes :
      (token list -> 'a * token list) * (string -> int) *
      (string -> 'a -> 'a -> 'a) -> token list -> 'a * token list
  val repeat : ('a -> 'b * 'a) -> 'a -> 'b list * 'a
  end;


functor ParseFUN (Lex: LEXICAL) : PARSE =
  struct
  type token = Lex.token;
  exception SynError of string;

  (*Phrase consisting of the keyword 'a' *)
  fun $a (Lex.Key b :: toks) =
        if a=b then (a,toks) else raise SynError a
    | $a _ = raise SynError "Symbol expected";

  (*Phrase consisting of an identifier*)
  fun id (Lex.Id a :: toks) = (a,toks)
    | id toks = raise SynError "Identifier expected";

  (*Application of f to the result of a phrase*)
  fun (ph>>f) toks = 
      let val (x,toks2) = ph toks
      in  (f x, toks2)  end;

  (*Alternative phrases*)
  fun (ph1 || ph2) toks = ph1 toks   handle SynError _ => ph2 toks;

  (*Consecutive phrases*)
  fun (ph1 -- ph2) toks = 
      let val (x,toks2) = ph1 toks
	  val (y,toks3) = ph2 toks2
      in  ((x,y), toks3)  end;

  fun empty toks = ([],toks);

  (*Zero or more phrases*)
  fun repeat ph toks = (   ph -- repeat ph >> (op::)
                        || empty   ) toks;

  fun infixes (ph,prec_of,apply) = 
    let fun over k toks = next k (ph toks)
        and next k (x, Lex.Key(a)::toks) = 
              if prec_of a < k then (x, Lex.Key a :: toks)
              else next k ((over (prec_of a) >> apply a x) toks)
          | next k (x, toks) = (x, toks)
    in  over 0  end;

  fun reader ph a =   (*Scan and parse, checking that no tokens remain*)
	 (case ph (Lex.scan a) of 
	      (x, []) => x
	    | (_, _::_) => raise SynError "Extra characters in phrase");

  end;


(*** Pretty printing.  See Oppen (1980).  From Chapter 8.  ***)

signature PRETTY = 
  sig
   type T
   val blo : int * T list -> T
   val str : string -> T
   val brk : int -> T
   val pr  : outstream * T * int -> unit
   end;


functor PrettyFUN () : PRETTY =
  struct
  datatype T = 
      Block of T list * int * int 	(*indentation, length*)
    | String of string
    | Break of int;			(*length*)

  (*Add the lengths of the expressions until the next Break; if no Break then
    include "after", to account for text following this block. *)
  fun breakdist (Block(_,_,len)::sexps, after) = len + breakdist(sexps, after)
    | breakdist (String s :: sexps, after) = size s + breakdist (sexps, after)
    | breakdist (Break _ :: sexps, after) = 0
    | breakdist ([], after) = after;

  fun pr (os, sexp, margin) =
   let val space = ref margin

       fun blanks 0 = ()
         | blanks n = (output(os," ");  space := !space - 1; 
                       blanks(n-1))

       fun newline () = (output(os,"\n");  space := margin)

       fun printing ([], _, _) = ()
	 | printing (sexp::sexps, blockspace, after) =
	  (case sexp of
	       Block(bsexps,indent,len) =>
		  printing(bsexps, !space-indent, breakdist(sexps,after))
	     | String s => (output(os,s);   space := !space - size s)
	     | Break len => 
		 if len + breakdist(sexps,after) <= !space 
		 then blanks len
		 else (newline();  blanks(margin-blockspace));
	    printing (sexps, blockspace, after))
   in  printing([sexp], margin, 0);  newline()  end;

  fun length (Block(_,_,len)) = len
    | length (String s) = size s
    | length (Break len) = len;

  val str = String  and  brk = Break;

  fun blo (indent,sexps) =
    let fun sum([], k) = k
	  | sum(sexp::sexps, k) = sum(sexps, length sexp + k)
    in  Block(sexps,indent, sum(sexps,0))  end;
  end;


(*** Types as an example of parsing ***)

signature TYPE = 
  sig
  datatype typ = Con of string * typ list | Var of string
  val pr : typ -> unit
  val read : string -> typ
  end;

functor TypeFUN (structure Parse: PARSE 
		 and       Pretty: PRETTY) : TYPE =
  struct
  datatype typ = Con of string * typ list
	       | Var of string;

  local (** Parsing **)
    fun makefun ((S,_),T) = Con("->",[S,T]);
    open Parse 

    fun typ toks =
     (   atom -- $"->" -- typ			>> makefun
      || atom	
       ) toks
    and atom toks =
      (   $"'" -- id				>> (Var o op^)
       || $"(" -- typ -- $")"			>> (#2 o #1)
       ) toks;
  in
    val read = reader typ;
  end;

  local (** Displaying **)
    open Pretty

    fun typ (Var a) = str a
      | typ (Con("->",[S,T])) = blo(0, [atom S, str " ->", brk 1, typ T])
     and atom (Var a) = str a
       | atom T = blo(1, [str"(", typ T, str")"]);
  in
    fun pr T = Pretty.pr (std_out, typ T, 50)
  end
end;


