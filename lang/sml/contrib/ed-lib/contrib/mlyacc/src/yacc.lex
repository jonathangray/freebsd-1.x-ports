(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi

   yacc.lex: Lexer specification
 *)

structure Tokens = Tokens
type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

type lexarg = Hdr.inputSource
type arg = lexarg

open Tokens
val error = Hdr.error
val lineno = Hdr.lineno
val text = Hdr.text

val pcount = ref 0
val commentLevel = ref 0
val actionstart = ref 0

val eof = fn i => (if (!pcount)>0 then
			error i (!actionstart)
			      " eof encountered in action beginning here !"
		   else (); EOF(!lineno,!lineno))

val Add = fn s => (text := s::(!text))

val lookup =
   let val dict = [("%prec",PREC_TAG),("%term",TERM),
		  ("%nonterm",NONTERM), ("%eop",PERCENT_EOP),("%start",START),
		  ("%prefer",PREFER),("%subst",SUBST),
		  ("%keyword",KEYWORD),("%name",NAME),
		  ("%verbose",VERBOSE), ("%nodefault",NODEFAULT),
		  ("%value",VALUE), ("%noshift",NOSHIFT),
		  ("%header",PERCENT_HEADER),("%pure",PERCENT_PURE),
		  ("%arg",PERCENT_ARG),
		  ("%pos",PERCENT_POS)]
   in fn (s,left,right) =>
	    let fun f ((a,d)::b) = if a=s then d(left,right) else f b
	 	  | f nil = UNKNOWN(s,left,right)
	    in f dict
    	    end
   end
%%
%header (functor LexMLYACC(structure Tokens : Mlyacc_TOKENS
			   structure Hdr : HEADER
	                   sharing Hdr = Header) : ARG_LEXER);
%arg (inputSource);
%s A CODE F COMMENT STRING EMPTYCOMMENT;
ws = [\t\ ]+;
idchars = [A-Za-z_'0-9];
id=[A-Za-z]{idchars}*;
tyvar="'"{idchars}*;
qualid ={id}".";
%%
<INITIAL>"(*"	=> (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
		    continue() before YYBEGIN INITIAL);
<A>"(*"		=> (YYBEGIN EMPTYCOMMENT; commentLevel := 1; continue());
<CODE>"(*"	=> (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
		    continue() before YYBEGIN CODE);
<INITIAL>[^%\n]+ => (Add yytext; continue());
<INITIAL>"%%"	 => (YYBEGIN A; HEADER (implode (rev (!text)),!lineno,!lineno));
<INITIAL,CODE,COMMENT,F,EMPTYCOMMENT>\n  => (Add yytext; Ref.inc lineno; continue());
<INITIAL>.	 => (Add yytext; continue());

<A>\n		=> (Ref.inc lineno; continue ());
<A>{ws}+	=> (continue());
<A>of		=> (OF(!lineno,!lineno));
<A>for		=> (FOR(!lineno,!lineno));
<A>"="		=> (EQUAL(!lineno,!lineno));
<A>"{"		=> (LBRACE(!lineno,!lineno));
<A>"}"		=> (RBRACE(!lineno,!lineno));
<A>","		=> (COMMA(!lineno,!lineno));
<A>"*"		=> (ASTERISK(!lineno,!lineno));
<A>"->"		=> (ARROW(!lineno,!lineno));
<A>"%left"	=> (PREC(Hdr.LEFT,!lineno,!lineno));
<A>"%right"	=> (PREC(Hdr.RIGHT,!lineno,!lineno));
<A>"%nonassoc" 	=> (PREC(Hdr.NONASSOC,!lineno,!lineno));
<A>"%"[a-z_]+	=> (lookup(yytext,!lineno,!lineno));
<A>{tyvar}	=> (TYVAR(yytext,!lineno,!lineno));
<A>{qualid}	=> (IDDOT(yytext,!lineno,!lineno));
<A>[0-9]+	=> (INT (yytext,!lineno,!lineno));
<A>"%%"		=> (DELIMITER(!lineno,!lineno));
<A>":"		=> (COLON(!lineno,!lineno));
<A>"|"		=> (BAR(!lineno,!lineno));
<A>{id}		=> (ID ((yytext,!lineno),!lineno,!lineno));
<A>"("		=> (pcount := 1; actionstart := (!lineno);
		    text := nil; YYBEGIN CODE; continue() before YYBEGIN A);
<A>.		=> (UNKNOWN(yytext,!lineno,!lineno));
<CODE>"("	=> (Ref.inc pcount; Add yytext; continue());
<CODE>")"	=> (Ref.dec pcount;
		    if !pcount = 0 then
			 PROG (implode (rev (!text)),!lineno,!lineno)
		    else (Add yytext; continue()));
<CODE>"\""	=> (Add yytext; YYBEGIN STRING; continue());
<CODE>[^()"\n]+	=> (Add yytext; continue());

<COMMENT>[(*)]	=> (Add yytext; continue());
<COMMENT>"*)"	=> (Add yytext; Ref.dec commentLevel;
		    if !commentLevel=0
			 then BOGUS_VALUE(!lineno,!lineno)
			 else continue()
		   );
<COMMENT>"(*"	=> (Add yytext; Ref.inc commentLevel; continue());
<COMMENT>[^*()\n]+ => (Add yytext; continue());

<EMPTYCOMMENT>[(*)]  => (continue());
<EMPTYCOMMENT>"*)"   => (Ref.dec commentLevel;
		          if !commentLevel=0 then YYBEGIN A else ();
			  continue ());
<EMPTYCOMMENT>"(*"   => (Ref.inc commentLevel; continue());
<EMPTYCOMMENT>[^*()\n]+ => (continue());

<STRING>"\""	=> (Add yytext; YYBEGIN CODE; continue());
<STRING>\\	=> (Add yytext; continue());
<STRING>\n	=> (Add yytext; error inputSource (!lineno) "unclosed string";
 	            Ref.inc lineno; YYBEGIN CODE; continue());
<STRING>[^"\\\n]+ => (Add yytext; continue());
<STRING>\\\"	=> (Add yytext; continue());
<STRING>\\[\ \t\n]   => (Add yytext;
			if String.extract 1 2 yytext="\n" then Ref.inc lineno else ();
		     	YYBEGIN F; continue());

<F>{ws}		=> (Add yytext; continue());
<F>\\		=> (Add yytext; YYBEGIN STRING; continue());
<F>.		=> (Add yytext; error inputSource (!lineno) "unclosed string";
		    YYBEGIN CODE; continue());
%%
