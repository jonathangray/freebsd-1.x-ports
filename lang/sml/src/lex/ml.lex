(* Copyright 1989 by AT&T Bell Laboratories *)

open ErrorMsg;

structure TokTable = TokenTable(Tokens);
type svalue = Tokens.svalue
type pos = int
type lexresult = (svalue,pos) Tokens.token
type lexarg = {comLevel : int ref, 
	       lineNum : int ref,
               linePos : int list ref, (* offsets of lines in file *)
	       charlist : string list ref,
	       stringstart : int ref, (* start of current string or comment*)
               brack_stack : int ref list ref, (* for frags *)
	       err : pos*pos -> ErrorMsg.complainer}
type arg = lexarg
type ('a,'b) token = ('a,'b) Tokens.token
val eof = fn ({comLevel,err,linePos,stringstart,
               lineNum,charlist, brack_stack}:lexarg) => 
	   let val pos = Integer.max(!stringstart+2, hd(!linePos))
	    in if !comLevel>0 then err (!stringstart,pos) COMPLAIN
					 "unclosed comment" nullErrorBody
		  	      else ();
	       Tokens.EOF(pos,pos)
	   end	
fun addString (charlist,s:string) = charlist := s :: (!charlist)
fun makeString charlist = (implode(rev(!charlist)) before charlist := nil)
fun makeHexInt sign s = let
      fun digit d = if (d < Ascii.uc_a) then (d - Ascii.zero)
	    else (10 + (if (d < Ascii.lc_a) then (d - Ascii.uc_a) else (d - Ascii.lc_a)))
      in
	revfold (fn (c,a) => sign(a*16, digit(ord c))) (explode s) 0
      end
fun makeInt sign s =
    revfold (fn (c,a) => sign(a*10, ord c - Ascii.zero)) (explode s) 0

local
val quote = ord "`"
in
fun has_quote s =
   let fun loop i = (ordof(s,i) = quote orelse loop (i+1))
                    handle Ord => false
   in
   loop 0
   end
end;
   
%% 
%reject
%s A S F Q AQ;
%header (functor MLLexFun(structure Tokens : ML_TOKENS));
%arg ({comLevel,lineNum,err,linePos,charlist,stringstart,brack_stack});
idchars=[A-Za-z'_0-9];
id=[A-Za-z]{idchars}*;
ws=("\012"|[\t\ ])*;
full_sym=[!%&$+/:<=>?@~|#*`]|\\|\-|\^;
sym=[!%&$+/:<=>?@~|#*]|\\|\-|\^;
quote="`";
num=[0-9]+;
frac="."{num};
exp="E"(~?){num};
real=(~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
hexnum=[0-9a-fA-F]+;
%%
<INITIAL>{ws}	=> (continue());
<INITIAL>\n	=> (inc lineNum; linePos := yypos :: !linePos; continue());
<INITIAL>"_"	=> (Tokens.WILD(yypos,yypos+1));
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>"{"	=> (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"	=> (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"["	=> (Tokens.LBRACKET(yypos,yypos+1));
<INITIAL>"#["	=> (Tokens.VECTORSTART(yypos,yypos+1));
<INITIAL>"]"	=> (Tokens.RBRACKET(yypos,yypos+1));
<INITIAL>";"	=> (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("	=> (if (null(!brack_stack))
                    then ()
                    else inc (hd (!brack_stack));
                    Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"	=> (if (null(!brack_stack))
                    then ()
                    else if (!(hd (!brack_stack)) = 1)
                         then ( brack_stack := tl (!brack_stack);
                                charlist := [];
                                YYBEGIN Q)
                         else dec (hd (!brack_stack));
                    Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"."		=> (Tokens.DOT(yypos,yypos+1));
<INITIAL>"..."		=> (Tokens.DOTDOTDOT(yypos,yypos+3));
<INITIAL>"'"("'"?)("_"|{num})?{id}
			=> (TokTable.checkTyvar(yytext,yypos));
<INITIAL>{id}	        => (TokTable.checkToken(yytext,yypos));
<INITIAL>{full_sym}+    => (if (!System.Control.quotation)
                            then if (has_quote yytext)
                                 then REJECT()
                                 else TokTable.checkToken(yytext,yypos)
                            else TokTable.checkToken(yytext,yypos));
<INITIAL>{sym}+         => (TokTable.checkToken(yytext,yypos));
<INITIAL>{quote}        => (if (!System.Control.quotation)
                            then (YYBEGIN Q;
                                  charlist := [];
                                  Tokens.BEGINQ(yypos,yypos+1))
                            else (err(yypos, yypos+1)
                                     COMPLAIN "quotation implementation error"
				     nullErrorBody;
                                  Tokens.BEGINQ(yypos,yypos+1)));
<INITIAL>{real}	=> (Tokens.REAL(yytext,yypos,yypos+size yytext));
<INITIAL>[1-9][0-9]* => (Tokens.INT(makeInt (op +) yytext
		    handle Overflow => (err (yypos,yypos+size yytext)
					  COMPLAIN "integer too large"
					  nullErrorBody;
				        1),
			yypos,yypos+size yytext));
<INITIAL>{num}	=> (Tokens.INT0(makeInt (op +) yytext
		    handle Overflow => (err (yypos,yypos+size yytext)
					  COMPLAIN "integer too large"
					  nullErrorBody; 0),
			yypos,yypos+size yytext));
<INITIAL>~{num}	=> (Tokens.INT0(makeInt (op -)
					(substring(yytext,1,size(yytext)-1))
		    handle Overflow => (err (yypos,yypos+size yytext)
					 COMPLAIN "integer too large"
					 nullErrorBody;
				        0),
			yypos,yypos+size yytext));
<INITIAL>"0x"{hexnum} => (
		    Tokens.INT0(makeHexInt (op +) (substring(yytext, 2, size(yytext)-2))
		        handle Overflow => (err (yypos,yypos+size yytext)
					      COMPLAIN "integer too large"
					      nullErrorBody;
					    0),
		      yypos, yypos+size yytext));
<INITIAL>"~0x"{hexnum} => (
		    Tokens.INT0(makeHexInt (op -) (substring(yytext, 3, size(yytext)-3))
		        handle Overflow => (err (yypos,yypos+size yytext)
					      COMPLAIN "integer too large"
					      nullErrorBody;
					    0),
		      yypos, yypos+size yytext));
<INITIAL>\"	=> (charlist := [""]; stringstart := yypos;
			YYBEGIN S; continue());
<INITIAL>"(*"	=> (YYBEGIN A; stringstart := yypos; comLevel := 1; continue());
<INITIAL>"*)"	=> (err (yypos,yypos+1) COMPLAIN "unmatched close comment"
		        nullErrorBody;
		    continue());
<INITIAL>\h	=> (err (yypos,yypos) COMPLAIN "non-Ascii character"
		        nullErrorBody;
		    continue());
<INITIAL>.	=> (err (yypos,yypos) COMPLAIN "illegal token" nullErrorBody;
		    continue());
<A>"(*"		=> (inc comLevel; continue());
<A>\n		=> (inc lineNum; linePos := yypos :: !linePos; continue());
<A>"*)" => (dec comLevel; if !comLevel=0 then YYBEGIN INITIAL else (); continue());
<A>.		=> (continue());
<S>\"	        => (YYBEGIN INITIAL; Tokens.STRING(makeString charlist,
				!stringstart,yypos+1));
<S>\n		=> (err (!stringstart,yypos) COMPLAIN "unclosed string"
		        nullErrorBody;
		    inc lineNum; linePos := yypos :: !linePos;
		    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos));
<S>[^"\\\n]*	=> (addString(charlist,yytext); continue());
<S>\\\n	       	=> (inc lineNum; linePos := yypos :: !linePos;
		    YYBEGIN F; continue());
<S>\\[\ \t]   	=> (YYBEGIN F; continue());
<F>\n		=> (inc lineNum; linePos := yypos :: !linePos; continue());
<F>{ws}		=> (continue());
<F>\\		=> (YYBEGIN S; stringstart := yypos; continue());
<F>.		=> (err (!stringstart,yypos) COMPLAIN "unclosed string"
		        nullErrorBody; 
		    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos+1));
<S>\\t		=> (addString(charlist,"\t"); continue());
<S>\\n		=> (addString(charlist,"\n"); continue());
<S>\\\\		=> (addString(charlist,"\\"); continue());
<S>\\\"		=> (addString(charlist,chr(Ascii.dquote)); continue());
<S>\\\^[@-_]	=> (addString(charlist,chr(ordof(yytext,2)-ord("@"))); continue());
<S>\\[0-9]{3}	=>
 (let val x = ordof(yytext,1)*100
	     +ordof(yytext,2)*10
	     +ordof(yytext,3)
	     -(Ascii.zero*111)
  in (if x>255
      then err (yypos,yypos+4) COMPLAIN "illegal ascii escape" nullErrorBody
      else addString(charlist,chr x);
      continue())
  end);
<S>\\		=> (err (yypos,yypos+1) COMPLAIN "illegal string escape"
		        nullErrorBody; 
		    continue());


<Q>"^"          => (YYBEGIN AQ;
                    let val x = makeString charlist
                    in
                    Tokens.OBJL(x,yypos,yypos+(size x))
                    end);
<Q>"`"          => ((* a closing quote *)
                    YYBEGIN INITIAL;
                    let val x = makeString charlist
                    in
                    Tokens.ENDQ(x,yypos,yypos+(size x))
                    end);
<Q>\n           => (inc lineNum; addString(charlist,"\n"); continue());
<Q>.            => (addString(charlist,yytext); continue());

<AQ>\n          => (inc lineNum; continue());
<AQ>{ws}        => (continue());
<AQ>{id}        => (YYBEGIN Q; 
                    let val hash = StrgHash.hashString yytext
                    in
                    Tokens.AQID(FastSymbol.rawSymbol(hash,yytext),
				yypos,yypos+(size yytext))
                    end);
<AQ>{sym}+      => (YYBEGIN Q; 
                    let val hash = StrgHash.hashString yytext
                    in
                    Tokens.AQID(FastSymbol.rawSymbol(hash,yytext),
				yypos,yypos+(size yytext))
                    end);
<AQ>"("         => (YYBEGIN INITIAL;
                    brack_stack := ((ref 1)::(!brack_stack));
                    Tokens.LPAREN(yypos,yypos+1));
<AQ>.           => (err (yypos,yypos+1) COMPLAIN
		       ("ml lexer: bad character after antiquote "^yytext)
		       nullErrorBody;
                    Tokens.AQID(FastSymbol.rawSymbol(0,""),yypos,yypos));
