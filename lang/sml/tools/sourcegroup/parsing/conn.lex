structure Tokens = Tokens
type pos = Interface.pos
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

fun line () = !Interface.line
fun eof () = Tokens.EOF (line(),line())
fun error e = output(std_out,
      "% Connections: (line "^(Interface.makeString(line()))^") "^e^"\n")
val charlist = ref ([""] :string list)
val numOffset = (ord "0")*111
val ascii'at = ord "@";
val quoteChar = "\"";
fun nullString () = charlist := [""];
fun addString (s:string) = charlist := s :: (!charlist);
fun makeString () = (implode(rev(!charlist)) before charlist := nil);
val comLevel = ref 0
%%
%s C S F;
%header (functor ConnLexFun
  (structure Tokens: Conn_TOKENS
   structure Interface :INTERFACE));
alpha=[A-Za-z];
ws = [\ \t];
idchars=[A-Za-z'_0-9];
id=[A-Za-z'_]{idchars}*;
%%
<INITIAL>\n          => (Interface.nextLine(); lex());
<INITIAL>{ws}+       => (lex());
<INITIAL>";"         => (Tokens.SEMI(line(),line()));
<INITIAL>"import"    => (Tokens.IMPORT(line(),line()));
<INITIAL>"export"    => (Tokens.EXPORT(line(),line()));
<INITIAL>"source"    => (Tokens.SOURCE(line(),line()));
<INITIAL>"namespace" => (Tokens.NAMESPACE(line(),line()));
<INITIAL>{id}        => (Tokens.ID(yytext,line(),line()));
<INITIAL>\"	     => (nullString(); YYBEGIN S; lex());
<INITIAL>"(*"	     => (YYBEGIN C; comLevel := 1; continue());
<INITIAL>.           => (error ("ignoring bad character "^yytext); lex());

<S>\"         => (YYBEGIN INITIAL;
		  Tokens.STRING (makeString(),line(),line()));
<S>\n         => (error "unclosed string"; Interface.nextLine();
		  YYBEGIN INITIAL; Tokens.STRING (makeString(),line(),line()));
<S>[^"\\\n]*  => (addString(yytext); lex());
<S>\\\n       => (Interface.nextLine(); YYBEGIN F; lex());
<S>\\[\ \t]   => (YYBEGIN F; lex());
<S>\\t        => (addString("\t"); lex());
<S>\\n	      => (addString("\n"); lex());
<S>\\\\	      => (addString("\\"); lex());
<S>\\\"       => (addString(quoteChar); lex());
<S>\\\^[@-_]  => (addString(chr(ordof(yytext,2)-ascii'at)); lex());
<S>\\[0-9]{3} =>
 (let val x = ordof(yytext,1)*100+ordof(yytext,2)*10+ordof(yytext,3)-numOffset
  in
    if x>255
      then error "illegal ascii escape"
      else addString(chr x);
    lex()
  end);
<S>\\         => (error "illegal string escape"; lex());

<F>\n   => (Interface.nextLine(); lex());
<F>{ws} => (lex());
<F>\\   => (YYBEGIN S; lex());
<F>.    => (error "unclosed string"; YYBEGIN INITIAL;
	    Tokens.STRING (makeString(),line(),line()));

<C>"(*" => (inc comLevel; continue());
<C>\n   => (Interface.nextLine(); continue());
<C>"*)" => (dec comLevel;
	    if !comLevel=0 then YYBEGIN INITIAL else (); continue());
<C>.    => (continue());
