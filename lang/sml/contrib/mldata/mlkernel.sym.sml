(* Mllama Version 4.27 of Sept 3 1990
Compiled by: Standard ML of New Jersey, Version 0.56, 13 April 1990 *)

(* Symbol Types
I:	ID
FORMAT:	ID
STRING:	ID
NUM:	ID
file:	UNIT
note:	LIT
ndec:	D
nspec:	SPEC
typen:	T
paras:	UNIT
decseq:	DS
decls:	D
decs:	DS
dec:	D
directive:	DIR
strdecs:	STRDECS
funcdecs:	FUNCDECS
sigdecs:	SIGDECS
datadecs:	DYS
typedecs:	TYS
fundecs:	FUNS
valdecs:	EQNS
exndecs:	EXNDECS
strdec:	STRDEC
funcdec:	FUNCDEC
sigopt:	SIGOPT
sigdec:	SIGDEC
funpar:	FUNPAR
signature:	SIG
specs:	SPECS
spec:	SPEC
valspecs:	VSS
exnspecs:	CASES
typespecs:	TS
structspecs:	STRSPECS
sharespecs:	SSS
valspec:	VS
structspec:	STRSPEC
sharespec:	SS
cideqns:	CIDS
structure:	STR
datadec:	DATA
cases:	CASES
case:	CASE1
typedec:	TY
type:	T
product:	TS
simpletype:	T
typelist:	TS
fieldlist:	FS
field:	FIELD
selector:	ID
format:	A
exndec:	EXNDEC
fundec:	EQNS
equation:	EQN
funequation:	EQN
heading:	E
pattern:	E
infixpat:	E
infpat:	ES
atpat:	E
matches:	MATCHS
match:	MATCH
expr:	E
bexpr:	E
infexpr:	ES
atexpr:	E
receq:	BIND
recpat:	BIND
recpats:	BINDS
receqs:	BINDS
exprlist:	ES
patlist:	ES
exprseq:	ES
cidseq:	CIDS
id:	E
simpleid:	E
cidb:	CID
IB:	ID
cid:	CID
builtinid:	E
con:	E
opseq:	ES
nopseq:	ES
op:	E
strings:	ES
SCCSID:	ES
*)
signature SYMBOLS=
sig
val NUMBEROFTERMINALS : int
val SYMBOL_NAMES : string array
val IMPORT : int
val ABSTYPE : int
val WITHTYPE : int
val WITH : int
val FUN : int
val REC : int
val FUNCTOR : int
val SIGNATURE : int
val OPEN : int
val NONFIX : int
val INFIX : int
val INCLUDE : int
val INFIXR : int
val SIG : int
val VAL : int
val EQTYPE : int
val STRUCTURE : int
val DATATYPE : int
val EXCEPTION : int
val SHARING : int
val LOCAL : int
val AND : int
val TYPE : int
val STRUCT : int
val FORMAT : int
val HIGH : int
val AS : int
val BAR : int
val IMP : int
val COLON : int
val HANDLE : int
val ANDALSO : int
val ORELSE : int
val IF : int
val THEN : int
val ELSE : int
val WHILE : int
val DO : int
val CASE : int
val OF : int
val FN : int
val RAISE : int
val LET : int
val IN : int
val END : int
val BRA : int
val KET : int
val LBRA : int
val LKET : int
val RECBRA : int
val RECKET : int
val COMMA : int
val SEMI : int
val OP : int
val DOT : int
val ARROW : int
val PROD : int
val EQ : int
val NUM : int
val I : int
val STRING : int
val llEND : int
val NOTE : int
end;
structure SYMBOLS=
struct
val IMPORT=0;
val ABSTYPE=1;
val WITHTYPE=2;
val WITH=3;
val FUN=4;
val REC=5;
val FUNCTOR=6;
val SIGNATURE=7;
val OPEN=8;
val NONFIX=9;
val INFIX=10;
val INCLUDE=11;
val INFIXR=12;
val SIG=13;
val VAL=14;
val EQTYPE=15;
val STRUCTURE=16;
val DATATYPE=17;
val EXCEPTION=18;
val SHARING=19;
val LOCAL=20;
val AND=21;
val TYPE=22;
val STRUCT=23;
val FORMAT=24;
val HIGH=25;
val AS=26;
val BAR=27;
val IMP=28;
val COLON=29;
val HANDLE=30;
val ANDALSO=31;
val ORELSE=32;
val IF=33;
val THEN=34;
val ELSE=35;
val WHILE=36;
val DO=37;
val CASE=38;
val OF=39;
val FN=40;
val RAISE=41;
val LET=42;
val IN=43;
val END=44;
val BRA=45;
val KET=46;
val LBRA=47;
val LKET=48;
val RECBRA=49;
val RECKET=50;
val COMMA=51;
val SEMI=52;
val OP=53;
val DOT=54;
val ARROW=55;
val PROD=56;
val EQ=57;
val NUM=58;
val I=59;
val STRING=60;
val llEND=61;
val NOTE=62;
val SYMBOL_NAMES =
 arrayoflist["IMPORT",
  "ABSTYPE",
  "WITHTYPE",
  "WITH",
  "FUN",
  "REC",
  "FUNCTOR",
  "SIGNATURE",
  "OPEN",
  "NONFIX",
  "INFIX",
  "INCLUDE",
  "INFIXR",
  "SIG",
  "VAL",
  "EQTYPE",
  "STRUCTURE",
  "DATATYPE",
  "EXCEPTION",
  "SHARING",
  "LOCAL",
  "AND",
  "TYPE",
  "STRUCT",
  "FORMAT",
  "HIGH",
  "AS",
  "BAR",
  "IMP",
  "COLON",
  "HANDLE",
  "ANDALSO",
  "ORELSE",
  "IF",
  "THEN",
  "ELSE",
  "WHILE",
  "DO",
  "CASE",
  "OF",
  "FN",
  "RAISE",
  "LET",
  "IN",
  "END",
  "BRA",
  "KET",
  "LBRA",
  "LKET",
  "RECBRA",
  "RECKET",
  "COMMA",
  "SEMI",
  "OP",
  "DOT",
  "ARROW",
  "PROD",
  "EQ",
  "NUM",
  "I",
  "STRING",
  "llEND",
  "NOTE",
  "llaccept",
  "llerror",
  "file",
  "paras",
  "note",
  "ndec",
  "dec",
  "nspec",
  "spec",
  "typen",
  "type",
  "decseq",
  "expr",
  "strings",
  "decls",
  "decs",
  "datadecs",
  "typedecs",
  "fundecs",
  "valdecs",
  "exndecs",
  "strdecs",
  "funcdecs",
  "sigdecs",
  "directive",
  "cidseq",
  "opseq",
  "nopseq",
  "strdec",
  "funcdec",
  "sigdec",
  "datadec",
  "typedec",
  "fundec",
  "equation",
  "exndec",
  "sigopt",
  "structure",
  "funpar",
  "signature",
  "specs",
  "cid",
  "valspecs",
  "typespecs",
  "structspecs",
  "exnspecs",
  "sharespecs",
  "valspec",
  "case",
  "simpletype",
  "structspec",
  "sharespec",
  "id",
  "cideqns",
  "cases",
  "IB",
  "format",
  "product",
  "typelist",
  "fieldlist",
  "field",
  "selector",
  "funequation",
  "pattern",
  "heading",
  "infixpat",
  "infpat",
  "atpat",
  "con",
  "patlist",
  "recpats",
  "matches",
  "match",
  "infexpr",
  "bexpr",
  "atexpr",
  "simpleid",
  "cidb",
  "exprseq",
  "exprlist",
  "receqs",
  "receq",
  "recpat",
  "builtinid",
  "op",
  "SCCSID" ];
val NUMBEROFTERMINALS = 62
end;
