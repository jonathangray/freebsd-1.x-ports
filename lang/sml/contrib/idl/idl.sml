(*
	Copyright (C) 1992
	Oxford University Computing Laboratory
	@(#)idl.sml	2.1 93/03/08 00:03:17
*)

val Title   = "IDL: ";
val Version = "2.1 93/03/08 00:03:17";

(*
	-------------------
	 History
	-------------------

	0.3     11 Mar 92       Fixed small bug in result generation
	0.4     04 Apr 92       Added SML inserts
				Puts #line directives in C file for
				easier source diagnosis
				Improved idl syntax error detection
				and reporting
	0.4a    07 Apr 92       C file is slightly prettier
				and declares address and bool
	0.5     28 Apr 92       C no longer depends on left-right
				ordering of parameter evaluation
				in dispatcher.
	0.6     03 Aug 92       Debugging output from C side

	September '92           SCCS -- EXPORTABLE VARIANT (see SCCS log)

	--------------------
	 Future
	--------------------

	I really should eliminate the Seq constructor, and just
	read a list of paragraphs.
*)

use     "lib/Import.sml"; structure Import=Import(); open Import;
Import  "lib/list.sig";
Import  "lib/list"; structure list=list(); open list;
Import  "lib/streamio.sig";
Import  "lib/streamio"; structure streamio=streamio(); open streamio;

type            Code = string
and             Name = string

datatype        Idl  =   Code of Code
		     |   Sign of Sign
		     |   Defn of Sign * Code * Code list
		     |   Line of int
		     |   Seq  of Idl * Idl
		     |   Sml  of Code
		     |   Val  of Name * Code    (* Used internally *)
		     |   TypeRep of Name * Name list * Name
		     |   TypeDef of Name * Name list * Name

withtype        Sign =   Name * (Name*Name) list * Name list

datatype        Symbol = NAME   of Name
		       | CODE   of Code
		       | SML    of Code
		       | NUMBER of real
		       | SEMI
		       | COLON
		       | EQ
		       | BRA
		       | KET
		       | ARROW
		       | COMMA
		       | IMP
		       | STAR
		       | ENDSTREAM

val lexstream  = ref std_in;
val linenumber = ref 1;
val charnumber = ref 1;
val linestart  = ref 1;
val charstart  = ref 1;
val debug      = ref false;


fun char()     = lookahead ( !lexstream );
fun readchar() =
let val c = inputc ( !lexstream) 1 in
   inc charnumber;
   if c="\n" then (inc linenumber; charnumber := 1) else ();
   c
end;

datatype Class= Ignorable |  Letter |  Digit |  Punct  ;

fun Scan()    =
let val ch = readchar ()
in  linestart := !linenumber;
   charstart := !charnumber;
   case Class ch of
      Ignorable => Scan ()
   |  Letter    => ScanVar ch
   |  Digit     => ScanNum ch
   |  Punct =>
      case ch of
	 ";" => SEMI
      |  "=" => (case char() of ">" => (readchar(); IMP) | _ => EQ)
      |  ":" => COLON
      |  "(" => (case char() of "*" => SMLcomment() | _ => BRA)
      |  "/" => (case char() of "*" => Ccomment() | _ => LexError())
      |  ")" => KET
      |  "," => COMMA
      |  "*" => STAR
      |  "%" => (case readchar() of "{" => ScanCode() | _ => LexError())
      |  "$" => (case readchar() of "{" => ScanSml() | _ => LexError())
      |  ""  => (close_in ( !lexstream); ENDSTREAM)
      |  _   => LexError ()
end

and SMLcomment() =
let fun C() =
    case readchar() of
	 ""  => ENDSTREAM
    |    "*" => (case char() of ")" => (readchar(); Scan()) | _ => C())
    |    _   => C();
in
   readchar(); C()
end

and Ccomment() =
let fun C() =
    case readchar() of
	 ""  => ENDSTREAM
    |    "*" => (case char() of "/" => (readchar(); Scan()) | _ => C())
    |    _   => C();
in
   readchar(); C()
end

and Class " "  = Ignorable
|   Class "\t" = Ignorable
|   Class "\n" = Ignorable
|   Class ch =
    if     "a" <= ch andalso ch <= "z"
    orelse "A" <= ch andalso ch <= "Z"
    orelse "_" = ch
    then
      Letter
    else if "0" <= ch andalso ch <= "9" then Digit else Punct

and ScanCode() =
let fun RCode r =
    case readchar() of
	 ""  => rev r
    |    "%" => (case readchar() of "}" => rev r | c => RCode (c:: "%" ::r))
    |    c   => RCode (c::r)
in
    CODE(implode(RCode []))
end

and ScanSml() =
let fun RCode r =
    case readchar() of
	 ""  => rev r
    |    "$" => (case readchar() of "}" => rev r | c => RCode (c:: "$" ::r))
    |    c   => RCode (c::r)
in
   SML(implode(RCode []))
end

and ScanVar firstchar = NAME(implode (ScanWord [firstchar]))

and ScanWord chs =
case Class (char ()) of
   Punct     => rev chs
|  Ignorable => rev chs
|  _         => ScanWord (readchar () ::chs)

and ScanNum firstdigit =
let fun AtoR []      = 0.0
   |   AtoR (d::ds) = real (ord d-ord "0") +10.0*AtoR ds;
   fun Scale (r, 0) = r
   |   Scale (r, n) = Scale (r/10.0, n-1);
   val ds1 = ScanDigits [firstdigit];
   val ds2 = if char () = "." then (readchar (); ScanDigits []) else []
in
   NUMBER (AtoR ds1+Scale (AtoR ds2, length ds2))
end

and ScanDigits chs =
case Class (char ()) of
  Digit => ScanDigits (readchar () ::chs)
|  _     => chs

and LexError () =
(app (outputc std_err) ["Lexical scanner error: spurious character at ",
		       makestring(!linenumber),
		       ".",
		       makestring(!charnumber),
		       "\n"];
 Scan ()
);

val Symb      = ref ENDSTREAM;
fun Next() =
(   Symb:=Scan ()
)

fun SyntaxError why =
(app (outputc std_err) ["Syntax error at ",
		      makestring(!linestart),
		      ".",
		      makestring(!charstart),
		      ": ",
		      why, "\n"];
raise Interrupt
);

fun Check s =
if !Symb=s then Next () else SyntaxError ("'" ^Name s^ "' expected")
and Name s =
case s of
  ENDSTREAM => "end-stream"
|  BRA   => "("
|  KET   => ")"
|  EQ    => "="
|  COLON => ":"
;

fun Ignore s = if !Symb=s then Next () else ();

fun readexprs () =
let fun rs () = while char()=" " orelse char()="\t" orelse char()="\n" do readchar();
    val n = ref 0
    fun re cs r =
    case char() of
	 ";"  => (readchar(); rev (implode (rev cs)::r))
    |    "\n" => (readchar(); rev (implode (rev cs)::r))
    |    ","  => if   !n<=0
		 then (readchar(); rs(); re [] (implode (rev cs)::r))
		 else (readchar(); re (","::cs) r)
    |    ""   => (rev (implode (rev cs)::r))
    |    "="  => (readchar(); case readchar() of
		   ">" => (rs(); re [] (implode (rev cs)::r))
		 |  c  => (re (c::"="::cs) r)
		 )
    |    "("  => (inc n; readchar(); re ("("::cs) r)
    |    ")"  => (dec n; readchar(); re (")"::cs) r)
    |    c    => (readchar(); re (c::cs) r)
in
    rs(); re [] []
end;

fun default typename = case typename of [n] => n | _ => "address";

fun readpara() =
let infix Seq
    val p = ref(readprim())
in  while
    case !Symb of
	SEMI   => (Next(); p := ( !p Seq (readprim())); true)
    |   NAME _ => (        p := ( !p Seq (readprim())); true)
    |   CODE _ => (        p := ( !p Seq (readprim())); true)
    |   SML  _ => (        p := ( !p Seq (readprim())); true)
    |   ENDSTREAM => false
    |   _      => SyntaxError "procedure heading or C/SML code insert or data definition expected"
    do ()
    ;
    !p
end

and readprim() =
let val line = !linestart in
Seq(Line line,
case !Symb before Next() of
     CODE c => Code c
|    SML c  => Sml c
|    NAME "type" =>
     let val name = readname();
	 val _    = Check EQ;
	 val typename = readtype();
     in
	 case !Symb of (* constructor = ctype => idltype *)
		IMP => TypeRep(name, typename, (Next(); readname()))
	 |      _   => TypeRep(name, typename, default typename)
     end
|    NAME "typedef" =>
     let val name = readname();
	 val _    = Check EQ;
	 val typename = readtype();
     in
	 case !Symb of (* constructor = ctype => idltype *)
		IMP => TypeDef(name, typename, (Next(); readname()))
	 |      _   => TypeDef(name, typename, default typename)
     end
|    NAME n =>
     let val _  = Check BRA;
	 val ps = readparams();
	 val _  = Check KET;
	 val qs = case !Symb of COLON => readnames(Next()) | _ => []
     in
	 case !Symb of
	      EQ   => MkDefn((n, ps, qs), (Next(); readcode()), readresult())
	 |    IMP  => MkDefn((n, ps, qs), "", readresult())
	 |    SEMI => Sign(n, ps, qs)
	 |    ENDSTREAM => Sign(n, ps, qs)
	 |    _    => SyntaxError "=> or = or ; expected after procedure heading"
     end
|    _      => SyntaxError "definition or C code or SML code expected"
)
end

and MkDefn(a as (_, _, types), c, vals) =
    if   length types = length vals orelse vals=[] then Defn(a, c, vals)
    else SyntaxError "Result types and values inconsistent"

and readnames() =
case !Symb of
     NAME n => n :: (Next(); Ignore STAR; readnames())
|    _      => []

and readparams() =
case !Symb of
     NAME n => readparam() ::
	       (case !Symb of COMMA => (Next(); readparams()) | _ => [])
|    _      => []

and readparam() = (readname(), (Check COLON; readname()))

and readcode()  =
case !Symb of
     CODE c => (Next(); c)
|    _      => SyntaxError "C code expected"

and readresult() =
case !Symb of
     IMP => (readexprs() before Next())
|    _   => []

and readname()  =
case !Symb of
     NAME n => (Next(); n)
|    _      => SyntaxError "name expected"

and readtype()  =
case !Symb of
     NAME n => (Next(); n :: readstars())
|    _      => SyntaxError "C typename expected"

and readstars() =
case !Symb of
     STAR => (Next(); "*" :: readstars())
|       _ => [];

fun idlfile filename = idl (open_in filename)
and idls    string = idl(open_string string)
and idl s' =
let     val s  = !lexstream
in      lexstream  := s';
	linenumber := 1;
	charnumber := 1;
	Next();
	readpara () before (close_in s'; lexstream := s)
end


fun funs p =
case p of
    Sign (n, _, _)         => [n]
|   Defn ((n, _, _), _, _) => [n]
|   Seq  (p1, p2)          => funs p1 @ funs p2
|   _                      => [];

fun last xs  = hd(rev xs);
fun front xs = implode(rev(tl(rev xs)));

fun types p =
case p of
    Sign (_, outputs, inputs)         => inputs @ map #2 outputs
|   Defn ((_, outputs, inputs), _, _) => inputs @ map #2 outputs
|   TypeDef(_, ctypename, idltype)    => [idltype]
|   TypeRep(_, ctypename, idltype)    => [idltype]
|   Seq  (p1, p2)                     => types p1 @ types p2
|   _                                 => [];

fun typedefs p =
case p of
    TypeRep(name, _, _) => [name]
|   TypeDef(name, _, _ )=> [name]
|   Seq  (p1, p2)       => typedefs p1 @ typedefs p2
|   _                   => [];

fun typenv p =
case p of
    Sign (_, outputs, inputs)            => []
|   Defn ((_, outputs, inputs), _, _)    => []
|   TypeDef(name, ctypename, idltype)    => [] (* We don't need to expand the def'n *)
|   TypeRep(name, ctypename, idltype)    => [(name, implode ctypename)]
|   Seq  (p1, p2)                        => typenv p1 @ typenv p2
|   _                                    => [];

fun typeof n [] = n
|   typeof n ((n', t)::env) = if n=n' then t else typeof n env;

fun mkenv p = set(funs p);

exception which_;

fun which n m []      = raise which_
|   which n m (x::xs) = if n=x then m else which n (m+1) xs;

fun seek n xs = which n 1 xs;

fun Fun env (name, outputs, inputs) =
      "fun "  ^ name ^ "(" ^ params outputs ^ ")=\n"
    ^ "( "    ^ caller(seek name env)
    ^ "\n "   ^ writers outputs
    ^ "\n ("  ^ readers inputs ^")"
    ^ "\n)"

and params []      = ""
|   params [p]     = paramt p
|   params (p::ps) = paramt p ^ ", " ^ params ps

and writers []      = ""
|   writers [p]     = writer p
|   writers (p::ps) = writer p ^ "\n " ^ writers ps

and readers []      = ""
|   readers [t]     = reader t
|   readers (t::ts) = reader t ^ "," ^ readers ts

and paramt (n, t) = n ^ ":" ^ t
and writer (n, t) = "write_" ^ t ^ "(" ^ n ^");"
and reader (t)    = "read_"  ^ t ^ "()"
and caller (n:int) = "write_int " ^ makestring n ^ ";"

fun Signature p =
let fun Sig p =
    case p of
	    Sign s          => Sig' s
	|   Defn (s, _, _)  => Sig' s
	|   Seq(p1, p2)     => (Sig p1^Sig p2)
	|   _               => ""
    and Sig' (name, outputs, inputs) =
	name
      ^ ":"
      ^ pl (map #2 outputs)
      ^ "->"
      ^ pl inputs
      ^ ";"
    and pl []      = ""
    |   pl [x]     = x
    |   pl (x::xs) = x^","^pl xs
in
    Sig p
end

fun Result((_, _, types), vals) =
let val vts = vals ||| types
in
    implode(((fn ((v, t), r) => (writer(if v="()" then "" else v, t)) :: r ) </ []) vts)
end
handle Zip => (print "INTERNAL ERROR #1\n\n"; raise Interrupt);

fun typeFuns(name, ctypename, idltypename) =
implode
[       "datatype ", name, " = ", name, " of ", idltypename, ";\n",
	"fun write_", name, "(", name, " ", idltypename, "'V", ")=",
	     "write_", idltypename, "(", idltypename, "'V", ");\n",
	"fun read_", name, "() = ",
	     name, "(", "read_", idltypename, "()", ");"
]

fun makemlbody env p =
case p of
   Sign s => write (Fun env s)
|  Defn (s, c, []) => (write (Fun env s))
|  Defn (s, c, r) => (write (Fun env s))
|  Code c => ()
|  Line _ => ()
|  Sml c => write ("\n" ^ c)
|  Val (name, code) => write ("\nval " ^ name ^ " =\n" ^ code)
|  TypeRep r => write (typeFuns r)
|  TypeDef r => write (typeFuns r)
|  Seq (p1 as Val _, p2) => (makemlbody env p1; write "\n"; makemlbody env p2)
|  Seq (p1 as TypeRep _, p2) =>
   (makemlbody env p1; write "\n"; makemlbody env p2)
|  Seq (p1 as TypeDef _, p2) =>
   (makemlbody env p1; write "\n"; makemlbody env p2)
|  Seq (p1 as Sml _, p2) => (makemlbody env p1; write "\n"; makemlbody env p2)
|  Seq (p1 as Code _, p2) => (makemlbody env p1; write "\n"; makemlbody env p2)
|  Seq (p1, p2) => (makemlbody env p1; write "\n"; makemlbody env p2)

fun Proc typenv (name, outputs, inputs) =
implode
    [ "static void "
    , name
    , "(" , cparams outputs , ") "
    , cparamds typenv outputs
    , "\n"
    ]

and cparams []      = ""
|   cparams [p]     = cparamt p
|   cparams (p::ps) = implode[cparamt p, ", ", cparams ps]

and cparamt (n, t)   = n

and cparamds typenv []      = ""
|   cparamds typenv [p]     = cparamd typenv p
|   cparamds typenv (p::ps) = implode[cparamd typenv p, " ", cparamds typenv ps]

and cparamd typenv (n, t)   = implode[typeof t typenv, " ", n, ";"]

and creaders []      = ""
|   creaders [t]     = creader t
|   creaders (t::ts) = implode[creader t, ",", creaders ts]

and creader(n, t) = n

and declreaders typenv []      = ""
|   declreaders typenv [t]     = declreader typenv t
|   declreaders typenv (t::ts) = implode[declreader typenv t, declreaders typenv ts]

and declreader typenv (n, t)   = implode[typeof t typenv, " ", n, " = read_", t, "(); "]

fun typerep(name, cty, idlty) =
implode
[ "#define read_",  name, " (", implode cty, ") read_",  idlty, "\n"
, "#define write_", name, " write_", idlty, "\n"
]

fun typedef(name, cty, idlty) =
implode
[ "#define read_",  name, " (", name, ") read_",  idlty, "\n"
, "#define write_", name, " write_", idlty, "\n"
, "typedef ", implode cty, " ", name, ";\n"
]

fun makecproc typenv p =
case p of
    Sign s                  => ()
|   Defn (s, c, [])         => (write(Proc typenv s); write("{ "^c^"}\n\n"))
|   Defn (s, c, r)          => (write(Proc typenv s); write("{ "^c^Result(s, r)^"\n}\n\n"))
|   Code c                  => write(c)
|   Line n                  => (write "#line "; writeint n)
|   Sml c                   => ()
|   Val c                   => ()
|   TypeRep r               => (write(typerep r))
|   TypeDef r               => (write(typedef r))
|   Seq(p1, p2)             => (makecproc typenv p1; write "\n"; makecproc typenv p2);

fun cformats name outputs =
let val quote = "\"";
    val fmt = fn "int"     => "%d"
	      |  "string"  => "\\\"%s\\\""
	      |  "real"    => "%f"
	      |  t         => t^"(%d)"

    fun formats [] = []
    |   formats [(n,t)] = [fmt t]
    |   formats ((n,t)::nts) = fmt t :: ", " :: formats nts

in
    implode ( quote :: name :: "(" ::  formats outputs @ [")\\n", quote])
end;

fun debugcproc name outputs =
    if !debug then
       "fprintf(stderr, "^creaders((cformats name outputs, "string") :: outputs)^"); "
    else "";

fun Case env typenv (name, outputs, inputs) =
implode
    [ "   case "
    , makestring(seek name env)
    , ": { "
    , declreaders typenv outputs
    , debugcproc name outputs
    , name
    , "("
    , creaders outputs
    , "); }; break;\n"
    ];

fun makeswitch env typenv p =
case p of
    Sign s         => write(Case env typenv s)
|   Defn (s, c, r) => write(Case env typenv s)
|   Seq(p1, p2)    => (makeswitch env typenv p1; makeswitch env typenv p2)
|   _              => ();

fun makeidlserver env typenv p =
let
    val pro = "void idlevent()\n{\n switch(read_int())\n {\n"
    val epi = " };\n idlreset();\n}\n"
in
    write pro;
    makeswitch env typenv p;
    write epi;
    write "void idlserver() { while (idlcontinue) idlevent(); }\n"
end

fun makec filename env typenv p =
let val ts      = set(types p \ typedefs p)
in  app write  ["/*\n\tGenerated by ", Title, Version, "\n*/\n"];
    write "\n#line 1 \"[prologue--"; writestring filename; write "]\"\n";
    (*
	Perhaps we should just copy the idlbase.h file here
    *)
    write "typedef  char*         string;\n";
    write "typedef  void          unit;\n";
    write "typedef  char*         address;\n";
    write "typedef  char          bool;\n";
    write "typedef  unsigned char byte;\n";
    write "extern   int  idlcontinue;\n";
    write "extern   void idlserver();\n";
    write "extern   void idlreset();\n";

    app
     (fn t => write("extern   "^t^" read_"^t^"();\nextern   void write_"^t^"();\n"))
     ts;

    write "\n#line 1 \""; writestring filename; write "\"\n";
    makecproc typenv p;
    write "\n#line 1 \"[epilogue--"; writestring filename; write "]\"\n";
    makeidlserver env typenv p
end;

fun makeml env p =
let
in  app write ["(*\n\tGenerated by ", Title, Version, "\n*)\n"];
    makemlbody env p
end;


fun withoutput name f x =
let val s = SwapStream outstream (open_out name)
in  f x;
    close_out(SwapStream outstream s)
end;

fun afterFirstSml extra p =
let fun flat  (Seq(a, b)) e  = flat a (flat b e)
    |   flat  p           e  = p :: e
    fun seq ps = Seq // ps
    fun insert [] = [extra]
    |   insert ((x as Sml _) :: xs) = x :: extra :: xs
    |   insert (x::xs) = x :: insert xs
in
    seq(insert(flat p []))
end

fun makeidl filename =
let val p        = idlfile (filename^".idl");
    val s        = Signature p;
    val sigproc  = Defn(("GetSignature", [], ["string"]), "",
			["\""^s^"\""])
    val e        = "GetSignature" :: mkenv p;
    val extra    = Seq(Val("Signature", "\""^s^"\""), sigproc)
    val program  = (afterFirstSml extra p)
    val ctypenv  = typenv p
in
    withoutput  (filename^".c")
		(makec  (filename^".idl") e ctypenv) program;
    withoutput  (filename^".sml")
		(makeml e) program
end;

fun main(args, _) =

(   app (outputc std_err) [Title, Version, "\n"];
    case args of
	 _::"-d"::name::_=> (debug := true; makeidl name)
    |    _::name::_      => makeidl name

);

fun save name = exportFn(name, main);

fun export() = save "/tmp/idl";


(* ------------------------------------- *)

