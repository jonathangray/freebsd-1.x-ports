(*
	ML pretty printer and pretty-printer-generator
	@(#)mlkernel.sml	2.5 93/02/05 15:03:46
*)

(*

  Make mlkernel.write.sml with

   System.system
  "mldata -comments      \
  \     -string prstring \
  \     -pre    pr       \
  \     -prim   pr       \
  \     -hom    prlayer mlkernel.syntax.sml > mlkernel.write.sml";

  Make mlkernel.{sym,tab}.sml and mlkernel.tab.data by

  System.system "mllama mlkernel";

*)
use     "lib/Import.sml"; structure Import=Import(); open Import;
val     Version = "@(#)mlkernel.sml	2.5 93/02/05 15:03:46";
val     VERSION = "(*\n " ^ Version ^ "\n " ^ System.version ^ "\n*)";
Import  "lib/list.sig";
Import  "lib/list";         structure list=list();          open list;
Import  "lib/streamio.sig";
Import  "lib/streamio";     structure streamio=streamio();  open streamio;
Import  "utils";            structure utils = utils();      open utils;
Import  "lib/sml-lr.sig";
Import  "lib/sml-lr";       structure LR = LR();
Import  "lib/extend.sig";
Import  "lib/extend";       structure extend=extend();      open extend;


structure serial =
struct
   open Array;

   infix 9 sub;

   type 'a serial = int * 'a;

   fun serial constructor =
   let val n = ref 0
   in
       (fn f => fn x => constructor (! n before inc n, f x),
	fn () => ! n,
	fn () => n := 0
       )
   end;

   fun evalserial eval'a cache (n, a) =
   case cache sub n of
      SOME v => v
   |  NONE   => let val v = eval'a a in update (cache, n, SOME v); v end
end;

open serial;

fun prserial pr cache = evalserial pr (! cache);


structure printable =
struct
   datatype printable = prstring of string   |  prnode of printable list  ;

   fun print (i : int) = prstring (makestring i);

   fun prlayer [x] = x
   |   prlayer xs  = prnode xs;

   fun prlist pr punct =
   let val punct = prstring punct;
       fun F []        = []
       |   F [x]       = [pr x]
       |   F (x :: xs) = pr x :: punct :: F xs
   in
       prlayer o F
   end;

   fun proption pr none NONE     = prstring none
   |   proption pr none (SOME x) = pr x;

   fun prunit () = prstring "()";

   fun writeprintable (prstring s) = writestring s
   |   writeprintable (prnode ps)  = app writeprintable ps;

   fun printsize (prstring s) = String.size s
   |   printsize (prnode ps)  = (0 /> (fn (n, p) => n + printsize p)) ps
end;

open printable;

fun writequoted s = prstring (quoted s);

use "mlkernel.write.sml";

fun writeD d = (writeprintable (Trees.prD d); write "\n");

use "mlkernel.sym.sml";

structure std_err =
struct
	val std_err = std_err;
	val err_output = outputc std_err;
end;

structure buffer =
struct
   val ch = ref " ";

   val size = 100;

   val buf = ref (array (size, ""));

   val p = ref 0;

   fun state () =
   let fun P n =
       if n = ! p then [! buf sub n] else (! buf sub n) :: P ((n + 1) mod size)
   in
       implode (P (! p + 1))
   end;

   val stream = ref std_in;

   val linecount = ref 0;

   val text = ref 0;

   val permitnotes = ref true;

   fun fromfile s =
   ( buf := array (size, "");
     if s = "/dev/stdin" orelse s="-" then (linecount := 1; ch := " "; text := 0; true)
     else (stream := open_in s; ch := " "; text := 0; linecount := 1; true)
   )
   handle
      Io s => (std_err.err_output (s ^ "\n"); false);

   fun reset () = (close_in (! stream); stream := std_in; ch := " "; text := 0);

   fun nextch () =
   ( ch := input (! stream, 1);
     p := (! p + 1) mod size;
     update (! buf, ! p, ! ch);
     case ! ch of
	"\n" => (inc linecount; text := 0)
     |  c    => if c > " " then inc text else ();
     ! ch
   )
end;

structure lexical =
struct
   open Trees ref;

   datatype Class = Ignore   | End   | Letter   | Digit   | Symb   | Punct
		  | Bra      | Dot   | Qt
		  ;

   val note    = ref ([] : string list);
   val comment = ref ([] : string list);

   fun commentch() = let val ch=buffer.nextch() in comment ::= ch; ch end

   val class = array (256, Ignore);

   fun classify kind chars =
   app (fn c => update (class, ord c, kind)) (explode chars);

   val _ = classify Symb "#!%&$+-/|:<=>?@\\~*`^";

   val _ = classify Letter "abcdefghijklmnopqrstuvwxyz";

   val _ = classify Letter "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

   val _ = classify Letter "_'";

   val _ = classify Digit "0123456789";

   val _ = classify Punct ",;){}[]";

   val _ = classify Dot ".";

   val _ = classify Qt "\"";

   val _ = classify Bra "(";

   fun Class ch = if ch = "" then End else class sub ord ch;

   open buffer SYMBOLS;

   fun internid ss = internlit (implode ss)

   and internlit ss =
   let val k =
       case ss of
	  "datatype"  => DATATYPE
       |  "of"        => OF
       |  "abstype"   => ABSTYPE
       |  "with"      => WITH
       |  "and"       => AND
       |  "withtype"  => WITHTYPE
       |  "local"     => LOCAL
       |  "in"        => IN
       |  "end"       => END
       |  "if"        => IF
       |  "then"      => THEN
       |  "else"      => ELSE
       |  "case"      => CASE
       |  "open"      => OPEN
       |  "val"       => VAL
       |  "fun"       => FUN
       |  "rec"       => REC
       |  "raise"     => RAISE
       |  "andalso"   => ANDALSO
       |  "orelse"    => ORELSE
       |  "while"     => WHILE
       |  "do"        => DO
       |  "as"        => AS
       |  "let"       => LET
       |  "handle"    => HANDLE
       |  "nonfix"    => NONFIX
       |  "structure" => STRUCTURE
       |  "struct"    => STRUCT
       |  "signature" => SIGNATURE
       |  "sig"       => SIG
       |  "functor"   => FUNCTOR
       |  "sharing"   => SHARING
       |  "infix"     => INFIX
       |  "eqtype"    => EQTYPE
       |  "exception" => EXCEPTION
       |  "include"   => INCLUDE
       |  "infixr"    => INFIXR
       |  "op"        => OP
       |  "type"      => TYPE
       |  "*"         => PROD
       |  "("         => BRA
       |  ")"         => KET
       |  "["         => LBRA
       |  "]"         => LKET
       |  "{"         => RECBRA
       |  "}"         => RECKET
       |  "="         => EQ
       |  ","         => COMMA
       |  "|"         => BAR
       |  ";"         => SEMI
       |  ":"         => COLON
       |  "->"        => ARROW
       |  "fn"        => FN
       |  "=>"        => IMP
       |  "."         => DOT
       |  "import"    => IMPORT
       |  "<end>"     => llEND
       |  _           => I
   in
       (k, ID ss)
   end

   and internformat chars = (FORMAT, ID (implode chars));

   fun internnum chars = (NUM, ID (implode chars));

   fun internstring chars = (STRING, ID (implode chars));


   fun ScanWhileClass P intern chars =
   if P (Class (nextch ())) then ScanWhileClass P intern (! ch :: chars)
   else intern (rev chars);

   fun Scan () =
   case Class (! ch) of
      Ignore => (nextch (); Scan ())
   |  Letter =>
      ScanWhileClass (fn Letter => true | Digit => true | _ => false)
      internid
      [! ch]
   |  Digit =>
      if !ch="0" then
	 (nextch();
	  if !ch="x" then
	     ScanWhileClass (fn Dot => true | Digit => true | _ => false)
	     internnum
	     ["x", "0"]
	  else
	  if Class(!ch)=Digit orelse Class(!ch)=Dot then
	     ScanWhileClass (fn Dot => true | Digit => true | _ => false)
	     internnum
	     [!ch, "0"]
	  else
	     internnum
	     ["0"]
	 )
      else
      ScanWhileClass (fn Dot => true | Digit => true | _ => false)
      internnum
      [! ch]
   |  Symb   => ScanWhileClass (fn Symb => true | _ => false) internid [! ch]
   |  Punct  => internid [! ch] before nextch ()
   |  Bra    => ScanBra (nextch ())
   |  Dot    => ScanWhileClass (fn Dot => true | _ => false) internid [! ch]
   |  Qt     => internstring (ScanStr [] (nextch ()))
   |  End    => (internlit "<end>")

   and ScanStr r c =
   case c of
      ""   => rev r
   |  "\\" => ScanStr (nextch () :: c :: r) (nextch ())
   |  "\"" => rev r before nextch ()
   |  _    => ScanStr (c :: r) (nextch ())

   and ScanBra c =
   case c of
      ""  => Scan (std_err.err_output "[Runaway comment]\n")
   |  "*" => ScanCommentBegin (nextch ())
   |  _   => internlit "("

   and ScanCommentBegin c =
   case c of
      ""   => Scan (std_err.err_output "[Runaway comment]\n")
   |  "["  => ScanFormat c
   |  "\"" => ScanFormat c
   |  "{"  => ScanFormat c
   |  _    => (comment := [c, "(*"]; ScanComment 0 c)

   and Note () =
   (if (! buffer.permitnotes) then
       note ::= implode (rev (!comment))
    else
       ();
    comment := [];
    Scan ()
   )

   and ScanComment level c =
   case c of
      ""  => (std_err.err_output "[Runaway comment]\n"; Note ())
   |  "*" => ScanCommentEnd level (commentch ())
   |  "(" =>
      (case commentch () of
	  "*" => ScanComment (level + 1) (commentch ())
       |  c   => ScanComment level (commentch ()))
   |  _   => ScanComment level (commentch ())

   and ScanCommentEnd level c =
   case c of
      "" => (std_err.err_output "[Runaway comment]\n"; Note ())
   |  ")" =>
      if level <= 0 then (nextch (); Note ())
      else ScanComment (level - 1) (commentch ())
   |  _    => ScanComment level (c)

   and ScanFormat bra =
   let val ket = case bra of "[" => "]" | "\"" => "\"" | "{" => "}";
       fun Rem1 r =
       case nextch () of
	  ""   => r
       |  "\\" => Rem1 (nextch () :: "\\" :: r)
       |  "`"  => Rem1 (nextch () :: "`" :: r)
       |  c    => if c = ket then Rem2 r else Rem1 (! ch :: r)
       and Rem2 r =
       case nextch () of "" => r | "*" => Rem3 r | c => Rem1 (c :: ket :: r)
       and Rem3 r =
       case nextch () of
	  ""  => r
       |  ")" => r before nextch ()
       |  c   => Rem1 (c :: "*" :: ket :: r)
   in
       internformat (rev (ket :: Rem1 [bra]))
   end;

   fun reset () = (buffer.reset (); note := []; comment := [])

end;

(*
	Although the safe way to print ``unparsed'' expression bodies
	(constructor Expr within E) is to put spaces between each of
	the component expressions, this can lead to some rather ugly
	looking text. So the prettyprinter for Expr nodes is left
	unspecified (to be more precise, it's the value of the variable
	ExprPrinter within Syntax, which is initially specified as a
	no-op) until we're in a position to define whether a pair of
	expressions can abut each other directly or need separating by
	spaces (i.e. after the lexical classes have been defined). The
	structure below defines a suitable printing function then
	assigns it to ExprPrinter.
*)

structure ExprPrint =
struct
   val Wide = ref true;

   fun firstchar (prstring s)      = if s = "" then s else substring (s, 0, 1)
   |   firstchar (prnode [])       = ""
   |   firstchar (prnode (p :: _)) = firstchar p;

   fun lastchar (prstring s) =
       if s = "" then s else substring (s, String.size s - 1, 1)
   |   lastchar (prnode []) = ""
   |   lastchar (prnode ps) = lastchar (hd (rev ps));

   fun NonFollow c1 c2 =
   let open lexical;
       val B =
       fn "[" => true
       |  "(" => true
       |  "{" => true
       |  "}" => true
       |  ")" => true
       |  "]" => true
       |  _   => false
   in
       case (Class c1, Class c2) of
	  (Letter, Digit) => false
       |  (Digit, Letter) => false
       |  (Qt, _)         => false
       |  (_, Qt)         => false
       |  (a, b)          => if B c1 orelse B c2 then false else a <> b
   end;

   fun Punctuate []   = []
   |   Punctuate [pr] = [pr]
   |   Punctuate (pr1 :: (rest as (pr2 :: prs))) =
       if NonFollow (lastchar pr1) (firstchar pr2) then pr1 :: Punctuate rest
       else pr1 :: prstring " " :: Punctuate rest;

   fun PrintExpr es =
   if ! Wide then prlist Syntax.prE " " es
   else prnode (Punctuate (Syntax.prE MAP es));

   val _ = Syntax.ExprPrinter := PrintExpr
end;


fun Expression [x] = x
|   Expression xs  = Trees.Expr xs;

val processor = ref (fn d : Trees.D => ());

fun process d = (Trees.InitCache (); ! processor d; Trees.ResetCache ());

fun mkNote con "" x      = x
|   mkNote con comment x = con (comment, x);

fun mkCom ""      = Trees.NoComment
|   mkCom comment = Trees.SomeComment comment;

local
    open lexical
in
    fun CurrentNote () = implode (rev (! note)) before note := []
end;


use "mlkernel.tab.sml";

fun Recover skip =
if skip then
let open std_err
in
    err_output "Syntax error just after\n";
    err_output (buffer.state ());
    err_output "\n"
end
else ();

local
    val Parse = PARSER.PARSE (fn _ => ()) Recover lexical.Scan;
    open buffer Trees
in
    fun ParseThen f file =
    if fromfile file then
    let val ref processor' = processor
    in
	processor := f;
	ResetCache ();
	Parse false;
	lexical.reset ();
	processor := processor'
    end
    else ();

    val try = ParseThen (! processor)
end;

structure formats =
struct
   datatype Class = Normal   |  Arg   |  Fun   |  Digit   |  End  ;

   val class = array (256, Normal);

   fun classify kind chars =
   app (fn c => update (class, ord c, kind)) (explode chars);

   val _ = classify Arg "@";

   val _ = classify Fun "$";

   val _ = classify Digit "0123456789";

   fun Class ch = if ch = "" then End else class sub ord ch;

   val ch = ref " ";

   val stream = ref std_in;

   fun nextch () = (ch := input (! stream, 1); ! ch);

   fun ThisClass () =
   case ! ch of "`" => (nextch (); Normal) | _ => Class (! ch);

   fun NextClass () = (nextch (); ThisClass ());

   fun ScanWhileClass P intern chars =
   if P (NextClass ()) then ScanWhileClass P intern (! ch :: chars)
   else intern (rev chars);

   datatype format =
       END
     | ARG   of int
     | STR   of string
     | FUN   of string * int
     | EXTRA of string * int
     | ML    of string      ;

   val Visible = (fn Normal => true | Digit => true | _ => false);

   fun mknum n []        = n
   |   mknum n (d :: ds) = mknum ((ord d - ord "0") + 10 * n) ds;

   fun Number () =
   ScanWhileClass (fn Digit => true | _ => false) (mknum 0) [! ch];

   fun Text P = ScanWhileClass P implode [! ch];

   fun ExtraParam () =
   let val s =
       case ThisClass () of
	  Arg => ""
       |  _   => Text (fn End => false | Arg => false | _ => true)
   in
       case NextClass () of Digit => EXTRA (s, Number ()) | _ => ML s
   end;

   fun Scan () =
   case ThisClass () of
      End => END
   |  Normal => STR (Text Visible)
   |  Digit => STR (Text Visible)
   |  Arg =>
      (case NextClass () of Digit => ARG (Number ()) | _ => ExtraParam ())
   |  Fun =>
      let val s =
	  (nextch (); Text (fn End => false | Fun => false | _ => true))
      in
	  nextch ();
	  FUN (s, Number ())
      end;

   fun Parse () = case Scan () of END => [] | item => item :: Parse ();

   fun ParseFormat string =
   if string <> "" andalso substring (string, 0, 1) = "{" then
      [ML (substring (string, 1, String.size string - 2))]
   else
   let val s = open_string (substring (string, 1, String.size string - 2))
   in
       stream := s;
       nextch ();
       Parse () before close_in s
   end
end;

structure objectprogram =
struct
   open Trees;

   val WriteString = ref "writestring";

   val FunPrefix = ref "write";

   val PrimPrefix = ref "write";

   val LayerPrint = ref "";

   val Canonical = ref false;

   val Hom = ref false;

   val Primitive =
   fn "int"    => true
   |  "string" => true
   |  "real"   => true
   |  "unit"   => true
   |  "list"   => true
   |  "option" => true
   |  _        => false;

   fun PrefixFor s = ! (if Primitive s then PrimPrefix else FunPrefix);

   fun FunId s = Id ([PrefixFor s ^ s]);

   val Wild = Id ["_"];

   fun MlQ s = Ml ("\"" ^ s ^ "\"");

   val Unit = Prod [];

   fun Apply (f, x) = case x of Ap _ => Ap (f, Tuple [x]) | _ => Ap (f, x);

   fun Wrs s = Apply (Id [! WriteString], MlQ s);

   fun WrQ s = Apply (Id [! WriteString], String (quoted s));

   fun WrCon s = Wrs (s ^ "(");

   fun Param (n) = Id ["S'" ^ makestring (n : int)];

   fun Layer l =
   if not (! Hom) then Tuple [Seq l] else Apply (Id [! LayerPrint], List l);

   infixr 9 at;

   fun env at a =
   let fun L []              = NONE
       |   L ((x, y) :: env) = if x = a then SOME y else L env
   in
       L env
   end;

   fun Interleave x [] z        = z
   |   Interleave x [y] z       = y :: z
   |   Interleave x (y :: ys) z = y :: x :: Interleave x ys z;

   fun PrintName ty =
   implode
   (case ty of
       Tnum (_, t)     => [PrintName t]
    |  TComment (_, t) => [PrintName t]
    |  Tid s           => [hd (rev s)]
    |  Bra ts          => "(" :: map PrintName ts @ [")"]
    |  Fun (t1, t2)    => [PrintName t1, " -> ", PrintName t2]
    |  Prod ts         => map PrintName ts
    |  Record fs =>
       ["{"] @ Interleave ","
       ((fn (s, t) => implode [s, ":", PrintName t]) MAP fs) ["}"]
    |  Inst (t1, t2)   => [PrintName t1, " ", PrintName t2]);

   fun Params (Record fs) =
       Rec ((fn (i, (s, t)) => EqBind (s, Param i)) MAP (enumerate1 fs))
   |   Params (Prod ts) = Tuple ((fn (i, t) => Param i) MAP (enumerate1 ts))
   |   Params (Tnum (_, t)) = Params t
   |   Params (TComment (_, t)) = Params t
   |   Params t = Param 1;

   fun Env (Prod ts)         = enumerate1 ts
   |   Env (Record fs)       = enumerate1 (# 2 MAP fs)
   |   Env (Tnum (_, t))     = Env t
   |   Env (TComment (_, t)) = Env t
   |   Env ty                = [(1, ty)];

   fun Canon ty =
   case ty of
      Tnum (_, t)     => Canon t
   |  TComment (_, t) => Canon t
   |  Tid s           => FunId (hd (rev s))
   |  Bra tys         => Tuple (Canon MAP tys)
   |  Fun _           => Fn [(Wild, WrQ ("fn: " ^ PrintName ty))]
   |  Prod _ => Fn [(Params ty, ProdBody (Wrs "(") (Wrs ")") (Env ty))]
   |  Record fs       => Fn [(Params ty, RecBody (enumerate1 fs))]
   |  Inst (t1, t2)   => Apply (Canon t2, Canon t1)

   and ProdBody bra ket env =
   Layer (bra :: Interleave (Wrs ",") (PrintParam MAP env) [ket])

   and RecBody env =
   Layer (Wrs "{" :: Interleave (Wrs ",") (PrintField MAP env) [Wrs "}"])

   and PrintParam (i, t) = Apply (Canon t, Param i)

   and PrintField (i, (s, t)) = Layer [Wrs s, Wrs "=", PrintParam (i, t)];

   fun MakeFn e = case e of Fn _ => e | _ => Fn [(Param 0, Apply (e, Param 0))];

   fun PrintItemWith f env item =
   let open formats std_err;
       fun Err n =
       ( app err_output
	 ["Bad item number (", makestring n, ") in format: ", f, "\n"];
	 Wrs "??"
       )
   in
       case item of
	  ARG n =>
	  (case env at n of NONE => Err n | SOME t => Apply (Canon t, Param n))
       |  STR s => Wrs s
       |  ML s => Ml s
       |  FUN (s, n) =>
	  (case env at n of NONE => Err n | SOME t => Apply (Ml s, Param n))
       |  EXTRA (s, n) =>
	  (case env at n of
	      NONE   => Err n
	   |  SOME t => Expr [Canon t, MlQ s, Param n])
   end;

   fun PrintWith env format =
   Layer (PrintItemWith format env MAP formats.ParseFormat format);

   fun Dec (name, printer) =
   case name of
      Tnum (_, t)     => Dec (t, printer)
   |  TComment (_, t) => Dec (t, printer)
   |  Tid i           => (FunId (hd (rev i)), printer)
   |  Inst (params, Tid i) =>
      (FunId (hd (rev i)), Fn [(Canon params, printer)]);

   fun TyPrint MakeFn ty format =
   case format of
      NONE => MakeFn (Canon ty)
   |  SOME format =>
      if ! Canonical then MakeFn (Canon ty)
      else Fn [(Params ty, PrintWith (Env ty) format)];

   fun DyPrint cases = Fn (DyCase MAP cases)

   and DyCase c =
   case c of
      Constant (i, NONE, _) => (Id [i], Wrs i)
   |  Constant (i, SOME format, _) =>
      if ! Canonical then DyCase (Constant (i, NONE, NoComment))
      else (As (Param 1, Id [i]), PrintWith [(1, Unit)] format)
   |  Construct (i, ty, format, _) =>
      if ! Canonical then DyConstruct (i, ty, NONE)
      else DyConstruct (i, ty, format)

   and DyConstruct (i, ty, format) =
   case format of
      NONE =>
      (Apply (Id [i], Params ty), ProdBody (WrCon i) (Wrs ")") (Env ty))
   |  SOME format => (Apply (Id [i], Params ty), PrintWith (Env ty) format);

   fun Def d =
   case d of
      Dnum (_, d)     => Def d
   |  CommentD (_, d) => Def d
   |  Data dys        => RecDecs (DataDecl MAP dys)
   |  With (dys, tys) =>
      RecDecs ((DataDecl MAP dys) @ (TypeDecl MakeFn MAP tys))
   |  Type tys        => ValDecs ((TypeDecl (fn e => e) MAP tys))

   and TypeDecl MakeFn (l, r, format, _) = Dec (l, TyPrint MakeFn r format)

   and DataDecl ((l, cases)) = Dec (l, DyPrint cases);

   val flat = (op @ </ []);

   fun WriterDeclaration d =
   case d of
      StrDecs strs => StrDecs (WriterInStruct MAP strs)
   |  SeqDecs ds   => SeqDecs (WriterDeclaration MAP ds)
   |  Dnum (_, d)  => WriterDeclaration d
   |  CommentD(c, d) => CommentD(c, WriterDeclaration d)
   |  d            => (case WDecl d of [] => d | ds => SeqDecs (d :: ds))

   and WDecl d =
   case d of
      Type _      => [Def d]
   |  Data _      => [Def d]
   |  With _      => [Def d]
   |  Dnum (_, d) => WDecl d
   |  SeqDecs ds  => flat (WDecl MAP ds)
   |  _           => []

   and WriterInStruct (i, sigopt, s) =
   (i, sigopt,
    (case s of
	  Struct d => Struct (SeqDecs (d :: WDecl d))
    |     _        => s
    )
   )
end;



val Defaults =
["-width", "70", "-pre", "W", "-prim", "write", "-string", "writestring",
 "--can", "--hom", "-wide", "-comments"];

fun Help () =
app std_err.err_output
(["usage: mldata [<switch> | <filename>]*\n",
  "-pre <string> sets prefix for non-primitive types\n",
  "-prim <string> sets prefix for primitive types\n",
  "-string <string> name of function to write strings\n",
  "-can  produce canonical (rather than custom)  output procedures\n",
  "-hom <layer> generate  <layer> homomorphisms\n",
  "-wide leave spaces between expression components if possible\n",
  "-comments preserve certain sorts of comment\n",
  "--comments obliterate all comments\n",
  "--wide turn off -wide\n",
  "--hom turn off -hom\n",
  "--can turn off -can\n",
  "-width  <number>  limit indented program width to <number>\n",
  "-pretty  <file> prettyprint file\n",
  "-switches  show state of switches\n",
  "(\"/dev/stdin\" and \"-\" both mean the standard input stream)\n",
  "Default switches are\n"] @ map (fn s => s ^ " ") Defaults @ ["\n"]);

use "mlkernel.pretty.sml";

fun Show () =
let open objectprogram
in
    app std_err.err_output
    [" -prim ", ! PrimPrefix,
     " -pre ", ! FunPrefix,
     " -string ", ! WriteString,
     " -width ", (makestring (! pretty.rm)),
     (if ! ExprPrint.Wide then " -wide" else " --wide"),
     (if ! Canonical then " -can" else " --can"),
     (if ! Hom then " -hom " ^ ! LayerPrint else " --hom"), "\n"]
end;

fun Do args =
let open objectprogram
in
    case args of
       "-can" :: args            => (Canonical := true; Do args)
    |  "--can" :: args           => (Canonical := false; Do args)
    |  "-pre" :: s :: args       => (FunPrefix := s; Do args)
    |  "-prim" :: s :: args      => (PrimPrefix := s; Do args)
    |  "-string" :: s :: args    => (WriteString := s; Do args)
    |  "-hom" :: s :: args       => (Hom := true; LayerPrint := s; Do args)
    |  "-width" :: s :: args =>
       (pretty.rm := formats.mknum 0 (explode s); Do args)
    |  "--hom" :: args           => (Hom := false; LayerPrint := ""; Do args)
    |  "--wide" :: args          => (ExprPrint.Wide := false; Do args)
    |  "-wide" :: args           => (ExprPrint.Wide := true; Do args)
    |  "-comments" :: args       => (buffer.permitnotes := true; Do args)
    |  "--comments" :: args      => (buffer.permitnotes := false; Do args)
    |  "-help" :: args           => Help ()
    |  "-switches" :: args       => (Show (); Do args)
    |  "-pretty" :: file :: args => (Pretty file; Do args)
    |  "-version" :: args        => (writestring VERSION; Do args)
    |  file :: args              => (Writer file; Do args)
    |  []                        => ()
end;

fun Main (_ :: args, _) = Do (args);

fun export () = (Do Defaults; exportFn ("/tmp/mldata", Main));

