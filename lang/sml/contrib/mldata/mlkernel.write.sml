structure Syntax =
struct
   type ID = string(*[@1]*);
   
   type CID = string list(*[@.@1]*);
   
   datatype COM = 
       NoComment        (*[]*)
     | SomeComment of string(*[(*@1*)]*);
   
   datatype D = 
       Dnum     of D serial    (*[$prserial prD Dtext$1]*)
     | CommentD of string * D  (*[@1 @2]*)
     | Comment  of string      (*[@1]*)
     | Type     of TYS         (*[type @1]*)
     | Data     of DYS         (*[datatype @1]*)
     | With     of DYS * TYS   (*[datatype @1 withtype @2]*)
     | AbsData  of DYS * D     (*[abstype @1 with @2 end]*)
     | AbsWith  of DYS * TYS * D
       (*[abstype @1 withtype @2 with @3 end]*)
       
     | FunDecs  of FUN list    (*[fun @ and @1]*)
     | ValDecs  of EQN list    (*[val @ and @1]*)
     | RecDecs  of EQN list    (*[val rec @ and @1]*)
     | ExnDecs  of EXNDEC list (*[exception @ and @1]*)
     | StrDecs  of STRDEC list (*[structure @ and @1]*)
     | FuncDecs of FUNCDEC list(*[functor @ and @1]*)
     | SigDecs  of SIGDEC list (*[signature @ and @1]*)
     | SeqDecs  of D list      (*[@; @1]*)
     | LocDecs  of D * D       (*[local @1 in @2 end]*)
     | DirDec   of DIR         (*[@1]*)
     | ItDec    of E           (*[@1]*)
     | Import   of E list      (*[import @ @1]*)
   
   and DIR = 
       Open    of CID list(*[open @ @1]*)
     | Include of E list  (*[include @ @1]*)
     | Infix   of E list  (*[infix @ @1]*)
     | Infixr  of E list  (*[infixr @ @1]*)
     | Nonfix  of E list  (*[nonfix @ @1]*)
   
   and SPEC = 
       StructSpecs of STRSPEC list (*[structure @ and @1]*)
     | TypeSpecs   of T list       (*[type @ and @1]*)
     | EqTypeSpecs of T list       (*[eqtype @ and @1]*)
     | ValSpecs    of VS list      (*[val @ and @1]*)
     | ExnSpecs    of CASE list    (*[exception @ and @1]*)
     | DataSpecs   of DYS          (*[datatype @1]*)
     | ShareSpecs  of SS list      (*[sharing @ and @1]*)
     | LocSpecs    of SPECS * SPECS(*[local @1 in @2 end]*)
     | DirSpec     of DIR          (*[@1]*)
     | CommentS    of string * SPEC(*[@1 @2]*)
   
   and SIG = SigId of CID (*[@1]*)  |  Sig of SPECS (*[sig @1 end]*) 
   
   and SS = 
       ShareTy  of CID list(*[type @ = @1]*)
     | ShareStr of CID list(*[@ = @1]*)
   
   and T = 
       Tnum     of T serial  (*[$prserial prT Ttext$1]*)
     | TComment of string * T(*[@2 @1]*)
     | Tid      of CID       (*[@1]*)
     | Bra      of T list    (*[(@,@1)]*)
     | Prod     of T list    (*[@ * @1]*)
     | Record   of FIELD list(*[{@, @1}]*)
     | Fun      of T * T     (*[@1->@2]*)
     | Inst     of T * T     (*[@1 @2]*)
   
   and E = 
       Enum    of E serial      (*[$prserial prE Etext$1]*)
     | Id      of CID           (*[@1]*)
     | Op      of ID            (*[op @1]*)
     | Num     of string        (*[@1]*)
     | String  of string        (*[\"@1\"]*)
     | Ml      of string        (*[(@1)]*)
     | Ap      of E * E         (*[@1 @2]*)
     | AndAlso of E * E         (*[@1 andalso @2]*)
     | OrElse  of E * E         (*[@1 orelse @2]*)
     | As      of E * E         (*[@1 as @2]*)
     | Has     of VS            (*[@1]*)
     | Tuple   of E list        (*[(@, @1)]*)
     | Rec     of BIND list     (*[{@, @1}]*)
     | List    of E list        (*"[@, @1]"*)
     | Seq     of E list        (*[@; @1]*)
     | Let     of D * E list    (*[let @1 in @; @2 end]*)
     | If      of E * E * E     (*[if @1 then @2 else @3]*)
     | While   of E * E         (*[while @1 do @2]*)
     | Case    of E * MATCH list(*[case @1 of @ | @2]*)
     | Handle  of E * MATCH list(*[@1 handle @ | @2]*)
     | Raise   of E             (*[raise @1]*)
     | Fn      of MATCH list    (*[fn @ | @1]*)
     | Expr    of E list        (*[$printExpr$1]*)
   
   and CASE = 
       Construct of ID * T * A * COM(*[@1 of @2 @3 @4]*)
     | Constant  of ID * A * COM    (*[@1 @2 @3]*)
   
   and BIND = 
       EqBind  of ID * E(*[@1 = @2]*)
     | VarBind of E     (*[@1]*)
   
   and EXNDEC = 
       ExnAbbrev of ID * CID(*[@1 = @2]*)
     | ExnCase   of CASE    (*[@1]*)
   
   and STRUCT = 
       StrId        of CID            (*[@1]*)
     | Struct       of D              (*[struct @1 end ]*)
     | FuncApDec    of STRUCT * D     (*[@1(@2)]*)
     | FuncApStruct of STRUCT * STRUCT(*[@1(@2)]*)
     | LetStruct    of D * STRUCT     (*[let @1 in @2 end]*)
   
   and FUNPAR = 
       OpenPar  of SPEC list(*[@; @1]*)
     | ClosePar of STRSPEC  (*[@1]*)
   
   and SIGOPT = SomeSig of SIG (*[:@1]*)  |  NoSig (*[]*) 
   withtype
       REM     = string                       (*"@1"*)
   
   and  A       = REM option                   (*[@@1]*)
   
   and  DATA    = T * CASE list                (*[@1 = @ |  @2]*)
   
   and  FIELD   = ID * T                       (*[@1 : @2]*)
   
   and  VS      = E * T                        (*[@1 : @2]*)
   
   and  TY      = T * T * A * COM              (*[@1 = @2 @3 @4]*)
   
   and  TYS     = TY list                      (*[@ and @1]*)
   
   and  DYS     = DATA list                    (*[@ and @1]*)
   
   and  EQN     = E * E                        (*[@1 = @2]*)
   
   and  FUN     = EQN list                     (*[@ | @1]*)
   
   and  MATCH   = E * E                        (*[@1 => @2]*)
   
   and  IDandE  = ID * E                       (*[@1 = @2]*)
   
   and  STRDEC  = ID * SIGOPT * STRUCT         (*[@1 @2 = @3]*)
   
   and  SIGDEC  = ID * SIG                     (*[@1 = @2]*)
   
   and  FUNCDEC = ID * FUNPAR * SIGOPT * STRUCT(*[@1(@2) @3 = @4]*)
   
   and  STRSPEC = ID * SIG                     (*[@1 : @2]*)
   
   and  SPECS   = SPEC list                    (*[@; @1]*);
   
   val (mkE, Ecount, Ereset) = serial Enum;
   
   val (mkD, Dcount, Dreset) = serial Dnum;
   
   val (mkT, Tcount, Treset) = serial Tnum;
   
   val Etext = ref (array (0, NONE) : printable option array);
   
   val Dtext = ref (array (0, NONE) : printable option array);
   
   val Ttext = ref (array (0, NONE) : printable option array);
   
   fun InitCache () = 
   ( Etext := array (Ecount (), NONE);
     Dtext := array (Dcount (), NONE);
     Ttext := array (Tcount (), NONE)
   );
   
   fun ResetCache () = (Ereset (); Dreset (); Treset ());
   
   val ExprPrinter = ref (fn _ : E list => prnode []);
   
   fun printExpr e = ! ExprPrinter e;
   
   val prID = fn S'1 => prlayer [prstring S'1];
   
   val prCID = fn S'1 => prlayer [prlist prstring (".") S'1];
   
   val rec prCOM = 
   fn S'1 as NoComment => prlayer []
   |  SomeComment S'1 => 
      prlayer [prstring ("(*"), prstring S'1, prstring ("*)")];
   
   val rec prD = 
   fn Dnum S'1 => prlayer [(prserial prD Dtext) S'1]
   |  CommentD (S'1, S'2) => 
      prlayer [prstring S'1, prstring (" "), prD S'2]
   |  Comment S'1 => prlayer [prstring S'1]
   |  Type S'1 => prlayer [prstring ("type "), prTYS S'1]
   |  Data S'1 => prlayer [prstring ("datatype "), prDYS S'1]
   |  With (S'1, S'2) => 
      prlayer
      [prstring ("datatype "), prDYS S'1, prstring (" withtype "), 
       prTYS S'2]
   |  AbsData (S'1, S'2) => 
      prlayer
      [prstring ("abstype "), prDYS S'1, prstring (" with "), prD S'2, 
       prstring (" end")]
   |  AbsWith (S'1, S'2, S'3) => 
      prlayer
      [prstring ("abstype "), prDYS S'1, prstring (" withtype "), 
       prTYS S'2, prstring (" with "), prD S'3, prstring (" end")]
   |  FunDecs S'1 => 
      prlayer [prstring ("fun "), prlist prFUN (" and ") S'1]
   |  ValDecs S'1 => 
      prlayer [prstring ("val "), prlist prEQN (" and ") S'1]
   |  RecDecs S'1 => 
      prlayer [prstring ("val rec "), prlist prEQN (" and ") S'1]
   |  ExnDecs S'1 => 
      prlayer [prstring ("exception "), prlist prEXNDEC (" and ") S'1]
   |  StrDecs S'1 => 
      prlayer [prstring ("structure "), prlist prSTRDEC (" and ") S'1]
   |  FuncDecs S'1 => 
      prlayer [prstring ("functor "), prlist prFUNCDEC (" and ") S'1]
   |  SigDecs S'1 => 
      prlayer [prstring ("signature "), prlist prSIGDEC (" and ") S'1]
   |  SeqDecs S'1 => prlayer [prlist prD ("; ") S'1]
   |  LocDecs (S'1, S'2) => 
      prlayer
      [prstring ("local "), prD S'1, prstring (" in "), prD S'2, 
       prstring (" end")]
   |  DirDec S'1 => prlayer [prDIR S'1]
   |  ItDec S'1 => prlayer [prE S'1]
   |  Import S'1 => 
      prlayer [prstring ("import "), prlist prE (" ") S'1]
   
   and prDIR = 
   fn Open S'1 => prlayer [prstring ("open "), prlist prCID (" ") S'1]
   |  Include S'1 => 
      prlayer [prstring ("include "), prlist prE (" ") S'1]
   |  Infix S'1 => prlayer [prstring ("infix "), prlist prE (" ") S'1]
   |  Infixr S'1 => 
      prlayer [prstring ("infixr "), prlist prE (" ") S'1]
   |  Nonfix S'1 => 
      prlayer [prstring ("nonfix "), prlist prE (" ") S'1]
   
   and prSPEC = 
   fn StructSpecs S'1 => 
      prlayer
      [prstring ("structure "), prlist prSTRSPEC (" and ") S'1]
   |  TypeSpecs S'1 => 
      prlayer [prstring ("type "), prlist prT (" and ") S'1]
   |  EqTypeSpecs S'1 => 
      prlayer [prstring ("eqtype "), prlist prT (" and ") S'1]
   |  ValSpecs S'1 => 
      prlayer [prstring ("val "), prlist prVS (" and ") S'1]
   |  ExnSpecs S'1 => 
      prlayer [prstring ("exception "), prlist prCASE (" and ") S'1]
   |  DataSpecs S'1 => prlayer [prstring ("datatype "), prDYS S'1]
   |  ShareSpecs S'1 => 
      prlayer [prstring ("sharing "), prlist prSS (" and ") S'1]
   |  LocSpecs (S'1, S'2) => 
      prlayer
      [prstring ("local "), prSPECS S'1, prstring (" in "), 
       prSPECS S'2, prstring (" end")]
   |  DirSpec S'1 => prlayer [prDIR S'1]
   |  CommentS (S'1, S'2) => 
      prlayer [prstring S'1, prstring (" "), prSPEC S'2]
   
   and prSIG = 
   fn SigId S'1 => prlayer [prCID S'1]
   |  Sig S'1 => 
      prlayer [prstring ("sig "), prSPECS S'1, prstring (" end")]
   
   and prSS = 
   fn ShareTy S'1 => 
      prlayer [prstring ("type "), prlist prCID (" = ") S'1]
   |  ShareStr S'1 => prlayer [prlist prCID (" = ") S'1]
   
   and prT = 
   fn Tnum S'1 => prlayer [(prserial prT Ttext) S'1]
   |  TComment (S'1, S'2) => 
      prlayer [prT S'2, prstring (" "), prstring S'1]
   |  Tid S'1 => prlayer [prCID S'1]
   |  Bra S'1 => 
      prlayer [prstring ("("), prlist prT (",") S'1, prstring (")")]
   |  Prod S'1 => prlayer [prlist prT (" * ") S'1]
   |  Record S'1 => 
      prlayer
      [prstring ("{"), prlist prFIELD (", ") S'1, prstring ("}")]
   |  Fun (S'1, S'2) => prlayer [prT S'1, prstring ("->"), prT S'2]
   |  Inst (S'1, S'2) => prlayer [prT S'1, prstring (" "), prT S'2]
   
   and prE = 
   fn Enum S'1 => prlayer [(prserial prE Etext) S'1]
   |  Id S'1 => prlayer [prCID S'1]
   |  Op S'1 => prlayer [prstring ("op "), prID S'1]
   |  Num S'1 => prlayer [prstring S'1]
   |  String S'1 => 
      prlayer [prstring ("\""), prstring S'1, prstring ("\"")]
   |  Ml S'1 => prlayer [prstring ("("), prstring S'1, prstring (")")]
   |  Ap (S'1, S'2) => prlayer [prE S'1, prstring (" "), prE S'2]
   |  AndAlso (S'1, S'2) => 
      prlayer [prE S'1, prstring (" andalso "), prE S'2]
   |  OrElse (S'1, S'2) => 
      prlayer [prE S'1, prstring (" orelse "), prE S'2]
   |  As (S'1, S'2) => prlayer [prE S'1, prstring (" as "), prE S'2]
   |  Has S'1 => prlayer [prVS S'1]
   |  Tuple S'1 => 
      prlayer [prstring ("("), prlist prE (", ") S'1, prstring (")")]
   |  Rec S'1 => 
      prlayer
      [prstring ("{"), prlist prBIND (", ") S'1, prstring ("}")]
   |  List S'1 => 
      prlayer [prstring ("["), prlist prE (", ") S'1, prstring ("]")]
   |  Seq S'1 => prlayer [prlist prE ("; ") S'1]
   |  Let (S'1, S'2) => 
      prlayer
      [prstring ("let "), prD S'1, prstring (" in "), 
       prlist prE ("; ") S'2, prstring (" end")]
   |  If (S'1, S'2, S'3) => 
      prlayer
      [prstring ("if "), prE S'1, prstring (" then "), prE S'2, 
       prstring (" else "), prE S'3]
   |  While (S'1, S'2) => 
      prlayer
      [prstring ("while "), prE S'1, prstring (" do "), prE S'2]
   |  Case (S'1, S'2) => 
      prlayer
      [prstring ("case "), prE S'1, prstring (" of "), 
       prlist prMATCH (" | ") S'2]
   |  Handle (S'1, S'2) => 
      prlayer
      [prE S'1, prstring (" handle "), prlist prMATCH (" | ") S'2]
   |  Raise S'1 => prlayer [prstring ("raise "), prE S'1]
   |  Fn S'1 => prlayer [prstring ("fn "), prlist prMATCH (" | ") S'1]
   |  Expr S'1 => prlayer [(printExpr) S'1]
   
   and prCASE = 
   fn Construct (S'1, S'2, S'3, S'4) => 
      prlayer
      [prID S'1, prstring (" of "), prT S'2, prstring (" "), prA S'3, 
       prstring (" "), prCOM S'4]
   |  Constant (S'1, S'2, S'3) => 
      prlayer
      [prID S'1, prstring (" "), prA S'2, prstring (" "), prCOM S'3]
   
   and prBIND = 
   fn EqBind (S'1, S'2) => 
      prlayer [prID S'1, prstring (" = "), prE S'2]
   |  VarBind S'1 => prlayer [prE S'1]
   
   and prEXNDEC = 
   fn ExnAbbrev (S'1, S'2) => 
      prlayer [prID S'1, prstring (" = "), prCID S'2]
   |  ExnCase S'1 => prlayer [prCASE S'1]
   
   and prSTRUCT = 
   fn StrId S'1 => prlayer [prCID S'1]
   |  Struct S'1 => 
      prlayer [prstring ("struct "), prD S'1, prstring (" end ")]
   |  FuncApDec (S'1, S'2) => 
      prlayer [prSTRUCT S'1, prstring ("("), prD S'2, prstring (")")]
   |  FuncApStruct (S'1, S'2) => 
      prlayer
      [prSTRUCT S'1, prstring ("("), prSTRUCT S'2, prstring (")")]
   |  LetStruct (S'1, S'2) => 
      prlayer
      [prstring ("let "), prD S'1, prstring (" in "), prSTRUCT S'2, 
       prstring (" end")]
   
   and prFUNPAR = 
   fn OpenPar S'1  => prlayer [prlist prSPEC ("; ") S'1]
   |  ClosePar S'1 => prlayer [prSTRSPEC S'1]
   
   and prSIGOPT = 
   fn SomeSig S'1  => prlayer [prstring (":"), prSIG S'1]
   |  S'1 as NoSig => prlayer []
   
   and prREM = fn S'1 => prlayer [prstring S'1]
   
   and prA = fn S'1 => prlayer [proption prREM ("") S'1]
   
   and prDATA = 
   fn (S'1, S'2) => 
      prlayer [prT S'1, prstring (" = "), prlist prCASE (" |  ") S'2]
   
   and prFIELD = 
   fn (S'1, S'2) => prlayer [prID S'1, prstring (" : "), prT S'2]
   
   and prVS = 
   fn (S'1, S'2) => prlayer [prE S'1, prstring (" : "), prT S'2]
   
   and prTY = 
   fn (S'1, S'2, S'3, S'4) => 
      prlayer
      [prT S'1, prstring (" = "), prT S'2, prstring (" "), prA S'3, 
       prstring (" "), prCOM S'4]
   
   and prTYS = fn S'1 => prlayer [prlist prTY (" and ") S'1]
   
   and prDYS = fn S'1 => prlayer [prlist prDATA (" and ") S'1]
   
   and prEQN = 
   fn (S'1, S'2) => prlayer [prE S'1, prstring (" = "), prE S'2]
   
   and prFUN = fn S'1 => prlayer [prlist prEQN (" | ") S'1]
   
   and prMATCH = 
   fn (S'1, S'2) => prlayer [prE S'1, prstring (" => "), prE S'2]
   
   and prIDandE = 
   fn (S'1, S'2) => prlayer [prID S'1, prstring (" = "), prE S'2]
   
   and prSTRDEC = 
   fn (S'1, S'2, S'3) => 
      prlayer
      [prID S'1, prstring (" "), prSIGOPT S'2, prstring (" = "), 
       prSTRUCT S'3]
   
   and prSIGDEC = 
   fn (S'1, S'2) => prlayer [prID S'1, prstring (" = "), prSIG S'2]
   
   and prFUNCDEC = 
   fn (S'1, S'2, S'3, S'4) => 
      prlayer
      [prID S'1, prstring ("("), prFUNPAR S'2, prstring (") "), 
       prSIGOPT S'3, prstring (" = "), prSTRUCT S'4]
   
   and prSTRSPEC = 
   fn (S'1, S'2) => prlayer [prID S'1, prstring (" : "), prSIG S'2]
   
   and prSPECS = fn S'1 => prlayer [prlist prSPEC ("; ") S'1]
end;

structure Trees =
struct
   open Syntax;
   
   datatype TREE = 
       ID       of ID          (*[@1]*)
     | BIND     of BIND        (*[@1]*)
     | BINDS    of BIND list   (*[@@1]*)
     | CID      of CID         (*[@1]*)
     | SIGOPT   of SIGOPT      (*[@1]*)
     | DIR      of DIR         (*[@1]*)
     | CIDS     of CID list    (*[@ @1]*)
     | D        of D           (*[@1]*)
     | DS       of D list      (*[@@1]*)
     | SS       of SS          (*[@1]*)
     | SSS      of SS list     (*[@ @1]*)
     | TY       of TY          (*[@1]*)
     | TYS      of TYS         (*[@1]*)
     | DATA     of DATA        (*[@1]*)
     | DYS      of DYS         (*[@1]*)
     | T        of T           (*[@1]*)
     | E        of E           (*[@1]*)
     | ES       of E list      (*[@,@1]*)
     | FIELD    of FIELD       (*[@1]*)
     | FS       of FIELD list  (*[{@,@1}]*)
     | A        of A           (*[@1]*)
     | EQN      of EQN         (*[@1]*)
     | EQNS     of EQNS        (*[@1]*)
     | FUNS     of EQNS list   (*[@ and @1]*)
     | MATCH    of MATCH       (*[@1]*)
     | MATCHS   of MATCH list  (*[@@1]*)
     | LIT      of string      (*[@1]*)
     | CASE1    of CASE        (*[@1]*)
     | CASES    of CASE list   (*[@|@1]*)
     | IDS      of ID list     (*[@,@1]*)
     | TS       of T list      (*[@@1]*)
     | VS       of VS          (*[@1]*)
     | VSS      of VS list     (*[@ @1]*)
     | UNIT     of unit        (*[]*)
     | IDandE   of IDandE      (*[@1]*)
     | STR      of STRUCT      (*[@1]*)
     | FUNPAR   of FUNPAR      (*[@1]*)
     | FUNCDEC  of FUNCDEC     (*[@1]*)
     | FUNCDECS of FUNCDEC list(*[@ @1]*)
     | STRDEC   of STRDEC      (*[@1]*)
     | STRDECS  of STRDEC list (*[@ @1]*)
     | STRSPEC  of STRSPEC     (*[@1]*)
     | STRSPECS of STRSPEC list(*[@ @1]*)
     | EXNDEC   of EXNDEC      (*[@1]*)
     | EXNDECS  of EXNDEC list (*[@ @1]*)
     | SIG      of SIG         (*[@1]*)
     | SPEC     of SPEC        (*[@1]*)
     | SIGDEC   of SIGDEC      (*[@1]*)
     | SPECS    of SPEC list   (*[@ @1]*)
     | SIGDECS  of SIGDEC list (*[@ @1]*)
   withtype
       EQNS = EQN list(*[@ @1]*);
   
   val rec prTREE = 
   fn ID S'1       => prlayer [prID S'1]
   |  BIND S'1     => prlayer [prBIND S'1]
   |  BINDS S'1    => prlayer [prlist prBIND ("") S'1]
   |  CID S'1      => prlayer [prCID S'1]
   |  SIGOPT S'1   => prlayer [prSIGOPT S'1]
   |  DIR S'1      => prlayer [prDIR S'1]
   |  CIDS S'1     => prlayer [prlist prCID (" ") S'1]
   |  D S'1        => prlayer [prD S'1]
   |  DS S'1       => prlayer [prlist prD ("") S'1]
   |  SS S'1       => prlayer [prSS S'1]
   |  SSS S'1      => prlayer [prlist prSS (" ") S'1]
   |  TY S'1       => prlayer [prTY S'1]
   |  TYS S'1      => prlayer [prTYS S'1]
   |  DATA S'1     => prlayer [prDATA S'1]
   |  DYS S'1      => prlayer [prDYS S'1]
   |  T S'1        => prlayer [prT S'1]
   |  E S'1        => prlayer [prE S'1]
   |  ES S'1       => prlayer [prlist prE (",") S'1]
   |  FIELD S'1    => prlayer [prFIELD S'1]
   |  FS S'1 => 
      prlayer
      [prstring ("{"), prlist prFIELD (",") S'1, prstring ("}")]
   |  A S'1        => prlayer [prA S'1]
   |  EQN S'1      => prlayer [prEQN S'1]
   |  EQNS S'1     => prlayer [prEQNS S'1]
   |  FUNS S'1     => prlayer [prlist prEQNS (" and ") S'1]
   |  MATCH S'1    => prlayer [prMATCH S'1]
   |  MATCHS S'1   => prlayer [prlist prMATCH ("") S'1]
   |  LIT S'1      => prlayer [prstring S'1]
   |  CASE1 S'1    => prlayer [prCASE S'1]
   |  CASES S'1    => prlayer [prlist prCASE ("|") S'1]
   |  IDS S'1      => prlayer [prlist prID (",") S'1]
   |  TS S'1       => prlayer [prlist prT ("") S'1]
   |  VS S'1       => prlayer [prVS S'1]
   |  VSS S'1      => prlayer [prlist prVS (" ") S'1]
   |  UNIT S'1     => prlayer []
   |  IDandE S'1   => prlayer [prIDandE S'1]
   |  STR S'1      => prlayer [prSTRUCT S'1]
   |  FUNPAR S'1   => prlayer [prFUNPAR S'1]
   |  FUNCDEC S'1  => prlayer [prFUNCDEC S'1]
   |  FUNCDECS S'1 => prlayer [prlist prFUNCDEC (" ") S'1]
   |  STRDEC S'1   => prlayer [prSTRDEC S'1]
   |  STRDECS S'1  => prlayer [prlist prSTRDEC (" ") S'1]
   |  STRSPEC S'1  => prlayer [prSTRSPEC S'1]
   |  STRSPECS S'1 => prlayer [prlist prSTRSPEC (" ") S'1]
   |  EXNDEC S'1   => prlayer [prEXNDEC S'1]
   |  EXNDECS S'1  => prlayer [prlist prEXNDEC (" ") S'1]
   |  SIG S'1      => prlayer [prSIG S'1]
   |  SPEC S'1     => prlayer [prSPEC S'1]
   |  SIGDEC S'1   => prlayer [prSIGDEC S'1]
   |  SPECS S'1    => prlayer [prlist prSPEC (" ") S'1]
   |  SIGDECS S'1  => prlayer [prlist prSIGDEC (" ") S'1]
   
   and prEQNS = fn S'1 => prlayer [prlist prEQN (" ") S'1]
end;

