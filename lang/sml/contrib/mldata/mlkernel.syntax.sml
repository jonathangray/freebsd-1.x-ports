(*
	SML superficial abstract syntax
	@(#)mlkernel.syntax.sml	2.3 93/02/05 15:03:46

	This file is used to generate mldata.write.sml, which
	is compiled as part of the prettyprinter.

	The comments are mldata-style formatting instructions,
	used to generate the horizontal-mode printers for SML.

	The vertical-mode printers are (at present) hand crafted.
*)



structure Syntax =
struct

   type ID = string(*[@1]*);

   type CID = string list(*[@.@1]*);

   datatype COM = NoComment (*[]*)  |  SomeComment of string (*[(*@1*)]*) ;

   datatype D =
       Dnum     of D serial     (*[$prserial prD Dtext$1]*)
     | CommentD of string * D   (*[@1 @2]*)
     | Comment  of string       (*[@1]*)
     | Type     of TYS          (*[type @1]*)
     | Data     of DYS          (*[datatype @1]*)
     | With     of DYS * TYS    (*[datatype @1 withtype @2]*)
     | AbsData  of DYS * D      (*[abstype @1 with @2 end]*)
     | AbsWith  of DYS * TYS * D(*[abstype @1 withtype @2 with @3 end]*)
     | FunDecs  of FUN list     (*[fun @ and @1]*)
     | ValDecs  of EQN list     (*[val @ and @1]*)
     | RecDecs  of EQN list     (*[val rec @ and @1]*)
     | ExnDecs  of EXNDEC list  (*[exception @ and @1]*)
     | StrDecs  of STRDEC list  (*[structure @ and @1]*)
     | FuncDecs of FUNCDEC list (*[functor @ and @1]*)
     | SigDecs  of SIGDEC list  (*[signature @ and @1]*)
     | SeqDecs  of D list       (*[@; @1]*)
     | LocDecs  of D * D        (*[local @1 in @2 end]*)
     | DirDec   of DIR          (*[@1]*)
     | ItDec    of E            (*[@1]*)
     | Import   of E list       (*[import @ @1]*)

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

   and BIND = EqBind of ID * E (*[@1 = @2]*)  |  VarBind of E (*[@1]*)

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

   fun printExpr e = ! ExprPrinter e
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
       EQNS = EQN list(*[@ @1]*)
end;

