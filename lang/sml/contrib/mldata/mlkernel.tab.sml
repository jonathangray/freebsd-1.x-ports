(* Mllama Version 4.27 of Sept 3 1990
Compiled by: Standard ML of New Jersey, Version 0.56, 13 April 1990 *)
structure GETREDUCTION=
struct
exception PARSE_FAIL;
local
 val TOP_OF_PARSE_STACK=(fn S'1::_ => S'1|_=>raise PARSE_FAIL)
 open Trees;
 val REDUCTIONS=arrayoflist
 [TOP_OF_PARSE_STACK,
  TOP_OF_PARSE_STACK,
  TOP_OF_PARSE_STACK,
  (fn  S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn _ => LIT(CurrentNote())),
  (fn D S'2::LIT S'1::_ => D(mkNote CommentD S'1 S'2)|_=>raise PARSE_FAIL),
  (fn SPEC S'2::LIT S'1::_ => SPEC(mkNote CommentS S'1 S'2)|_=>raise PARSE_FAIL),
  (fn LIT S'2::T S'1::_ => T(mkNote TComment S'2 S'1)|_=>raise PARSE_FAIL),
  (fn DS S'1::_ => UNIT(process(mkD SeqDecs(rev S'1)))|_=>raise PARSE_FAIL),
  (fn E S'2::LIT S'1::_ => UNIT(process(mkD CommentD(S'1, ItDec S'2)))|_=>raise PARSE_FAIL),
  (fn ES S'3::_::LIT S'1::_ => UNIT(process(mkD CommentD(S'1, Import S'3)))|_=>raise PARSE_FAIL),
  (fn _::_:: S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  TOP_OF_PARSE_STACK,
  (fn D S'1::_ => DS([S'1])|_=>raise PARSE_FAIL),
  (fn D S'2::DS S'1::_ => DS(S'2 :: S'1)|_=>raise PARSE_FAIL),
  (fn _ => D(mkD SeqDecs [])),
  (fn DS S'1::_ => D(mkD SeqDecs(rev S'1))|_=>raise PARSE_FAIL),
  (fn D S'1::_ => DS([S'1])|_=>raise PARSE_FAIL),
  (fn D S'2::DS S'1::_ => DS(S'2 :: S'1)|_=>raise PARSE_FAIL),
  (fn _:: S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn DYS S'2::_::_ => D((mkD Data(rev S'2)))|_=>raise PARSE_FAIL),
  (fn TYS S'4::_::DYS S'2::_::_ => D((mkD With(rev S'2, rev S'4)))|_=>raise PARSE_FAIL),
  (fn _::D S'4::_::DYS S'2::_::_ => D((mkD AbsData(rev S'2, S'4)))|_=>raise PARSE_FAIL),
  (fn _::D S'6::_::TYS S'4::_::DYS S'2::_::_ => D((mkD AbsWith(rev S'2, rev S'4, S'6)))|_=>raise PARSE_FAIL),
  (fn TYS S'2::_::_ => D((mkD Type(rev S'2)))|_=>raise PARSE_FAIL),
  (fn FUNS S'2::_::_ => D(mkD FunDecs((rev S'2)))|_=>raise PARSE_FAIL),
  (fn EQNS S'2::_::_ => D(mkD ValDecs((rev S'2)))|_=>raise PARSE_FAIL),
  (fn EQNS S'3::_::_::_ => D(mkD RecDecs((rev S'3)))|_=>raise PARSE_FAIL),
  (fn EXNDECS S'2::_::_ => D(mkD ExnDecs(rev S'2))|_=>raise PARSE_FAIL),
  (fn STRDECS S'2::_::_ => D(mkD StrDecs(rev S'2))|_=>raise PARSE_FAIL),
  (fn FUNCDECS S'2::_::_ => D(mkD FuncDecs(rev S'2))|_=>raise PARSE_FAIL),
  (fn SIGDECS S'2::_::_ => D(mkD SigDecs(rev S'2))|_=>raise PARSE_FAIL),
  (fn _::D S'4::_::D S'2::_::_ => D(mkD LocDecs(S'2, S'4))|_=>raise PARSE_FAIL),
  (fn DIR S'1::_ => D(DirDec S'1)|_=>raise PARSE_FAIL),
  (fn CIDS S'2::_::_ => DIR(Open(rev S'2))|_=>raise PARSE_FAIL),
  (fn ES S'2::_::_ => DIR(Nonfix(rev S'2))|_=>raise PARSE_FAIL),
  (fn ES S'2::_::_ => DIR(Infix(S'2))|_=>raise PARSE_FAIL),
  (fn ES S'2::_::_ => DIR(Include(S'2))|_=>raise PARSE_FAIL),
  (fn ES S'2::_::_ => DIR(Infixr(S'2))|_=>raise PARSE_FAIL),
  (fn STRDEC S'1::_ => STRDECS([S'1])|_=>raise PARSE_FAIL),
  (fn STRDEC S'3::_::STRDECS S'1::_ => STRDECS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn FUNCDEC S'1::_ => FUNCDECS([S'1])|_=>raise PARSE_FAIL),
  (fn FUNCDEC S'3::_::FUNCDECS S'1::_ => FUNCDECS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn SIGDEC S'1::_ => SIGDECS([S'1])|_=>raise PARSE_FAIL),
  (fn SIGDEC S'3::_::SIGDECS S'1::_ => SIGDECS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn DATA S'1::_ => DYS([S'1])|_=>raise PARSE_FAIL),
  (fn DATA S'3::_::DYS S'1::_ => DYS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn TY S'1::_ => TYS([S'1])|_=>raise PARSE_FAIL),
  (fn TY S'3::_::TYS S'1::_ => TYS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn EQNS S'1::_ => FUNS([(rev S'1)])|_=>raise PARSE_FAIL),
  (fn EQNS S'3::_::FUNS S'1::_ => FUNS(rev S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn EQN S'1::_ => EQNS([S'1])|_=>raise PARSE_FAIL),
  (fn EQN S'3::_::EQNS S'1::_ => EQNS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn EXNDEC S'1::_ => EXNDECS([S'1])|_=>raise PARSE_FAIL),
  (fn EXNDEC S'3::_::EXNDECS S'1::_ => EXNDECS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn STR S'4::_::SIGOPT S'2::ID S'1::_ => STRDEC(S'1, S'2, S'4)|_=>raise PARSE_FAIL),
  (fn STR S'7::_::SIGOPT S'5::_::FUNPAR S'3::_::ID S'1::_ => FUNCDEC(S'1, S'3, S'5, S'7)|_=>raise PARSE_FAIL),
  (fn SIG S'2::_::_ => SIGOPT(SomeSig S'2)|_=>raise PARSE_FAIL),
  (fn _ => SIGOPT(NoSig)),
  (fn SIG S'3::_::ID S'1::_ => SIGDEC(S'1, S'3)|_=>raise PARSE_FAIL),
  (fn SIG S'3::_::ID S'1::_ => FUNPAR(ClosePar(S'1, S'3))|_=>raise PARSE_FAIL),
  (fn _ => FUNPAR(OpenPar[])),
  (fn SPECS S'1::_ => FUNPAR(OpenPar(rev S'1))|_=>raise PARSE_FAIL),
  (fn _::SPECS S'2::_::_ => SIG(Sig(rev S'2))|_=>raise PARSE_FAIL),
  (fn CID S'1::_ => SIG(SigId S'1)|_=>raise PARSE_FAIL),
  (fn SPEC S'1::_ => SPECS([S'1])|_=>raise PARSE_FAIL),
  (fn SPEC S'2::SPECS S'1::_ => SPECS(S'2 :: S'1)|_=>raise PARSE_FAIL),
  (fn _:: S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn VSS S'2::_::_ => SPEC(ValSpecs(rev S'2))|_=>raise PARSE_FAIL),
  (fn TS S'2::_::_ => SPEC(TypeSpecs(rev S'2))|_=>raise PARSE_FAIL),
  (fn TS S'2::_::_ => SPEC(EqTypeSpecs(rev S'2))|_=>raise PARSE_FAIL),
  (fn STRSPECS S'2::_::_ => SPEC(StructSpecs(rev S'2))|_=>raise PARSE_FAIL),
  (fn DYS S'2::_::_ => SPEC(DataSpecs(rev S'2))|_=>raise PARSE_FAIL),
  (fn CASES S'2::_::_ => SPEC(ExnSpecs(rev S'2))|_=>raise PARSE_FAIL),
  (fn SSS S'2::_::_ => SPEC(ShareSpecs(rev S'2))|_=>raise PARSE_FAIL),
  (fn _::SPECS S'4::_::SPECS S'2::_::_ => SPEC(LocSpecs(rev S'2, rev S'4))|_=>raise PARSE_FAIL),
  (fn DIR S'1::_ => SPEC(DirSpec S'1)|_=>raise PARSE_FAIL),
  (fn VS S'1::_ => VSS([S'1])|_=>raise PARSE_FAIL),
  (fn VS S'3::_::VSS S'1::_ => VSS(S'3::S'1)|_=>raise PARSE_FAIL),
  (fn CASE1 S'1::_ => CASES([S'1])|_=>raise PARSE_FAIL),
  (fn CASE1 S'3::_::CASES S'1::_ => CASES(S'3::S'1)|_=>raise PARSE_FAIL),
  (fn T S'1::_ => TS([S'1])|_=>raise PARSE_FAIL),
  (fn T S'3::_::TS S'1::_ => TS(S'3::S'1)|_=>raise PARSE_FAIL),
  (fn STRSPEC S'1::_ => STRSPECS([S'1])|_=>raise PARSE_FAIL),
  (fn STRSPEC S'3::_::STRSPECS S'1::_ => STRSPECS(S'3::S'1)|_=>raise PARSE_FAIL),
  (fn SS S'1::_ => SSS([S'1])|_=>raise PARSE_FAIL),
  (fn SS S'3::_::SSS S'1::_ => SSS(S'3::S'1)|_=>raise PARSE_FAIL),
  (fn T S'3::_::E S'1::_ => VS(S'1, S'3)|_=>raise PARSE_FAIL),
  (fn SIG S'3::_::ID S'1::_ => STRSPEC(S'1, S'3)|_=>raise PARSE_FAIL),
  (fn CIDS S'2::_::_ => SS(ShareTy(rev S'2))|_=>raise PARSE_FAIL),
  (fn CIDS S'1::_ => SS(ShareStr(rev S'1))|_=>raise PARSE_FAIL),
  (fn CID S'3::_::CID S'1::_ => CIDS([S'3, S'1])|_=>raise PARSE_FAIL),
  (fn CID S'3::_::CIDS S'1::_ => CIDS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn _::D S'2::_::_ => STR(Struct(S'2))|_=>raise PARSE_FAIL),
  (fn _::STR S'4::_::D S'2::_::_ => STR(LetStruct(S'2, S'4))|_=>raise PARSE_FAIL),
  (fn CID S'1::_ => STR(StrId S'1)|_=>raise PARSE_FAIL),
  (fn _::STR S'3::_::STR S'1::_ => STR(FuncApStruct(S'1, S'3))|_=>raise PARSE_FAIL),
  (fn _::D S'3::_::STR S'1::_ => STR(FuncApDec(S'1, S'3))|_=>raise PARSE_FAIL),
  (fn CASES S'3::_::T S'1::_ => DATA(S'1, rev S'3)|_=>raise PARSE_FAIL),
  (fn CASE1 S'1::_ => CASES([S'1])|_=>raise PARSE_FAIL),
  (fn CASE1 S'3::_::CASES S'1::_ => CASES(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn LIT S'5::A S'4::T S'3::_::ID S'1::_ => CASE1(Construct(S'1, S'3, S'4, mkCom S'5))|_=>raise PARSE_FAIL),
  (fn LIT S'3::A S'2::ID S'1::_ => CASE1(Constant(S'1, S'2, mkCom  S'3))|_=>raise PARSE_FAIL),
  (fn LIT S'5::A S'4::T S'3::_::T S'1::_ => TY(S'1,S'3,S'4, mkCom  S'5)|_=>raise PARSE_FAIL),
  (fn  S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn T S'3::_::T S'1::_ => T(mkT Fun(S'1, S'3))|_=>raise PARSE_FAIL),
  (fn TS S'3::_::T S'1::_ => T(mkT Prod(S'1 :: rev S'3))|_=>raise PARSE_FAIL),
  (fn T S'1::_ => TS([S'1])|_=>raise PARSE_FAIL),
  (fn T S'3::_::TS S'1::_ => TS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn CID S'1::_ => T(Tid S'1)|_=>raise PARSE_FAIL),
  (fn CID S'2::T S'1::_ => T(mkT Inst(S'1, Tid S'2))|_=>raise PARSE_FAIL),
  (fn _::TS S'2::_::_ => T(mkT Bra(rev S'2))|_=>raise PARSE_FAIL),
  (fn _::FS S'2::_::_ => T(mkT Record(rev S'2))|_=>raise PARSE_FAIL),
  (fn T S'1::_ => TS([S'1])|_=>raise PARSE_FAIL),
  (fn T S'3::_::TS S'1::_ => TS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn FIELD S'1::_ => FS([S'1])|_=>raise PARSE_FAIL),
  (fn FIELD S'3::_::FS S'1::_ => FS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn T S'3::_::ID S'1::_ => FIELD(S'1, S'3)|_=>raise PARSE_FAIL),
  (fn  S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn  S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn _ => A(NONE)),
  (fn ID S'1::_ => A(SOME S'1)|_=>raise PARSE_FAIL),
  (fn CASE1 S'1::_ => EXNDEC(ExnCase S'1)|_=>raise PARSE_FAIL),
  (fn CID S'3::_::ID S'1::_ => EXNDEC(ExnAbbrev(S'1, S'3))|_=>raise PARSE_FAIL),
  (fn EQN S'1::_ => EQNS([S'1])|_=>raise PARSE_FAIL),
  (fn EQN S'3::_::EQNS S'1::_ => EQNS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn E S'3::_::E S'1::_ => EQN(S'1, S'3)|_=>raise PARSE_FAIL),
  (fn E S'3::_::E S'1::_ => EQN(S'1, S'3)|_=>raise PARSE_FAIL),
  (fn  S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn T S'3::_::E S'1::_ => E(Has(S'1, S'3))|_=>raise PARSE_FAIL),
  (fn  S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn T S'3::_::E S'1::_ => E(Has(S'1, S'3))|_=>raise PARSE_FAIL),
  (fn E S'3::_::E S'1::_ => E(As(S'1, S'3))|_=>raise PARSE_FAIL),
  (fn ES S'1::_ => E(mkE Expression(rev S'1))|_=>raise PARSE_FAIL),
  (fn E S'1::_ => ES([S'1])|_=>raise PARSE_FAIL),
  (fn E S'2::ES S'1::_ => ES(S'2::S'1)|_=>raise PARSE_FAIL),
  (fn  S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn  S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn _::ES S'2::_::_ => E(mkE Tuple(rev S'2))|_=>raise PARSE_FAIL),
  (fn _::_::_ => E(mkE Tuple[])|_=>raise PARSE_FAIL),
  (fn _::ES S'2::_::_ => E(mkE List(rev S'2))|_=>raise PARSE_FAIL),
  (fn _::_::_ => E(List[])|_=>raise PARSE_FAIL),
  (fn _::BINDS S'2::_::_ => E(Rec(rev S'2))|_=>raise PARSE_FAIL),
  (fn _::_::_ => E(Rec[])|_=>raise PARSE_FAIL),
  (fn MATCH S'1::_ => MATCHS([S'1])|_=>raise PARSE_FAIL),
  (fn MATCH S'3::_::MATCHS S'1::_ => MATCHS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn E S'3::_::E S'1::_ => MATCH(S'1, S'3)|_=>raise PARSE_FAIL),
  (fn ES S'1::_ => E(mkE Expression(rev S'1))|_=>raise PARSE_FAIL),
  (fn  S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn E S'2::ES S'1::_ => E(mkE Expression(rev(S'2::S'1)))|_=>raise PARSE_FAIL),
  (fn T S'3::_::ES S'1::_ => E(mkE Has(Expression(rev S'1), S'3))|_=>raise PARSE_FAIL),
  (fn MATCHS S'3::_::E S'1::_ => E(mkE Handle(S'1, rev S'3))|_=>raise PARSE_FAIL),
  (fn E S'3::_::E S'1::_ => E(mkE AndAlso(S'1, S'3))|_=>raise PARSE_FAIL),
  (fn E S'3::_::E S'1::_ => E(mkE OrElse(S'1, S'3))|_=>raise PARSE_FAIL),
  (fn E S'6::_::E S'4::_::E S'2::_::_ => E(mkE If(S'2, S'4, S'6))|_=>raise PARSE_FAIL),
  (fn E S'4::_::E S'2::_::_ => E(mkE While(S'2, S'4))|_=>raise PARSE_FAIL),
  (fn MATCHS S'4::_::E S'2::_::_ => E(mkE Case(S'2, rev S'4))|_=>raise PARSE_FAIL),
  (fn MATCHS S'2::_::_ => E(mkE Fn(rev S'2))|_=>raise PARSE_FAIL),
  (fn E S'2::_::_ => E(mkE Raise S'2)|_=>raise PARSE_FAIL),
  (fn E S'1::_ => ES([S'1])|_=>raise PARSE_FAIL),
  (fn E S'1::_ => ES([S'1])|_=>raise PARSE_FAIL),
  (fn E S'2::ES S'1::_ => ES(S'2 :: S'1)|_=>raise PARSE_FAIL),
  (fn E S'2::ES S'1::_ => ES(S'2 :: S'1)|_=>raise PARSE_FAIL),
  (fn ID S'2::_::_ => E(Op S'2)|_=>raise PARSE_FAIL),
  (fn CID S'3::_::ID S'1::_ => E(Id(S'1 :: rev S'3))|_=>raise PARSE_FAIL),
  (fn  S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn _::ES S'4::_::D S'2::_::_ => E(mkE Let(S'2, rev S'4))|_=>raise PARSE_FAIL),
  (fn _::ES S'4::_::E S'2::_::_ => E(mkE Tuple[Seq(S'2 :: rev S'4)])|_=>raise PARSE_FAIL),
  (fn _::ES S'4::_::E S'2::_::_ => E(mkE Tuple(S'2 :: rev S'4))|_=>raise PARSE_FAIL),
  (fn _::E S'2::_::_ => E(mkE Tuple[S'2])|_=>raise PARSE_FAIL),
  (fn _::_::_ => E(Tuple[])|_=>raise PARSE_FAIL),
  (fn _::ES S'2::_::_ => E(mkE List(rev S'2))|_=>raise PARSE_FAIL),
  (fn _::_::_ => E(List[])|_=>raise PARSE_FAIL),
  (fn _::BINDS S'2::_::_ => E(Rec(rev S'2))|_=>raise PARSE_FAIL),
  (fn _::_::_ => E(Rec[])|_=>raise PARSE_FAIL),
  (fn E S'3::_::ID S'1::_ => BIND(EqBind(S'1, S'3))|_=>raise PARSE_FAIL),
  (fn E S'3::_::ID S'1::_ => BIND(EqBind(S'1, S'3))|_=>raise PARSE_FAIL),
  (fn E S'1::_ => BIND(VarBind S'1)|_=>raise PARSE_FAIL),
  (fn BIND S'1::_ => BINDS([S'1])|_=>raise PARSE_FAIL),
  (fn BIND S'3::_::BINDS S'1::_ => BINDS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn BIND S'1::_ => BINDS([S'1])|_=>raise PARSE_FAIL),
  (fn BIND S'3::_::BINDS S'1::_ => BINDS(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn E S'1::_ => ES([S'1])|_=>raise PARSE_FAIL),
  (fn E S'3::_::ES S'1::_ => ES(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn E S'1::_ => ES([S'1])|_=>raise PARSE_FAIL),
  (fn E S'3::_::ES S'1::_ => ES(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn E S'1::_ => ES([S'1])|_=>raise PARSE_FAIL),
  (fn E S'3::_::ES S'1::_ => ES(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn CID S'1::_ => CIDS([S'1])|_=>raise PARSE_FAIL),
  (fn CID S'2::CIDS S'1::_ => CIDS(S'2 :: S'1)|_=>raise PARSE_FAIL),
  (fn CID S'1::_ => E(Id S'1)|_=>raise PARSE_FAIL),
  (fn ID S'2::_::_ => E(Op S'2)|_=>raise PARSE_FAIL),
  (fn ID S'1::_ => E(Id [S'1])|_=>raise PARSE_FAIL),
  (fn ID S'1::_ => CID([S'1])|_=>raise PARSE_FAIL),
  (fn ID S'3::_::CID S'1::_ => CID(S'3 :: S'1)|_=>raise PARSE_FAIL),
  (fn  S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn _::_ => ID("*")|_=>raise PARSE_FAIL),
  (fn _::_ => ID("=")|_=>raise PARSE_FAIL),
  (fn _::_ => ID("->")|_=>raise PARSE_FAIL),
  (fn CID S'1::_ => CID(rev S'1)|_=>raise PARSE_FAIL),
  (fn _::_ => E(Id ["*"])|_=>raise PARSE_FAIL),
  (fn _::_ => E(Id ["="])|_=>raise PARSE_FAIL),
  (fn ID S'1::_ => E(String S'1)|_=>raise PARSE_FAIL),
  (fn ID S'1::_ => E(Num S'1)|_=>raise PARSE_FAIL),
  (fn E S'1::_ => ES([S'1])|_=>raise PARSE_FAIL),
  (fn E S'2::ES S'1::_ => ES(S'2 :: S'1)|_=>raise PARSE_FAIL),
  (fn ES S'2::ID S'1::_ => ES(Num S'1 :: rev S'2)|_=>raise PARSE_FAIL),
  (fn ES S'1::_ => ES(rev S'1)|_=>raise PARSE_FAIL),
  (fn ID S'1::_ => E(Id [S'1])|_=>raise PARSE_FAIL),
  (fn  S'1::_ => (S'1)|_=>raise PARSE_FAIL),
  (fn ID S'1::_ => ES([String S'1])|_=>raise PARSE_FAIL),
  (fn ES S'2::ID S'1::_ => ES(String S'1 :: S'2)|_=>raise PARSE_FAIL),
  (fn _ => ES([] (* @(#)mlkernel.mlg	2.5 93/02/05 15:03:45 *)))]
in
 fun GETREDUCTION prod_' = REDUCTIONS sub prod_'
end;
end;

structure GETGOTO=
struct
local open LR
fun Row(size_', pairs_')=
let val r_'=array(size_',0)
in List.app (fn(st_',st_'')=>update(r_',st_',st_'')) pairs_'; r_' end;
val GOTOS=arrayoflist
[Row(1,[]),
 Row(1,[]),
 Row(1,[]),
 Row(2,[(1,2)]),
 Row(8,[(1,3),(7,51)]),
 Row(380,[(1,4),(6,49),(7,4),(22,49),(29,49),(111,49),(152,49),(176,245),
    (195,264),(196,49),(248,245),(250,245),(286,321),(287,49),(300,245),(307,245),
    (308,49),(309,49),(312,348),(314,349),(341,245),(347,49),(364,245),(379,245)]),
 Row(348,[(1,5),(6,50),(7,5),(22,109),(29,109),(111,198),(152,109),(196,109),
    (287,109),(308,109),(309,109),(347,109)]),
 Row(50,[(4,40),(49,40)]),
 Row(380,[(176,246),(248,306),(250,246),(300,246),(307,306),(341,306),(364,246),(379,306)]),
 Row(246,[(245,302)]),
 Row(356,[(226,285),(355,371)]),
 Row(356,[(54,145),(140,219),(173,242),(185,255),(194,263),(200,267),(220,280),(223,283),
    (226,286),(355,286)]),
 Row(8,[(1,6),(7,6)]),
 Row(318,[(4,41),(24,115),(25,116),(26,117),(28,121),(30,124),(31,126),(138,217),
    (139,218),(174,243),(186,256),(201,268),(202,269),(204,271),(206,273),(208,126),
    (209,273),(211,277),(212,278),(315,350),(317,351)]),
 Row(53,[(8,53),(52,144)]),
 Row(348,[(22,110),(29,122),(152,228),(196,265),(287,322),(308,345),(309,346),(347,368)]),
 Row(348,[(22,111),(29,111),(152,111),(196,111),(287,111),(308,111),(309,111),(347,111)]),
 Row(298,[(9,57),(20,103),(297,333)]),
 Row(191,[(23,112),(151,227),(190,260)]),
 Row(11,[(10,66)]),
 Row(97,[(18,97),(96,183)]),
 Row(22,[(21,105)]),
 Row(20,[(19,101)]),
 Row(12,[(11,78)]),
 Row(13,[(12,81)]),
 Row(246,[(4,42),(49,42),(245,303)]),
 Row(14,[(13,83)]),
 Row(92,[(14,88),(15,92),(16,94),(17,92),(91,182)]),
 Row(18,[(15,93),(17,95)]),
 Row(190,[(19,102),(189,259)]),
 Row(178,[(11,79),(177,249)]),
 Row(180,[(12,82),(179,253)]),
 Row(298,[(9,58),(20,58),(153,229),(297,58)]),
 Row(200,[(23,113),(151,113),(190,113),(199,266)]),
 Row(171,[(10,67),(170,239)]),
 Row(185,[(18,98),(96,98),(184,254)]),
 Row(193,[(21,106),(192,262)]),
 Row(305,[(100,188),(304,343)]),
 Row(368,[(258,310),(347,369),(365,380),(367,381)]),
 Row(177,[(176,247)]),
 Row(358,[(178,251),(187,257),(293,324),(357,373)]),
 Row(365,[(176,248),(250,307),(300,341),(364,379)]),
 Row(373,[(9,59),(10,68),(13,84),(18,68),(20,59),(23,59),(27,68),(54,59),
    (60,155),(62,68),(63,68),(64,68),(74,68),(83,180),(96,68),(114,155),
    (137,68),(140,59),(146,155),(151,59),(153,59),(170,68),(171,68),(172,68),
    (173,59),(178,252),(184,68),(185,59),(187,252),(190,59),(191,261),(194,59),
    (199,59),(200,59),(203,68),(205,68),(220,59),(221,59),(223,59),(226,59),
    (234,68),(236,68),(238,68),(258,311),(281,155),(293,252),(294,68),(295,59),
    (297,59),(299,337),(301,59),(320,59),(329,155),(336,337),(347,311),(352,155),
    (354,68),(355,59),(356,59),(357,252),(361,376),(362,337),(363,378),(365,311),
    (367,311),(372,155)]),
 Row(295,[(294,325)]),
 Row(302,[(295,328),(301,342)]),
 Row(297,[(296,331)]),
 Row(299,[(298,334)]),
 Row(300,[(299,338)]),
 Row(355,[(294,326),(354,370)]),
 Row(360,[(21,107),(154,230),(192,107),(289,323),(298,335),(359,375)]),
 Row(357,[(9,60),(20,60),(23,114),(54,146),(140,146),(151,114),(153,60),(173,146),
    (185,146),(190,114),(194,146),(199,114),(200,146),(220,146),(221,281),(223,146),
    (226,146),(295,329),(297,60),(301,329),(320,352),(355,146),(356,372)]),
 Row(359,[(296,332),(358,374)]),
 Row(363,[(299,339),(362,377)]),
 Row(355,[(10,69),(18,69),(27,69),(62,69),(63,69),(64,69),(74,69),(96,69),
    (137,69),(170,69),(171,69),(172,69),(184,69),(203,69),(205,69),(234,69),
    (236,69),(238,69),(294,327),(354,327)]),
 Row(363,[(299,340),(336,360),(362,340)]),
 Row(155,[(154,231)]),
 Row(360,[(4,43),(21,108),(24,43),(25,43),(26,43),(28,43),(30,43),(31,43),
    (33,135),(45,43),(65,169),(138,43),(139,43),(154,108),(156,232),(174,43),
    (186,43),(192,108),(201,43),(202,43),(204,43),(206,43),(208,43),(209,43),
    (211,43),(212,43),(289,108),(298,108),(315,43),(317,43),(359,108)]),
 Row(268,[(108,195),(263,312),(267,314)]),
 Row(222,[(221,282)]),
 Row(55,[(54,147)]),
 Row(56,[(55,148)]),
 Row(226,[(55,149),(225,284)]),
 Row(239,[(32,131),(55,150),(64,165),(214,131),(225,150),(238,165)]),
 Row(172,[(10,70),(170,70),(171,240)]),
 Row(239,[(10,71),(18,99),(27,118),(62,158),(63,158),(64,166),(96,99),(137,118),
    (170,71),(171,71),(172,241),(184,99),(203,118),(205,118),(234,290),(236,291),
    (238,166)]),
 Row(172,[(10,72),(170,72),(171,72)]),
 Row(239,[(10,73),(18,73),(27,73),(62,73),(63,73),(64,73),(96,73),(137,73),
    (170,73),(171,73),(172,73),(184,73),(203,73),(205,73),(234,73),(236,73),
    (238,73)]),
 Row(239,[(10,74),(18,74),(27,74),(62,74),(63,74),(64,74),(96,74),(137,74),
    (170,74),(171,74),(172,74),(184,74),(203,74),(205,74),(234,74),(236,74),
    (238,74)]),
 Row(239,[(10,75),(18,75),(27,75),(62,75),(63,75),(64,75),(74,175),(96,75),
    (137,75),(170,75),(171,75),(172,75),(184,75),(203,75),(205,75),(234,75),
    (236,75),(238,75)]),
 Row(318,[(4,44),(10,76),(18,76),(24,44),(25,44),(26,44),(27,76),(28,44),
    (30,44),(31,44),(45,44),(62,76),(63,76),(64,76),(74,76),(96,76),
    (137,76),(138,44),(139,44),(170,76),(171,76),(172,76),(174,44),(184,76),
    (186,44),(201,44),(202,44),(203,76),(204,44),(205,76),(206,44),(208,44),
    (209,44),(211,44),(212,44),(234,76),(236,76),(238,76),(315,44),(317,44)]),
 Row(64,[(62,159),(63,161)]),
 Row(65,[(64,167)]),
 Row(204,[(27,119),(137,216),(203,270)]),
 Row(206,[(27,120),(137,120),(203,120),(205,272)]),
 Row(318,[(4,45),(24,45),(25,45),(26,45),(28,45),(30,45),(31,45),(138,45),
    (139,45),(174,45),(186,45),(201,45),(202,45),(204,45),(206,45),(208,45),
    (209,45),(211,45),(212,45),(315,45),(317,45)]),
 Row(318,[(4,46),(24,46),(25,46),(26,46),(28,46),(30,46),(31,46),(45,141),
    (138,46),(139,46),(174,46),(186,46),(201,46),(202,46),(204,46),(206,46),
    (208,46),(209,46),(211,46),(212,46),(315,46),(317,46)]),
 Row(318,[(4,47),(24,47),(25,47),(26,47),(28,47),(30,47),(31,47),(45,142),
    (138,47),(139,47),(174,47),(186,47),(201,47),(202,47),(204,47),(206,47),
    (208,47),(209,47),(211,47),(212,47),(315,47),(317,47)]),
 Row(318,[(4,48),(24,48),(25,48),(26,48),(28,48),(30,48),(31,48),(45,143),
    (138,48),(139,48),(174,48),(186,48),(201,48),(202,48),(204,48),(206,48),
    (208,48),(209,48),(211,48),(212,48),(315,48),(317,48)]),
 Row(373,[(9,61),(10,61),(13,61),(18,61),(20,61),(23,61),(27,61),(54,61),
    (60,61),(62,61),(63,61),(64,61),(74,61),(83,61),(96,61),(114,61),
    (136,215),(137,61),(140,61),(146,61),(151,61),(153,61),(170,61),(171,61),
    (172,61),(173,61),(178,61),(184,61),(185,61),(187,61),(190,61),(191,61),
    (194,61),(199,61),(200,61),(203,61),(205,61),(220,61),(221,61),(223,61),
    (226,61),(234,61),(236,61),(238,61),(258,61),(281,61),(293,61),(294,61),
    (295,61),(297,61),(299,61),(301,61),(320,61),(329,61),(336,61),(347,61),
    (352,61),(354,61),(355,61),(356,61),(357,61),(361,61),(362,61),(363,61),
    (365,61),(367,61),(372,61)]),
 Row(210,[(206,274),(209,276)]),
 Row(209,[(31,127),(208,275)]),
 Row(33,[(32,132)]),
 Row(215,[(32,133),(214,279)]),
 Row(239,[(64,168),(238,292)]),
 Row(183,[(14,89),(15,89),(16,89),(17,89),(88,89),(91,89),(92,89),(94,89),
    (182,89)]),
 Row(183,[(14,90),(15,90),(16,90),(17,90),(88,181),(91,90),(92,181),(94,181),
    (182,181)]),
 Row(1,[])]
in
 fun GETGOTO(st_',sym_') = (GOTOS sub (sym_'-62)) sub st_'
end;
end;

structure GETRECOVER=
struct
fun GETRECOVER(st,sym) = false;
val ERRORNONTERMINAL = 2;
end;

structure GETACTION=
struct
(* Parse-action tables are read-in at compile time *)
local open LR 
(* Decoding functions *)
val numterms = 62;
fun decode n =
(case n mod 4 of
  0=>ACCEPT
| 1=>ERROR
| 2=>SHIFT(n div 4)
| 3=>REDUCE(n div 4 mod 2048,numterms+n div 8192 mod 2048,n div 16777216)
| _=>ERROR);
fun UpdateRow r (syms, act) =
let val act=decode act in map (fn(sym)=>update(r, sym, act)) syms end;
fun C pairs =
let val r=array(1+numterms, ERROR) in List.app (UpdateRow r) pairs; r end;
fun S pairs =
let val r = array(1+numterms, ERROR) in
    List.app (fn(sym, act)=>update(r, sym, decode act)) pairs; r
end;
 val tabledata = open_in "mlkernel.tab.data"
fun RL RF n = if n=0 then [] else RF ()::RL RF (n-1);
fun RN n  = 
 case lookahead tabledata of
   "0" => (inputc tabledata 1; RN  (n*10))
|  "1" => (inputc tabledata 1; RN  (n*10+1))
|  "2" => (inputc tabledata 1; RN  (n*10+2))
|  "3" => (inputc tabledata 1; RN  (n*10+3))
|  "4" => (inputc tabledata 1; RN  (n*10+4))
|  "5" => (inputc tabledata 1; RN  (n*10+5))
|  "6" => (inputc tabledata 1; RN  (n*10+6))
|  "7" => (inputc tabledata 1; RN  (n*10+7))
|  "8" => (inputc tabledata 1; RN  (n*10+8))
|  "9" => (inputc tabledata 1; RN  (n*10+9))
|  _     => n;
fun RNUM ()   = 
 case lookahead tabledata of
   "0" => RN 0
|  "1" => RN 0
|  "2" => RN 0
|  "3" => RN 0
|  "4" => RN 0
|  "5" => RN 0
|  "6" => RN 0
|  "7" => RN 0
|  "8" => RN 0
|  "9" => RN 0
|  _     => (inputc tabledata 1; RNUM ());
fun RCL() = (RL RNUM (RNUM ()), RNUM ());
fun RCST() = RL RCL  (RNUM ());
fun RSST() = RL (fn () =>(RNUM (),RNUM ()))  (RNUM ());
fun R() = RL (fn()=>case RNUM () of 0=>C(RCST())|_=>S(RSST()))(RNUM());
 val ACTION=arrayoflist (C[] :: R ())
 val _=close_in tabledata
in
 fun GETACTION(st_', sy_')=(ACTION sub st_') sub sy_';
end;
end;

structure PARSER=
struct
structure LR=LR
structure GETREDUCTION = GETREDUCTION
structure GETACTION = GETACTION
structure GETGOTO = GETGOTO
structure GETRECOVER = GETRECOVER
val PARSE = LR.parse GETACTION.GETACTION GETGOTO.GETGOTO GETRECOVER.GETRECOVER GETRECOVER.ERRORNONTERMINAL
     SYMBOLS.llEND GETREDUCTION.GETREDUCTION SYMBOLS.SYMBOL_NAMES ;
end;

