(*
	Prettyprinting functions for mldata

	%W% %E% %T%
*)

structure prettybase =
struct
   open Trees;

   val rm = ref 80;

   val pos = ref 0;

   fun Pos () = ! pos;

   val ind = ref 0;

   val vert = ref false;

   val inLet = ref false;

   fun Horizontal () = ! vert = false;

   val inds = ref ([] : int list);

   val verts = ref ([] : bool list);

   fun Setting (r as ref n) n' f x = (r := n'; f x; r := n);

   fun WithinLet f = Setting inLet true f ();

   fun PushVert () = (verts := ! vert :: ! verts);

   fun PopVert () =
   case ! verts of nil => () | v :: vs => (vert := v; verts := vs);

   fun PushInd n = (inds := ! ind :: ! inds; ind := n; PushVert ());

   fun PopInd () =
   case inds of
      ref nil       => (outputc std_err.std_err "PopInd\n"; raise Interrupt)
   |  ref (i :: is) => (ind := i; inds := is; PopVert ());

   fun Und () = case ! inds of [] => ! ind | [_] => ! ind | ind :: _ => ind;

   fun SetInd () = PushInd (! pos);

   fun AugInd n = PushInd (! ind + n);

   fun prettyprintable pr = (writeprintable pr; pos := ! pos + printsize pr);

   val Max = (0 /> max);

   val Tsize = printsize o prT;

   val Asize = printsize o prA;

   val COMsize = printsize o prCOM;

   val Esize = printsize o prE;

   val Isize = printsize o prID;

   val CASEsize = printsize o prCASE;

   val DATAsize = printsize o prDATA;

   val prettyCASE = prettyprintable o prCASE;

   val prettyDATA = prettyprintable o prDATA;

   fun RoomForN (n) = (! rm >= n + ! pos);

   fun RoomForEWith n e = RoomForN (n + Esize e);

   fun RoomForExtra n p = RoomForN (n + printsize p);

   fun RoomFor p = RoomForExtra 0 p;

   fun prettys s = (write s; pos := ! pos + String.size s);

   fun prettyindent n =
   if n <= 0 then () else (prettys " "; prettyindent (n - 1));

   fun prettynl () = (write "\n"; pos := 0; prettyindent (! ind));

   fun prettystring s =
   (lfolds (fn ("\n", _) => prettynl () | (c, _) => prettys c) () s; ());

   fun prettylist punct nl size pretty objects =
   let val s = String.size punct;
       fun F [] = ()
       |   F [x] =
	   (if RoomForN (size x) then () else prettystring nl; pretty x)
       |   F (x :: xs) =
	   ( if RoomForN (s + size x) then () else prettystring nl;
	     pretty x;
	     prettystring punct;
	     F xs
	   )
   in
       F objects
   end;

   fun prettycommentbody string =
   let val size = String.size string;
       fun stringat n = if n < size then ordof (string, n) else 0;
       fun gap n =
       if n = size then n
       else if ordof (string, n) <= 32 then gap (n + 1) else n;
       fun res s = implode (rev s);
       fun sylls n s =
       if n = size then [res s]
       else
       let val c = ordof (string, n)
       in
	   if c = 10 andalso stringat (n + 1) = 10 then
	      res s :: "\n\n" :: sylls (gap n) []
	   else
	   if c <= 32 then
	      if null s then sylls (n + 1) [] else res s :: sylls (gap n) []
	   else sylls (n + 1) (chr c :: s)
       end
   in
       prettylist " " "\n" String.size prettystring (sylls 0 [])
   end;

   fun prettycomment "" = ()
   |   prettycomment s =
       (
	 SetInd ();
	 prettycommentbody s;
	 PopInd ();
	 ()

       );

   fun prevlist s ind f []  = ()
   |   prevlist s ind f [x] = f x
   |   prevlist s ind f (x :: xs) =
       ( f x;
	 prettynl ();
	 prettystring s;
	 prettyindent ind;
	 prevlist s ind f xs
       );

   fun postvlist s f []  = ()
   |   postvlist s f [x] = f x
   |   postvlist s f (x :: xs) =
       (f x; prettystring s; prettynl (); postvlist s f xs);

   fun prettysequence f xs = postvlist (if ! inLet then ";" else ";\n") f xs;

   fun prettyDec prLHS prettyLHS prRHS prettyRHS (lhs, rhs) =
   let val here = Und ();
       val pl = prLHS lhs;
       val pr = prRHS rhs;
       val dec = prnode [pl, prstring " = ", pr]
   in
       if RoomFor dec then prettyprintable dec
       else
       let
       in
	   prettyLHS lhs;
	   prettys " =";
	   PushInd here;
	   vert := true;
	   prettynl ();
	   prettyRHS rhs;
	   PopInd ()
       end
   end;

   fun prandnl start indentation printer objects =
   let val pnewl = if ! inLet then prettynl else prettynl o prettynl;
       fun prx pnl punct ind [] = ()
       |   prx pnl punct ind (x :: xs) =
	   ( pnl ();
	     prettys punct;
	     prettyindent ind;
	     printer x;
	     prx pnewl "and " indentation xs
	   )
   in
       prx (fn () => ()) start 0 objects
   end;

   fun prand start indentation printer objects =
   Setting inLet true (prandnl start indentation printer) objects;

   val lastchar = ExprPrint.lastchar o prE;

   val firstchar = ExprPrint.firstchar o prE;

   val prettyI = prettys;

   fun Disjoint e [] = false
   |   Disjoint e (e' :: _) = ExprPrint.NonFollow (lastchar e) (firstchar e');

   fun prettyA space a =
   (prettyindent space; case a of SOME _ => prettyprintable (prA a) | _ => ());

   fun prettyC space c =
   case c of SomeComment s => (prettyindent space; prettycomment s) | _ => ()
end;

structure pretty =
struct
   open prettybase;

   fun prettyD d =
   let val pr = prD d
   in
       AugInd 0;
       if ! inLet andalso RoomFor pr andalso Horizontal () then
	  prettyprintable pr
       else
       ( vert := true;
	 case d of
	    Dnum (_, d) => prettyD d
	 |  CommentD (c, d) => (prettycomment c; prettynl (); prettyD d)
	 |  Type tys => (prettyTYS "type " tys)
	 |  Data dys => (prandnl "datatype " 0 prettyDY dys)
	 |  With (dys, tys) =>
	    ( prandnl "datatype " 0 prettyDY dys;
	      prettynl ();
	      prettys "withtype";
	      prettynl ();
	      prettyTYS "\032\032\032\032" tys
	    )
	 |  AbsData (dys, d) =>
	    ( prandnl "abstype " 0 prettyDY dys;
	      prettynl ();
	      prettys "with";
	      AugInd 4;
	      prettynl ();
	      prettyD d;
	      PopInd ();
	      prettynl ();
	      prettys "end"
	    )
	 |  AbsWith (dys, tys, d) =>
	    ( prandnl "abstype " 0 prettyDY dys;
	      prettynl ();
	      prettys "withtype";
	      prettynl ();
	      prettyTYS "\032\032\032\032\032" tys;
	      prettynl ();
	      prettys "with";
	      AugInd 4;
	      prettynl ();
	      prettyD d;
	      PopInd ();
	      prettynl ();
	      prettys "end"
	    )
	 |  FunDecs eqns => (prandnl "fun " 0 prettyFUN eqns)
	 |  ValDecs eqns => (prandnl "val " 0 prettyEQN eqns)
	 |  RecDecs eqns => (prandnl "val rec " 0 prettyEQN eqns)
	 |  ExnDecs ds => (prandnl "exception " 0 prettyEXNDEC ds)
	 |  StrDecs eqns => (prandnl "structure " 0 prettySTRDEC eqns)
	 |  FuncDecs ds => (prandnl "functor " 0 prettyFUNCDEC ds)
	 |  SigDecs ds => (prandnl "signature " 0 prettySIGDEC ds)
	 |  SeqDecs ds => (prettysequence prettyD ds)
	 |  LocDecs (d, b) =>
	    ( SetInd ();
	      prettys "local ";
	      AugInd 4;
	      prettynl ();
	      Setting inLet true prettyD d;
	      PopInd ();
	      prettynl ();
	      prettys "in";
	      AugInd 4;
	      prettynl ();
	      prettyD b;
	      PopInd ();
	      prettynl ();
	      prettys "end";
	      PopInd ()
	    )
	 |  ItDec e => prettyE e
	 |  _ => prettyprintable pr
       );
       PopInd ()
   end

   and prettySPEC d =
   let val pr = prSPEC d
   in
       AugInd 0;
       if RoomFor pr andalso Horizontal () then prettyprintable pr
       else
       ( vert := true;
	 case d of
	    ValSpecs sps    => (prand "val " 0 prettyVS sps)
	 |  ExnSpecs sps    => (prand "exception " 0 prettyCASE sps)
	 |  StructSpecs sps => (prand "structure " 0 prettySTRSPEC sps)
	 |  TypeSpecs sps   => (prand "type " 1 prettyT sps)
	 |  EqTypeSpecs sps => (prand "eqtype " 3 prettyT sps)
	 |  DataSpecs sps   => (prand "datatype " 0 prettyDY sps)
	 |  LocSpecs (sps, sps') =>
	    ( SetInd ();
	      prettys "local";
	      AugInd 4;
	      prettynl ();
	      prettysequence prettySPEC sps;
	      PopInd ();
	      prettynl ();
	      prettys "in";
	      AugInd 4;
	      prettynl ();
	      prettysequence prettySPEC sps';
	      PopInd ();
	      prettynl ();
	      prettys "end";
	      PopInd ()
	    )
	 |  CommentS (c, s) => (prettycomment c; prettynl (); prettySPEC s)
	 |  _               => prettyprintable pr
       );
       PopInd ()
   end

   and prettyT ty =
   if RoomForN (Tsize ty) then prettyprintable (prT ty)
   else
      case ty of
	 Tnum (_, ty) => prettyT ty
      |  Record ts =>
	 ( prettys "{";
	   SetInd ();
	   postvlist ", " prettyFIELD ts;
	   PopInd ();
	   prettys "}"
	 )
      |  _            => prettyprintable (prT ty)

   and prettySTRSPEC (spec as (i, s)) =
   let val prspec = prSTRSPEC spec
   in
       if RoomFor prspec then prettyprintable prspec
       else (prettyprintable (prID i); prettys ":"; prettynl (); prettySIG s)
   end

   and prettyFIELD (i, t) = (prettyI i; prettys " : "; prettyT t)

   and prettyVS vs = prettyprintable (prVS vs)

   and prettyTYS keyword tys =
   let val lhswidth = Max ((Tsize o # 1) MAP tys);
       val rhswidth = Max ((Tsize o # 2) MAP tys);
       val awidth = Max ((Asize o # 3) MAP tys);
       fun prettyTY (lhs, rhs, a, c) =
       ( prettyT lhs;
	 prettyindent (lhswidth - Tsize lhs);
	 prettys " = ";
	 prettyT rhs;
	 prettyA (rhswidth - Tsize rhs) a;
	 prettyC (awidth - Asize a) c
       )
   in
       prandnl keyword 1 prettyTY tys
   end

   and prettyDY (t, cases) =
   if RoomForN (DATAsize (t, cases)) then prettyDATA (t, cases)
   else
   if All (fn Constant (_, NONE, NoComment) => true | _ => false) cases then
   ( prettyprintable (prT t);
     prettys "\032=";
     SetInd ();
     prettys "\032";
     prettylist "|\032\032" "\n\032" CASEsize prettyCASE cases;
     PopInd ()
   )
   else prettyDYV (t, cases)

   and prettyDYV (t, cases) =
   let val ConWidth =
       Isize o (fn Construct (i, _, _, _) => i | Constant (i, _, _) => i);
       val TyWidth =
       Tsize o (fn Construct (_, t, _, _) => t | Constant _ => Prod []);
       val AWidth =
       Asize o (fn Construct (_, _, a, _) => a | Constant (_, a, _) => a);
       fun Fit c = RoomForN (CASEsize c);
       val fitcases = Fit <| cases;
       val conwidth = Max (ConWidth MAP fitcases);
       val tywidth = Max (TyWidth MAP fitcases);
       val awidth = Max (AWidth MAP fitcases);
       val fullwidth = conwidth + tywidth + awidth;
       fun prettyAC af a cf c =
       ( if RoomForN (af + Asize a) then () else prettynl ();
	 prettyA af a;
	 prettynl ();
	 prettyC 0 c
       );
       fun prettyCon c =
       if Fit c then
	  case c of
	     Construct (i, ty, a, c) =>
	     ( prettys i;
	       prettyindent (conwidth - String.size i);
	       prettys " of ";
	       prettyT ty;
	       prettyA (tywidth - Tsize ty) a;
	       prettyC (awidth - Asize a) c
	     )
	  |  Constant (i, a, c) =>
	     ( prettys i;
	       prettyA (tywidth + conwidth - Isize i) a;
	       prettyC (4 + awidth - Asize a) c
	     )
       else
       ( SetInd ();
	 case c of
	    Construct (i, ty, a, c) =>
	    ( prettys i;
	      prettyindent (conwidth - String.size i);
	      prettys " of ";
	      prettyT ty;
	      prettyAC (tywidth - Tsize ty) a (awidth - Asize a) c
	    )
	 |  Constant (i, a, c) =>
	    ( prettys i;
	      prettyAC (tywidth + conwidth - Isize i) a (4 + awidth - Asize a)
	      c
	    );
	 PopInd ()
       )
   in
       prettyprintable (prT t);
       prettys " ";
       prettys "= ";
       prettynl ();
       prettys "\032\032";
       SetInd ();
       prettys "\032\032";
       prevlist "|" 1 prettyCon cases;
       PopInd ()
   end

   and prettyE e =
   let val pr = prE e
   in
       if RoomFor pr then prettyprintable pr
       else
       ( SetInd ();
	 vert := true;
	 case e of
	    Enum (_, e) => prettyE e
	 |  AndAlso es => pretty2Es " andalso " es
	 |  OrElse es => pretty2Es " orelse " es
	 |  Ap es => pretty2Es "" es
	 |  As es => pretty2Es " as " es
	 |  Let (d, es) =>
	    let val ref inLet' = inLet
	    in
		inLet := true;
		prettys "let ";
		AugInd 4;
		prettyD d;
		PopInd ();
		prettynl ();
		prettys "in";
		AugInd 4;
		prettynl ();
		prettysequence prettyE es;
		PopInd ();
		prettynl ();
		prettys "end";
		inLet := inLet'
	    end
	 |  Case (e, ms) =>
	    ( prettys "case ";
	      SetInd ();
	      prettyE e;
	      PopInd ();
	      prettys " of";
	      prettynl ();
	      prettys "\032\032\032";
	      prettyMATCHS ms
	    )
	 |  Fn ms => (prettys "fn "; prettyMATCHS ms)
	 |  While (bb, dd) =>
	    ( prettys "while ";
	      prettyE bb;
	      prettys " do";
	      if RoomForEWith 1 dd then (prettys " "; prettyE dd)
	      else (AugInd 3; prettynl (); prettyE dd; PopInd ())
	    )
	 |  If (bb, tt, ff) =>
	    let fun BaseE e = case e of Enum (_, e) => BaseE e | _ => e;
		fun ForThen e =
		case BaseE e of Tuple [Seq _] => 0 | Let _ => 0 | _ => 3;
		fun ForElse e =
		case BaseE e of
		   Tuple [Seq _] => 0
		|  If _          => 0
		|  Let _         => 0
		|  _             => 3;
		val AugForThen = AugInd o ForThen;
		val AugForElse = AugInd o ForElse
	    in
		prettys "if ";
		prettyE bb;
		if RoomForEWith 6 tt then
		(prettys " then "; prettyE tt; prettynl ())
		else
		if RoomForN 5 then
		( prettys " then";
		  AugForThen tt;
		  prettynl ();
		  prettyE tt;
		  PopInd ();
		  prettynl ()
		)
		else
		( prettynl ();
		  prettys "then ";
		  prettynl ();
		  AugForThen tt;
		  prettyE tt;
		  PopInd ();
		  prettynl ()
		);
		if RoomForEWith 5 ff then (prettys "else "; prettyE ff)
		else
		( prettys "else";
		  AugForElse ff;
		  prettynl ();
		  prettyE ff;
		  PopInd ()
		)
	    end
	 |  Tuple ([Seq es]) =>
	    ( prettys "( ";
	      SetInd ();
	      Setting inLet true (prettysequence prettyE) es;
	      PopInd ();
	      prettynl ();
	      prettys ")"
	    )
	 |  Handle (e, ms) =>
	    ( prettyE e;
	      prettynl ();
	      prettys "handle";
	      prettynl ();
	      prettys "\032\032\032";
	      prettyMATCHS ms
	    )
	 |  Expr es =>
	    ( AugInd 3;
	      prettyElist (not (! ExprPrint.Wide)) "" " " "" es;
	      PopInd ()
	    )
	 |  Tuple es => prettyElist false "(" ", " ")" es
	 |  Rec bs =>
	    ( prettys "{ ";
	      SetInd ();
	      postvlist "," prettyBIND bs;
	      prettys "}";
	      PopInd ()
	    )
	 |  List es => prettyElist false "[" ", " "]" es
	 |  Has (e, t) => (prettyE e; prettys " : "; prettyT t)
	 |  _ => prettyprintable pr;
	 PopInd ()
       )
   end

   and prettyBIND b =
   let val prb = prBIND b
   in
       if RoomFor prb then prettyprintable prb
       else
	  case b of
	     EqBind (i, e) => (prettyI i; prettys " = "; prettyE e)
	  |  VarBind e     => prettyE e
   end

   and pretty2Es comb (e1, e2) =
   (prettyE e1; prettys comb; prettynl (); prettyE e2)

   and prettyElist squeeze bra comma ket es =
   let val es = ref es;
       val n = ref 0;
       fun condnl (e) =
       if ! n = 0 orelse RoomForEWith 0 e then () else prettynl ()
   in
       prettys bra;
       SetInd ();
       while case es of
		ref nil   => false
	     |  ref ([e]) => (condnl e; prettyE e; false)
	     |  ref (e :: es') =>
		( condnl e;
		  prettyE e;
		  if squeeze andalso Disjoint e es' then () else prettys comma;
		  es := es';
		  true
		) do inc n;
       PopInd ();
       prettys ket
   end

   and prettyEQN pair = prettyPair true "\032=\032" pair

   and prettyMATCHS ms = prettyPairs "|\032\032" "\032=>\032" false ms

   and prettyFUN eqns =
   case eqns of
      [eqn] => prettyEQN eqn
   |  _     => prettyPairs "|\032\032\032" "\032=\032" false eqns

   and prettyPairs bar sep undent pairs =
   let val lhswidth = Max ((Esize o # 1) MAP pairs);
       val rhswidth = Max ((Esize o # 2) MAP pairs);
       val lengths = String.size sep;
       fun Fit n (l, r) = RoomForN (lhswidth + Esize r + n + lengths);
       val y = Fit (String.size bar) <| pairs;
       val ylhswidth = Max ((Esize o # 1) MAP y);
       val lengthy = List.length y;
       val lengthn = List.length pairs - lengthy;
       val prettyQ = prettyPair undent sep;
       fun prettyP (pair as (l, r)) =
       if Fit 0 pair then
       (prettyE l; prettyindent (ylhswidth - Esize l); prettys sep; prettyE r)
       else prettyQ pair
   in
       prevlist bar 0 (if 2 * lengthy > 3 * lengthn then prettyP else prettyQ)
       pairs
   end

   and prettyPair undent sep (l, r) =
   let val here = if undent then Und () else Pos ();
       val pr = prE r
   in
       prettyE l;
       prettys sep;
       if RoomFor pr then prettyprintable pr
       else (PushInd here; prettynl (); prettyE r; PopInd ())
   end

   and prettySTRDEC (id, sigopt, str) =
   prettyDec prSTRID prettySTRID prSTRUCT prettySTRUCT ((id, sigopt), str)

   and prSTRID (id, sigopt) = prnode [prID id, prSIGOPT sigopt]

   and prettySTRID (id, sigopt) =
   (prettyprintable (prID id); prettySIGOPT sigopt)

   and prettyEXNDEC d = (prettyprintable o prEXNDEC) d

   and prettyFUNCDEC (d as (i, funpar, sigopt, str)) =
   let val prdec = prFUNCDEC d
   in
       if RoomFor prdec then prettyprintable prdec
       else
       let val prpar = prFUNPAR funpar
       in
	   prettyprintable (prID i);
	   if RoomForExtra 3 prpar then
	   (prettys "("; prettyprintable prpar; prettys ")")
	   else
	   ( prettynl ();
	     prettys "( ";
	     SetInd ();
	     case funpar of
		OpenPar specs    => (prettysequence prettySPEC specs)
	     |  ClosePar strspec => prettyprintable (prSTRSPEC strspec);
	     PopInd ();
	     prettynl ();
	     prettys ")"
	   );
	   prettySIGOPT sigopt;
	   prettys "=";
	   prettynl ();
	   prettySTRUCT str
       end
   end

   and prettySIGOPT s =
   let val prs = prSIGOPT s
   in
       if RoomForExtra 1 prs then prettyprintable prs
       else
	  case s of
	     SomeSig s => (prettys ":"; prettynl (); prettySIG s)
	  |  NoSig     => ()
   end

   and prettySIG s =
   case s of
      Sig specs =>
      ( prettys "sig";
	AugInd 3;
	prettynl ();
	prettysequence prettySPEC specs;
	PopInd ();
	prettynl ();
	prettys "end"
      )
   |  _ => prettyprintable (prSIG s)

   and prettySTRUCT s =
   case s of
      Struct ds =>
      ( prettys "struct";
	AugInd 3;
	prettynl ();
	prettyD ds;
	PopInd ();
	prettynl ();
	prettys "end"
      )
   |  LetStruct (d, s) =>
      ( SetInd ();
	prettys "let ";
	AugInd 4;
	prettyD d;
	PopInd ();
	prettynl ();
	prettys "in";
	AugInd 4;
	prettynl ();
	prettySTRUCT s;
	PopInd ();
	prettynl ();
	prettys "end";
	PopInd ()
      )
   |  _ => prettyprintable (prSTRUCT s)

   and prettySIGDEC d =
   prettyDec prID (prettyprintable o prID) prSIG prettySIG d;

   fun pr d =
   (pos := 0; ind := 0; inds := []; vert := false; prettyD d; write ";\n\n")
end;

fun Writer file =
ParseThen (fn t => (pretty.pr (objectprogram.WriterDeclaration t))) file;

fun Pretty file = ParseThen (fn t => (pretty.pr (t))) file;

val cwd = ref ".";

fun Pr f =
let val s = SwapStream outstream (open_out (f ^ ".sml.pretty"))
in
    Pretty (! cwd ^ "/" ^ f ^ ".sml");
    close_out (SwapStream outstream s)
end;

