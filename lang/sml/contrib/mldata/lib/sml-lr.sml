
functor LR() :LR=
struct
   open Array;

   infix 9 sub;

   datatype parseaction =
       ERROR
     | ACCEPT
     | SHIFT  of int
     | REDUCE of int * int * int

   and 'a ParseResult = ParseOK of 'a   |  ParseERR of 'a   |  ParseFAIL  ;

   val lr_out = ref std_out;

   local
       fun outs (s : string) = outputc ( ! lr_out) s
       and outi (i : int) = outs (makestring i);
       fun stack () =
       let val stack = ref nil;
	   fun Push s = stack := s :: ( ! stack);
	   fun Pop n =
	   if n > 0 then (stack := tl ( ! stack); Pop (n - 1)) else ! stack;
	   fun Top () = hd ( ! stack)
       in
	   (Push, Pop, Top)
       end;
       fun TRACEPARSER (getsname : string array) outa
	   (comment, state, act, symb, attr, pstack, astack) =
       let fun outsy s = outs (getsname sub s)
       in
	   outs "\n";
	   outs comment;
	   outs "St:";
	   outi state;
	   outs "; Sy:(";
	   outsy symb;
	   outs ", ";
	   outa attr;
	   outs "); ";
	   case act of
	      ERROR   => outs "ERROR"
	   |  ACCEPT  => outs "ACCEPT"
	   |  SHIFT n => (outs "SHIFT:"; outi n)
	   |  REDUCE (prod, nt, size) =>
	      ( outs "REDUCE:";
		outi prod;
		outs " ";
		outsy nt;
		outs " ";
		outi size
	      );
	   DUMPSTACKS outsy outa (pstack, astack)
       end
       and DUMPSTACKS outsy outa (pstack, astack) =
       let val ast = ref astack
       in
	   List.app
	   (fn (st, sym) =>
	       ( outs "\n  (";
		 outi st;
		 outs " ";
		 outsy sym;
		 outs ": ";
		 if null ( ! ast) then ()
		 else (outa (hd ( ! ast)); ast := tl ( ! ast));
		 outs ")"
	       )) pstack;
	   List.app (fn a => (outs "::"; outa a)) ( ! ast)
       end
   in
       fun parse getaction getgoto getrecovery errornonterminal endsymbol
	   getattrfun getsymbolname writeattribute recovering getsymbol trace =
       let val (Push, Pop, Top) = stack ();
	   val (PushA, PopA, TopA) = stack ();
	   val halt = ref false;
	   val success = ref true;
	   val errors = ref false;
	   val errorstate = ref (0);
	   val errornonterminal = errornonterminal + endsymbol : int;
	   val (symbolval, attrval) = getsymbol ();
	   val symb = ref (symbolval);
	   val attr = ref attrval;
	   fun nextsymbol () =
	   let val (symbolval, attrval) = getsymbol ()
	   in
	       symb := symbolval;
	       attr := attrval
	   end;
	   val TRACE = TRACEPARSER getsymbolname writeattribute;
	   fun FindRecoveryLevel ([], n) = false
	   |   FindRecoveryLevel ((state, _) :: ss, n) =
	       if state <> ! errorstate andalso getrecovery (state, ! symb)
	       then
	       ( Pop n;
		 PopA n;
		 if trace then
		    TRACE
		    ("[Recovered]", state, ERROR, ! symb, ! attr, Pop 0, PopA 0)
		 else ();
		 true
	       )
	       else FindRecoveryLevel (ss, n + 1);
	   fun SkipSymbols state =
	   if ! symb = endsymbol then false
	   else
	   ( if trace then
		TRACE ("[Skipping]", state, ERROR, ! symb, ! attr, [], [])
	     else ();
	     nextsymbol ();
	     if FindRecoveryLevel (Pop 0, 0) then true else SkipSymbols state
	   )
       in
	   Push (1, ! symb);
	   PushA ( ! attr);
	   while not ( ! halt) do
	      let val (state, _) = Top ();
		  val act = getaction (state, ! symb)
	      in
		  if trace then
		     TRACE ("", state, act, ! symb, ! attr, Pop 0, PopA 0)
		  else ();
		  case act of
		     ACCEPT => (halt := true)
		  |  SHIFT state' =>
		     ( Push (state', ! symb);
		       PushA ( ! attr);
		       errorstate := 0;
		       nextsymbol ()
		     )
		  |  REDUCE (prod, nonterminal, size) =>
		     let val attr = getattrfun prod (PopA 0);
			 val (state', _) = (Pop size; Top ())
		     in
			 PopA size;
			 Push (getgoto (state', nonterminal), nonterminal);
			 PushA (attr)
		     end
		  |  ERROR =>
		     ( errors := true;
		       recovering true;
		       if FindRecoveryLevel (Pop 0, 0) orelse SkipSymbols state
		       then
		       ( errorstate := let val (state, _) = Top () in state end;
			 Push
			 (getgoto ( ! errorstate, errornonterminal),
			  errornonterminal);
			 PushA (TopA ());
			 recovering false
		       )
		       else (halt := true; success := false)
		     )
	      end;
	   recovering false;
	   if ! success then
	      if ! errors then ParseERR (TopA ()) else ParseOK (TopA ())
	   else ParseFAIL
       end
   end
end;


