(* Copyright 1989 by AT&T Bell Laboratories *)
(*
The following are already in the symbol table:
     1) Magical words that can be free in signatures (from PrimTypes):
		int string bool unit real list array ref exn
     2) Built-in constructors (from PrimTypes):
		:: nil ref true false
     3) Built-in structures:
		PrimTypes InLine
	The InLine structure is not typed (all values have type alpha).
All matches in this file should be exhaustive; the match and bind exceptions
 are not defined at this stage of bootup, so any uncaught match will cause
 an unpredictable error.
*)


functor CoreFunc (Assembly : ASSEMBLY) =
  struct
    structure Assembly = Assembly

    exception Bind
    exception Match

    exception Ord        	(* for strings, bytearray update bounds check *)
    exception Range      	(* for bytearray update *)
    exception Subscript  	(* for vectors *)
    exception RealSubscript	(* for floating arrays *)

    local exception NoProfiler
    in val profile_register =
      ref(fn s:string => (raise NoProfiler):int*int array*int ref)
    end

    val current = Assembly.current  (* get rid of this *)
    val toplevel = Assembly.A.create_b 18  (* get rid of this *)

    val getDebugf = ref (fn ()=>())

    val forcer_p = ref (fn () => ())

    fun getDebug x = InLine.! getDebugf x

    val vector0 = Assembly.vector0  (* needed to compile ``#[]'' *)
    val errorMatch = ref ""

    fun stringequal(a,b) =
	  if InLine.ieql(a,b) then true
	    else InLine.boxed a andalso InLine.boxed b andalso
	      let val len = InLine.length a
		in if InLine.ieql(len,InLine.length b)
		  then let
		    fun f 0 = true
		      | f i = let val j = InLine.-(i,1)
			      in if InLine.ieql(InLine.ordof(a,j), InLine.ordof(b,j))
			           then f j else false
			      end
		    in f len end
		  else false
		end

    local
      val ieql = InLine.ieql and ineq = InLine.ineq and feql = InLine.feql
      val cast = InLine.cast and sub = InLine.subscript
      val getObjTag = InLine.gettag and boxed = InLine.boxed and op * = InLine.*
      val op + = InLine.+ and op - = InLine.-
    in
      fun polyequal (a : 'a, b : 'a) =
	    ieql(a, b)
	    orelse (boxed a andalso boxed b
	      andalso let
		val aTag = getObjTag a
		fun pairEq () = let val bTag = getObjTag b
		      in
			(ieql(bTag, 0x02) orelse ineq(InLine.andb(bTag, 0x3), 0x2))
			  andalso polyequal(sub(a,0), sub(b,0))
			  andalso polyequal(sub(a,1), sub(b,1))
		      end
		in
		  case aTag
		   of 0x02 (* tag_pair *) => pairEq()
		    | 0x06 (* tag_reald *) => feql(a, b)
		    | 0x0a (* tag_embedded_reald *) => feql(a, b)
		    | 0x12 (* tag_special *) => false
		    | 0x22 (* tag_record *) => if ieql(getObjTag b, aTag)
			then let
			  val lenm1 = (InLine.length a) - 1
			  fun m (j : int) = if ieql(j, lenm1)
				then polyequal(sub(a,j), sub(b,j))
				else polyequal(sub(a,j), sub(b,j)) andalso m(j + 1)
			  in
			    m 0
			  end
			else false
		    | 0x26 (* tag_array *) => false
		    | 0x2a (* tag_string *) => stringequal(cast a, cast b)
		    | 0x2e (* tag_embedded_string *) => stringequal(cast a, cast b)
		    | 0x32 (* tag_bytearray *) => false
		    | 0x36 (* tag_realdarray *) => false
		    | _ (* tagless pair *) => pairEq()
		  (* end case *)
		end)
   end (* local *)
       val profile_sregister = ref(fn (x:Assembly.object,s:string)=>x)

end (* CoreFunc *)
