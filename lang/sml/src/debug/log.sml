signature LOG =
sig
  (* Characterizes a log with entries of type entry. *)
  type entry
  type mark
  exception Log
      (* Raised by any marker operation given a marker that is invalid
         either because it points to end of log or because it points to
	 an entry that was truncated. *) 
  val new: unit -> mark
      (* Returns mark at end of new, initially empty log. *)
  val copyMark: mark -> mark
      (* Create a new mark that copies argument. *)
  val resetMark: mark -> mark -> unit
      (* Reset first arg to second. *)
  val equalMarks: mark * mark -> bool
      (* Returns true iff marks point to same location in same log. *)
  val append: mark -> entry -> unit
      (* Append to log at mark and advance mark. 
         If mark is not already at end of log, truncate first. *)
  val truncate: mark -> unit
      (* Delete all entries in log from mark onwards. 
         Invalidates marks beyond current location. 
	 Argument mark is still valid for appends. *)
  val get: mark -> entry
      (* Get entry at mark, and advance mark.
         Raise Log if at end. *)
  val read: mark -> entry
      (* Get entry at mark, without advancing.
         Raise Log if at end. *)
  val replace: mark -> entry -> unit
      (* Replace entry at mark, and advance mark.
         Raise Log if at end. *)
  val write: mark -> entry -> unit
      (* Replace entry at mark, without advancing.
         Raise Log if at end. *)
  val advance: mark -> unit
      (* Advance mark. Raise Log if at end. *)
end

functor Log (type entry): LOG =
struct
  open Array List
  infix 9 sub
  type entry = entry
  datatype segment = SEG of contents ref
  and contents = ARRSEG of (entry array * segment (* next *))
               | LISTSEG of entry list ref * int ref (* count *)
  datatype mark = MARK of (segment ref (* pointer's seg *)
			   * int ref (* pointer's index *))
  val zeroarr:entry array = arrayoflist []
  exception Log
  exception InLog of string

(* Note: It is not an invariant that arrays are length > 0, but we try to
   avoid creating such arrays unnecessarily. *)

  fun emptyseg() = SEG(ref(LISTSEG(ref(nil:entry list),ref 0)))
  fun new () = MARK(ref(emptyseg()),ref 0)

  fun normalize(SEG(contentsref as (ref(LISTSEG(ref (h::t),ref c))))) =
      (* non-empty top list: convert to array *)
      let val a = array(c,h)
          fun fill (~1,nil) = ()
	    | fill(_,nil) = raise (InLog "normalize")
	    | fill(n,h::t) = (update(a,n,h);fill(n-1,t))
      in fill(c-2,t);
	 contentsref := ARRSEG(a,emptyseg())
      end
    | normalize(SEG(ref(LISTSEG(ref nil,_)))) = () 
      (* empty top list already *)
    | normalize _ = () 

  fun fix(mark as (MARK(segref as (ref(SEG(ref(ARRSEG(a,next))))),indexref as ref index))) =
      let fun nonempty(seg as (SEG(ref(ARRSEG(a,next))))) =
	       if Array.length a = 0 then
		 nonempty next
	       else seg
	    | nonempty(seg as (SEG(ref(LISTSEG _)))) =
	       seg
      in segref := nonempty next;
	 indexref := 0
      end
    | fix _ = raise (InLog "fix")

(* Careful: we must take care that after a truncation, any further appends will
            go into a new segment.  *)
  fun truncate(MARK(ref(SEG(contentsref as (ref(LISTSEG(lref as ref l,_))))),ref index)) =
      (* make an array consisting of first index elements of list. *)
      let val a = if index > 0 then
	            let val a = array(index,hd l) handle Hd => raise Log
			fun fill (~1,_) = ()
			  | fill (_,nil) = raise Log (* mark was invalid *)
			  | fill (n,h::t) = (update(a,n,h); fill(n-1,t))
			in fill (index-2,tl l);
			   a
			end
		  else zeroarr
          val empty = emptyseg()
      in contentsref := ARRSEG(a,empty)
      end
    | truncate(MARK(ref(SEG(contentsref as (ref(ARRSEG(a,next))))),ref index)) =
      (* mark is in intermediate ARRAYSEG. *)
      let fun zap (SEG(contentsref as (ref(ARRSEG(a,next))))) =
	       (contentsref := LISTSEG(ref nil,ref ~1);
		zap next)
	    | zap (SEG(contentsref as (ref(LISTSEG(lref,cref))))) =
	        (lref := nil;
		 cref := ~1)
	  val a' = if index > 0 then 
	             array(index,a sub 0) handle Subscript => raise Log
		   else zeroarr
	  fun fill n = if n < index then
		         (update(a',n,a sub n handle Subscript => raise Log);
			  fill (n+1))
		        else ()
          val empty = emptyseg()
      in fill 1;
         contentsref := ARRSEG(a',empty);
         zap next
      end

  fun append(mark as MARK(ref(SEG(ref(LISTSEG(lref as ref l,cref as ref c)))),
		        indexref as ref index)) x =
	if index = c then
	  (lref := x::l;
	   inc cref;
	   inc indexref)
	else if index < c then
	  (truncate mark;
	   append mark x)
	else 
          raise Log
    | append(mark as MARK((ref(SEG(ref(ARRSEG(a,_))))),ref index)) x =
	let val alen = Array.length a
	in if index = alen then
	     (fix mark;
	      append mark x)
	   else if index < alen then 
	     (* mark points into a non-zero array segment *)
	     (truncate mark;
	      append mark x)
	   else (* index > alen : invalid mark *)
	     raise Log
        end

  fun copyMark(mark as MARK(segref as ref seg,indexref)) = 
      (normalize seg;
       MARK(ref (!segref),ref (!indexref)))

  fun resetMark (MARK(segref,indexref)) (MARK(ref seg,ref index)) =
      (segref := seg;
       indexref := index)

  fun read(MARK(ref(SEG(ref(LISTSEG(ref nil,_)))),_)) =
      (* mark is at end of log. *)
         raise Log
    | read(mark as MARK(ref(seg as SEG(ref(LISTSEG _))),_)) =
      (* mark is in non-empty top LISTSEG; normalize first *)
        (normalize seg;
	 read mark)
    | read(mark as MARK(ref(SEG(ref(ARRSEG(a,_)))),ref index)) =
      (* normal case *)
      if index = Array.length a then
	(fix mark;
	 read mark)
      else  
        a sub index
          handle Subscript => raise Log


  fun write(MARK(ref(SEG(ref(LISTSEG(ref nil,_)))),_)) x =
      (* mark is at end of log. *)
         raise Log
    | write(mark as MARK(ref(seg as SEG(ref(LISTSEG _))),_)) x =
      (* mark is in non-empty top LISTSEG; normalize first *)
        (normalize seg;
	 write mark x)
    | write(mark as MARK(ref(SEG(ref(ARRSEG(a,_)))),ref index)) x =
      (* normal case *)
        if index = Array.length a then
	  (fix mark;
	   write mark x)
        else
	  update(a,index,x)
            handle Subscript => raise Log


  fun advance(mark as MARK(ref(SEG(ref(ARRSEG(a,_)))),indexref as ref index)) =
	let val alen = Array.length a
	in if index = alen then
	     (fix mark;
	      advance mark)
	   else if index < alen then 
	       inc indexref
	   else (* index > alen : invalid mark *)
	     raise Log
        end
   | advance(MARK(ref(SEG(ref(LISTSEG(ref nil,_)))),_)) = 
        raise Log
   | advance(mark as MARK(ref(seg as SEG(ref(LISTSEG _))),_)) =
	 (normalize seg;
	  advance mark)

  fun get mark = read mark before advance mark

  fun replace mark x = write mark x before advance mark

  fun equalMarks(mark1,mark2) =
      let fun fixif (mark as MARK(ref(SEG(ref(ARRSEG(a,_)))),ref index)) =
  	        if index = Array.length a then
		    fix mark
		else ()
	    | fixif _ = ()
	  fun equal (MARK(ref seg1,ref index1), MARK(ref seg2,ref index2)) =
	      seg1 = seg2 andalso index1 = index2
      in
        fixif mark1;
        fixif mark2;
        equal (mark1,mark2)
      end
end



