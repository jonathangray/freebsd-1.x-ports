(* DebugStore

   Support "historical" refs and arrays.
*)

signature DEBUG_STORE = sig
  val remember: unit -> DebugKernel.doers
  datatype ('a,'b) plist = PNIL | PCONS of 'a * 'b * ('a,'b) plist
  val updatedAList: (System.Unsafe.object array System.Unsafe.Weak.weak,int) plist ref
  val updatedRList: System.Unsafe.object array System.Unsafe.Weak.weak list ref
  val createdList: System.Unsafe.object array System.Unsafe.Weak.weak list ref
  val hcreatea: ((int * System.Unsafe.object) -> (System.Unsafe.object array))
  val hcreater: System.Unsafe.object -> (System.Unsafe.object array)
end


(*  The following are intended usages for making and updating historical
	refs and arrays:

fun href (obj) = hcreater (obj)

fun harray(cnt,obj) = hcreatea(cnt,obj)

fun hass (objr, value) = 
        (objr := value;
	 updatedRList := (weak objr) :: (!updatedRList))

fun hupd (arr, offset, value) =
	(Array.update (arr, offset, value);
	 updatedAList := PCONS(weak arr, offset,!updatedAList))

*)

structure DebugStore : DEBUG_STORE =
struct

  open Array List System.Unsafe System.Unsafe.Weak DebugUtil DebugKernel
  infix 9 sub 

  (* for inlining *)
  val special_weak = System.Tags.special_weak
  
  datatype ('a,'b) plist = PNIL | PCONS of 'a * 'b * ('a,'b) plist
  
  val updatedAList = ref (PNIL: (object array weak,int) plist)
  val updatedRList = ref (nil:object array weak list)
  val createdList = ref (nil:object array weak list) 
      (* it's rididulous to keep this separately... *)
  
  structure Log = TimedLog(type entry = object array weak)
  val mark = Log.new()
  
  fun getstrong (recreater:unit->object array) (refurbisher:object array ->object array): object array  =
       case strong (Log.read mark) of
	 SOME old => (Log.advance mark;
		      refurbisher old)
       | NONE => 
	  let val new = recreater()
	  in Log.replace mark (weak new);
	     new
	  end
    
  fun hcreatea(cnt,value) =
    let fun cladd obj =
	  (if cnt > 0 then
	     createdList := weak obj :: (!createdList)
	   else ();
	   obj)
        fun recreate () = array(cnt,value)  (* may raise Subscript *)
        fun refurbish obj = 
	  let fun d (~1) = ()
	  	| d n = ((*unsafe*)update(obj,n,value); d (n-1)) 
	  in d (cnt-1);
	     obj
	  end
    in 
      case (!execMode) of
	RECORD _ =>
	  let val newobj = array(cnt,value)
	  in Log.append mark (weak newobj);
	     cladd newobj
	  end
      | REPLAY _ =>
	  cladd (getstrong recreate refurbish)
      | STOP => array(cnt,value)
    end

  fun hcreater(value) = hcreatea(1,value)

  (* We should really use a sparse log for creations, using time labels
     to tell us when (potentially) strong entries are available.
     If this proves intractable, we may need to return to the old hash
     table technique. *)

  datatype storecreation = STORECREATION of object array * object array
				  * storecreation | NILstorecreation
  datatype storeupdate = STOREUPDATE of object array * int  * object 
				  * storeupdate | NILstoreupdate
  datatype storedelta = STOREDELTA of time * storecreation list * storeupdate
				      * storedelta * int ref * storehandle weak
		      | NILstoredelta
  and storehandle = STOREHANDLE of storedelta ref
  
  val lasth = ref (STOREHANDLE(ref NILstoredelta))
  
  val unique0 = ref () (* unique values *)
  val unique1 = ref () 
  
  fun remember' lastsd =
    let val h = STOREHANDLE(ref NILstoredelta)
	val _ = forcegc() 
	(*val _ = (print "remembering..."; flush_out std_out)  *)
	fun docl (warr::next,acc) =
  (*	    (case (strong warr) of
		 SOME a => let val a' = ArrayExt.copy a
			   in (*unsafe*) update (a,0,cast unique0);
			      docl (next,STORECREATION(a,a',acc))
			   end
	       | NONE => docl (next,acc))
  inlined version follows : *)
	      if (*unsafe*) getSpecial warr = special_weak then
		   let val a = (*unsafe*) subscript(cast warr,0)
		       val a' = ArrayExt.copy a
		   in (*unsafe*) update (a,0,cast unique0);
		      docl (next,STORECREATION(a,a',acc))
		   end
	      else docl(next,acc)
	  | docl (nil,acc) = acc
  (*      val _ = (print "cl original = "; print (length (!createdList));print "\n") *)
	val cl = docl (!createdList,NILstorecreation)
	fun sizecl (STORECREATION (_,_,next)) = 1 + (sizecl next)
	  | sizecl (NILstorecreation) = 0
  (*      val _ = (print "condensed to "; print (sizecl cl); print "\n") *)
	fun douAl (PCONS(warr,i,next),acc) =
  (*	    (case (strong warr) of
		 SOME a => if (not(cast((*unsafe*) subscript(a,0))=unique0)) then
			     let val ai' = (*unsafe*) subscript(a,i)
			     in if (not (cast ai' = unique1)) then
				  ((*unsafe*) update(a,i,cast unique1);
				   douAl (next,STOREUPDATE(a,i,ai',acc)))
				else douAl (next,acc)
			     end
			   else douAl (next,acc)
	       | NONE => douAl (next,acc)) 
  inlined version follows: *)
	      ((*print " "; print i; print ":";*)
	       if (*unsafe*) getSpecial warr = special_weak then
		   let val a = (*unsafe*) subscript(cast warr,0)
		   in if (cast ((*unsafe*) subscript(a,0)) = unique0) then
			((*print "U0";*)
			 douAl (next,acc))
		      else
			let val ai' = (*unsafe*) subscript(a,i)
			in if (cast ai' = unique1) then
			     ((*print "U";*)
			       douAl (next,acc))
			   else
			      ((*unsafe*) update(a,i,cast unique1);
			       douAl (next,STOREUPDATE(a,i,ai',acc)))
			end
		    end
	       else douAl(next,acc))
	  | douAl (PNIL,acc) = acc
	fun douRl (warr::next,acc) =
  (*	    (case (strong warr) of
		 SOME a => let val a0' = a sub 0
			   in if (not(cast a0' = unique0)) then
				((*unsafe*) update(a,0,cast unique0);
				 douRl (next,STOREUPDATE(a,0,a0',acc)))
			      else douRl (next,acc)
			   end
	       | NONE => douRl (next,acc))
  inlined version follows: *)
	    if (*unsafe*) getSpecial warr = special_weak then
		let val a = (*unsafe*) subscript(cast warr,0)
		    val a0' = (*unsafe*) subscript(a,0)
		in if cast a0' = unique0 then
		     douRl (next,acc)
		   else
		     ((*unsafe*) update(a,0,cast unique0);
		      douRl (next,STOREUPDATE(a,0,a0',acc)))
		end
	      else douRl(next,acc)
	  | douRl (nil,acc) = acc
	fun plength (PCONS(_,_,next)) = 1 + (plength next)
	  | plength (PNIL) = 0
  (*      val _ = (print "A R update original = "; print (plength (!updatedAList));
		  print " "; print (length (!updatedRList)); print "\n") *)
	val ul = douRl (!updatedRList, douAl (!updatedAList,NILstoreupdate)) 
	fun sizeul (STOREUPDATE(_,off,_,next)) = 
		  ((*print off; print " "; *)
		   1 + (sizeul next))
	  | sizeul (NILstoreupdate) = ((*print "total ";*) 0)
  (*      val _ = (print "condensed to "; print (sizeul ul); print "\n") *)
	fun resetc (STORECREATION(a,a',next)) = 
	      ((*unsafe*) update (a,0,(*unsafe*) subscript(a',0));
	       resetc next)
	  | resetc nNILstorecreation = ()
	val _ = resetc cl
	fun resetu (STOREUPDATE(a,i,ai',next)) =
	      ((*unsafe*) update(a,i,ai');
	       resetu next)
	  | resetu NILstoreupdate = ()
	val _ = resetu ul
        val _ = case lastsd of 
		  STOREDELTA(_,_,_,_,rc,_) => inc rc
		| NILstoredelta => ()
	val newsd = STOREDELTA(currentTime(),[cl],ul,lastsd,ref 0,weak h)
    in  (fn (STOREHANDLE(sdr)) => sdr := newsd) h;
	lasth := h;
	createdList := nil;
	updatedAList := PNIL;
	updatedRList := nil;
  (*      print "done\n"; *)
	h
    end
  
  fun remember0' () =
    case (!lasth) of
      STOREHANDLE(ref (sd as (STOREDELTA(time,_,_,_,_,_)))) =>
	if time = currentTime() then
          !lasth
        else remember' sd
    | STOREHANDLE(ref (sd as NILstoredelta)) =>
	remember' sd

  fun recreate (STORECREATION(a,v,rest)) = 
	  ((*print "recreate\n"; *)
	   ArrayExt.reset(a,v); recreate rest)
    | recreate NILstorecreation = ()
  
  fun reupdate (STOREUPDATE(a,i,v,rest)) = 
	  ((*print "reupdate "; print i; *)
	   (*unsafe*)update(a,i,v); reupdate rest)
    | reupdate NILstoreupdate = ()
  
  (* -- this version condenses update lists against each other where
	  possible (but not update lists against create lists...)  *)
  fun redo (sd as (STOREDELTA (time,cls,ul,sd',rc,wh)),fromTime) =
	if time >= fromTime then
	  let val (oldcls,olduls,oldsd) = redo (sd',fromTime)
	  in (* print ("+redo " ^ (makestring time) ^ "\n"); *)
	     case (rc,strong wh) of
	       (ref 1,NONE) => (cls @ oldcls,ul::olduls,oldsd)
	     | (_,_) =>
		 let val cls' = cls @ oldcls
		     fun markprune (STOREUPDATE(a,i,v,rest),acc) =
			  if not (cast ((*unsafe*) subscript(a,i)) = unique0) then
			    ((*unsafe*) update (a,i,cast unique0);
			     markprune (rest, STOREUPDATE(a,i,v,acc)))
			  else markprune (rest,acc)
		       | markprune (NILstoreupdate,acc) = acc
		     val ul' = 
			 if null olduls then
			    ul 
			 else revfold markprune (ul::olduls) NILstoreupdate
		 in
		   app recreate cls';
		   reupdate ul';
		   (nil,nil,STOREDELTA(time,cls',ul',oldsd,rc,wh))
 		 end
	  end
        else (nil,nil,sd)
     | redo (NILstoredelta,_) = (nil,nil,NILstoredelta)  

  
  (*	-- simple version --  
  fun redo (STOREDELTA (time,cls,ul,sd',_,_),fromTime) =
      if time >= fromTime then
	(redo (sd',fromTime); 
         (*print ("+redo " ^ (makestring time) ^ "\n"); *)
	 app recreate cls; 
	 reupdate ul)
       else ()
    | redo (NILstoredelta,_) = ()
*)
  
  fun restore'(h as (STOREHANDLE(ref sd)),fromTime) =
      (redo(sd,fromTime);
       (* ?? sd := redo(sd,fromTime); *)
       lasth := h;
       createdList := nil;
       updatedAList := PNIL;
       updatedRList := nil)
       
  fun remember () = 
     let val storehandle = remember0'()
	 val savedMark = Log.copyMark mark
	 fun reset () = Log.resetMark mark savedMark
         fun undo _ = ((* print "+undo\n"; *)
		       restore'(storehandle,0);
		       reset())
	 fun redo _ = ((* print "+redo\n";*)
		       restore'(storehandle,currentTime()); 
		       reset())
     in {undo=undo,redo=redo}
     end

end (* struct *)

