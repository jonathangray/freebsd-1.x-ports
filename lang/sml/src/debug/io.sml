(* DebugIO
 
   Support "historical" version of IO library. 
*)



signature DEBUG_IO = sig
  val remember: unit -> DebugKernel.doers
  val logit:((unit->'a)*('a->unit)*bool)->'a  
end

structure DebugIO: DEBUG_IO = 
struct
  open DebugUtil DebugStatic DebugKernel

  type echoer = System.Unsafe.object -> unit
  datatype ioresult = NORMAL of System.Unsafe.object * echoer
	            | EXCEPTION of exn

  structure Log = TimedLog(type entry=ioresult)
  val mark = Log.new()
  fun getNext mark = 
      let val (_,entry) = Log.next mark
      in Log.advance mark;
	 entry
      end

  fun remember () = 
     let val savedMark = Log.copyMark mark
         fun undo _  =  Log.resetMark mark savedMark
	 fun redo QUIET = Log.resetMark mark savedMark
	   | redo NOISY = 
	       (* execute echos for all log entries (advancing currmark) 
		  until we hit savedMark! *)
	       (dbgprint "+redo\n";
		while (not(Log.equalMarks (mark,savedMark))) do
		  (case getNext mark of
		     NORMAL (v,g) => g v
		   | EXCEPTION _ => ()))
	   | redo (BREAK _) =
	       debugPanic "IO.can't redo in BREAK mode"
     in {undo=undo,redo=redo}
     end

  fun logit (f:unit->'a,g:'a->unit,noisy:bool) :'a = 
    (if noisy then
	 let val forced =
	       case !execMode of
		 RECORD(BREAK _) => true
	       | REPLAY(BREAK _) => true
               | _ => false
         in pseudoEvent {forced=forced,evn=pseudoEvn IOev,args=[]}
	 end
     else ();
     case (!execMode) of
       RECORD onNoise => 
	      (let val v = f()
	       in Log.append mark (NORMAL(System.Unsafe.cast v,
					  System.Unsafe.cast g));
		  dbgprint ("+app " ^ (makestring (currentTime())) ^ "\n");
		  v
               end handle e => 
		 (Log.append mark (EXCEPTION e);
		  raise e))
     | REPLAY onNoise =>
	     ((case Log.get mark of
		 NORMAL (v,g') =>  (* expect g = g' *)
		     ((case onNoise of
		 	 QUIET => ()
                       | _ =>  (dbgprint ("+redo " ^ 
					  (makestring (currentTime())) ^ "\n");
				g(System.Unsafe.cast v)));
		      System.Unsafe.cast v)
	       | EXCEPTION e => raise e)
		   handle Log.Logtime(t1,t2) =>
		       debugPanic ("IO.logit replay time mismatch" ^
				   (makestring t1) ^ " " ^ (makestring t2)))
      | IGNORE => f()  (* no log *))


end (* structure *)
 
(* Notes:

(1) On replay, appearance of terminal i/o is controlled by the silent flag.
If flag is false, terminal input is echoed to std_out, and terminal
output is performed to std_out. If flag is true, terminal input is not
echoed and terminal output is thrown away.  All non-terminal output is
always thrown away.

The choice of std_out for echoing input is somewhat arbitrary.
Using std_out for output too avoids problems
with closed output streams, although it gives the wrong results when
multiple streams are writing to same terminal due to buffering.

(2) There may be a problem with exceptions being raised in the middle
of I/O operations: on input, this means no input is returned, but
input pointer may have changed (we get this right); on output, some
output may have occured (we get this wrong to terminals).
*)

