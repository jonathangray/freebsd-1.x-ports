(* Copyright 1989 by AT&T Bell Laboratories *)

structure ErrorMsg : ERRORMSG =
struct

  open PrettyPrint Source

 (* error reporting *)

  exception Error  (* was Syntax, changed to Error in 0.92 *)

  datatype severity = WARN | COMPLAIN

  type complainer = severity -> string -> (ppstream -> unit) -> unit

  fun defaultConsumer () =
      {consumer = System.Print.say,
       linewidth = !System.Print.linewidth,
       flush = System.Print.flush}

  val nullErrorBody = (fn (ppstrm: ppstream) => ())

  fun ppmsg(errConsumer,location,severity,msg,body) =
      with_pp errConsumer (fn ppstrm =>
	(begin_block ppstrm CONSISTENT 0;
	 begin_block ppstrm CONSISTENT 2;
	 add_string ppstrm location;
	 add_string ppstrm  (* print error label *)
	    (case severity
	       of WARN => " Warning: "
		| COMPLAIN => " Error: ");
	 add_string ppstrm msg;
	 body ppstrm;
         end_block ppstrm;
	 add_newline ppstrm;
	 end_block ppstrm))

  fun record(COMPLAIN,anyErrors) = anyErrors := true
    | record(WARN,_) = ()

  fun location_string ({fileName,linePos,lineNum,...}:inputSource) (p1,p2) =
      let fun look(p:int,a::rest,n) =
		if a<p then (n,p-a) else look(p,rest,n-1)
	    | look _ = (0,0)
	  val (p1line,p1pos) = look(p1,!linePos,!lineNum)
       in implode(Pathnames.trim fileName :: ":" :: makestring p1line :: "."
		  :: makestring p1pos
		  :: (if p1+1>=p2 then []
		      else let val (p2line,p2pos) = look(p2-1,!linePos,!lineNum)
			    in ["-", makestring p2line, ".", makestring p2pos]
	                   end))
      end


  fun error (source as {anyErrors, errConsumer,...}: inputSource)
            (p1:int,p2:int) (severity:severity)
	    (msg: string) (body : ppstream -> unit) = 
      (ppmsg(errConsumer,(location_string source (p1,p2)),severity,msg,body);
       record(severity,anyErrors))

  fun errorNoFile (errConsumer,anyErrors) ((p1,p2): region) severity msg body = 
      (ppmsg(errConsumer,
	     if p2>0 then implode[makestring p1, "-", makestring p2] else "",
	     severity, msg, body);
       record(severity,anyErrors))

  fun impossible msg =
      (app System.Print.say ["Error: Compiler bug: ",msg,"\n"];
       System.Print.flush();
       raise Error)

  fun impossibleWithBody msg body =
      (with_pp (defaultConsumer()) (fn ppstrm =>
	(add_string ppstrm "Error: Compiler bug: ";
	 add_string ppstrm msg;
	 body ppstrm;
	 add_newline ppstrm));
       raise Error)

  val matchErrorString = location_string

end  (* structure ErrorMsg *)
