(* source.sml *)

structure Source : SOURCE =
struct

  (* character position in a file *)
  type charpos = int

  (* region in a file delineated by two character positions, start and end
     respectively. *)
  type region = charpos * charpos

  type inputSource =
         {fileName: string,
	  linePos: int list ref,
	  lineNum: int ref,
	  anyErrors: bool ref,
	  errConsumer: PrettyPrint.ppconsumer,
	  interactive: bool,
	  sourceStream: instream, 
	  indexStream: outstream option}

  fun newSource(fileName,lineNum,sourceStream,interactive,
		errConsumer,indexStream) =
      {fileName=fileName,sourceStream=sourceStream,interactive=interactive,
       errConsumer=errConsumer,linePos=ref[1],lineNum=ref lineNum,
       anyErrors=ref false, indexStream=indexStream}

  fun closeSource({fileName,sourceStream,interactive,
		    errConsumer,indexStream,...}: inputSource): unit =
      (if interactive then () 
       else close_in sourceStream handle Io _ => ();
       case indexStream
	of SOME f => (close_out f handle Io _ => ())
	 | NONE => ())

  fun filepos({fileName,linePos,lineNum,...}: inputSource) p =
      let fun look(p:int,a::rest,n) = 
	      if a<p then (fileName,n,p-a) else look(p,rest,n-1)
	    | look _ = (fileName,0,0)
       in look(p,!linePos,!lineNum)
      end

end (* structure Source *)
