(* user-level redefinition of io for history *)
		
structure HistoricalIO : IO =
struct
  val logit:((unit->'a)*('a->unit)*bool)->'a =
	System.Unsafe.cast (!System.Control.Debug.interface 24)
   (* arguments are:
       - thunk to execute if we're recording
       - "echo" function to execute on stored result if we're replaying
       - flag: true if this is a "noisy" operation that should be avoided
               if we're executing speculatively. *)

  fun echo (s:string) =    (* for now, to std_out *)
     IO.outputc (IO.std_out) s

  fun flush () = (* for now, to std_out (to match echo) *)
     IO.flush_out IO.std_out

  fun echof (s:string) =   (* for now, to std_out *)
     (IO.outputc IO.std_out s;
      IO.flush_out IO.std_out)

  val noecho = fn x => ()  

  open IO

  fun open_in s : instream = logit (fn () => IO.open_in s, noecho, true)
  fun open_out s : outstream = logit (fn () => IO.open_out s, noecho, true)
  fun open_append s : outstream = logit (fn () => IO.open_append s, 
					 noecho, true)
  fun open_string s : instream = logit (fn () => IO.open_string s, 
					noecho, false)
  fun close_in is : unit =  logit (fn () => IO.close_in is, noecho, true)
  fun close_out os : unit =  logit (fn () => IO.close_out os, noecho, true)
  fun outputc os s : unit = logit (fn () => IO.outputc os s,
				  if (IO.is_term_out os) then 
				    fn _ => echo s 
				  else noecho,
				  true)
  fun output(os,s) : unit = logit (fn () => IO.output(os,s),
				  if (IO.is_term_out os) then 
				    fn _ => echo s 
				  else noecho,
				  true)
  fun input(is,i) : string = let val b = IO.is_term_in is
			    in logit (fn () => IO.input(is,i),
				      if b then echof else noecho,
				      b)
			    end
  fun inputc is i : string = let val b = IO.is_term_in is
			    in logit (fn () => IO.inputc is i,
				      if b then echof else noecho,
				      b)
			    end
  fun input_line is : string = let val b = IO.is_term_in is
			       in logit (fn () => IO.input_line is,
					 if b then echof else noecho,
					 b)
			       end
  fun lookahead is : string = logit (fn () => IO.lookahead is,
				     noecho,  (* somewhat arbitrary *)
				     IO.is_term_in is)
  fun end_of_stream is : bool = logit (fn () => IO.end_of_stream is, noecho,
								false)
  fun can_input is : int = logit (fn () => IO.can_input is, noecho, true)
  fun flush_out os : unit = logit (fn () => IO.flush_out os, 
				   if IO.is_term_out os then 
				     fn _ => flush()
				   else noecho,
 				   true)
end





