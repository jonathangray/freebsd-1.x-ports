(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure Interrupt :INTERRUPT = struct

  (* This function applies operation to ().  If it handles an interrupt
     signal (Control-C), it raises the exception Interrupt. Example:
       (handleInterrupt foo) handle Interrupt => print "Bang!\n"
  *)

  fun handleInterrupt (operation : unit -> unit) =
    let exception Done
        val old'handler = System.Signals.inqHandler(System.Signals.SIGINT)
        fun reset'handler () =
          System.Signals.setHandler(System.Signals.SIGINT, old'handler)
     in ( callcc (fn k =>
           ( System.Signals.setHandler(System.Signals.SIGINT,SOME(fn _ => k)) ;
             operation ();
             raise Done )) ;
          (* print ("\n--- interrupt ---\n"); *)
          raise Interrupt )
        handle Done => (reset'handler ())
             | exn  => (reset'handler (); raise exn)
    end
end
