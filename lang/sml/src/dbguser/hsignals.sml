(*
signature SIGNALS =
  sig
    datatype signal
      = SIGHUP | SIGINT | SIGQUIT | SIGALRM | SIGTERM | SIGURG
      | SIGCHLD | SIGIO | SIGWINCH | SIGUSR1 | SIGUSR2
      | SIGTSTP | SIGCONT (* not yet supported *)
      | SIGGC
    val setHandler : (signal * ((int * unit cont) -> unit cont) option) -> unit
    val inqHandler : signal -> ((int * unit cont) -> unit cont) option
    val maskSignals : bool -> unit
    val pause : unit -> unit
	(* sleep until the next signal *)
  end
*)

structure HistoricalSignals  (* : SIGNALS *) =
struct
  open System.Signals (* for datatype signal *)
  val (setHandler:(signal * ((int * unit cont) -> unit cont) option) -> unit,
       inqHandler:signal -> ((int * unit cont) -> unit cont) option,
       maskSignals:bool->unit,
       pause:unit->unit) = 
          System.Unsafe.cast (!System.Control.Debug.interface 19)
end (* structure *)

