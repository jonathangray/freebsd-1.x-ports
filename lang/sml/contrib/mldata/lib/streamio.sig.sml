








signature streamio =
sig
    val instream        : instream ref            (* current input stream *)
    val outstream       : outstream ref           (* current output stream *)
    val errstream       : outstream ref           (* current error stream *)
    val SwapStream      : 'a ref -> 'a -> 'a
    val eof             : unit -> bool            (* instream is at end of file *)
    val readch          : unit -> string          (* read character from instream *)
    val readline        : unit -> string          (* read line from instream *)
    val writestring     : string -> unit          (* write string to outstream *)
    val write           : string -> unit          (* write string to outstream *)
    val writebool       : bool -> unit
    val writeint        : int -> unit
    val writereal       : real -> unit
    val writelist       : ('a -> 'b) -> string -> 'a list -> unit
    val writeoption     : ('a -> 'b) -> string -> 'a option -> unit
    val writef          : ('a -> 'b) -> string -> 'a list -> unit
end
