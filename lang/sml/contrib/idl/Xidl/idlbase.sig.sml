(*
	SML Base for IDL servers
	Bernard Sufrin,
	Oxford.

	@(#)idlbase.sig.sml	2.1 93/03/07 00:58:11
*)

signature idlbase =
sig
    datatype address = address of int * int;
    type     short;
    type     byte;
    sharing type short = byte = Integer.int


    exception server_input_terminated
    exception server_output_terminated
    val bytein          : unit -> int
    val flush           : unit -> unit
    val infromserver    : instream ref
    val outtoserver     : outstream ref
    val read_address    : unit -> address
    val read_bool       : unit -> bool
    val read_int        : unit -> int
    val read_short      : unit -> short
    val read_byte       : unit -> byte
    val read_string     : unit -> string
    val read_unit       : unit -> unit
    val running         : bool ref
    val servername      : string ref
    val startserver     : string -> string list -> unit
    val stopserver      : unit -> unit
    val write_address   : address -> unit
    val write_bool      : bool -> unit
    val write_int       : int -> unit
    val write_short     : int -> unit
    val write_byte      : byte -> unit
    val write_real      : real -> unit
    val write_string    : string -> unit
    val write_unit      : unit -> unit
end;
