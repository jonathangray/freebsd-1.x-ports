(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Copyright 1989 by AT&T Bell Laboratories *)
signature ERRORMSG =
 sig
    exception Syntax
    exception Cascade of string
    datatype severity = WARN | COMPLAIN | CONDEMN | CASCADE | BUG
    type pos (* = int *)
    type pos2 (* = pos * pos *)
    type complainer (*  = severity -> string -> unit *)
    type pos2complainer (* = pos2 -> complainer *)
    type inputSource (* = {fileName: string,  linePos: int list ref,
	  		    lineNum: int ref, anyErrors: bool ref,
			    errStream: outstream, interactive: bool,
			    sourceStream: instream, 
			    indexStream: outstream option} *)
    val newSource: string * instream * bool * outstream * outstream option 
		    -> inputSource
    val closeSource: inputSource -> unit
    val filepos: inputSource -> int -> string * int * int
    val error:  inputSource -> pos2 -> complainer
    val say : string -> unit
    val warn : string -> unit
    val complain : string -> unit
    val impossible : string -> 'a
    val initialErr: string -> instream ->
                      (pos2complainer * ((string * pos * pos) -> unit))
 end
