(* Copyright 1989 by AT&T Bell Laboratories *)
signature ERRORMSG =
 sig
    datatype severity = WARN | COMPLAIN
    type complainer (*  = severity -> string -> (PrettyPrint.ppstream -> unit)
		          -> unit *)
    exception Error
    val defaultConsumer : unit -> PrettyPrint.ppconsumer
    val nullErrorBody : PrettyPrint.ppstream -> unit
    val error : Source.inputSource -> Source.region -> complainer
    val matchErrorString : Source.inputSource -> Source.region -> string
    val errorNoFile : PrettyPrint.ppconsumer * bool ref -> Source.region
	              -> complainer
    val impossible : string -> 'a
    val impossibleWithBody : string -> (PrettyPrint.ppstream -> unit) -> 'a
 end
