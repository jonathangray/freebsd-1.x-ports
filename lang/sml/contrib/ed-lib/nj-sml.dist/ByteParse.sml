(*$ByteParse: PARSE String General Instream	*)

structure ByteParse: PARSE =

(* BYTES

Created by:	Dave Berry LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		22 Sep 1989

Maintenance:	Author

RCS LOG

$Log: ByteParse.sml,v $
Revision 1.1  1994/02/08 00:23:14  jkh
Initial revision

Revision 1.1  91/09/13  14:18:09  14:18:09  db (Dave Berry)
Initial revision



*)

struct


(* TYPES *)

  type T = int


(* OBSERVERS *)

  val fixedWidth = true


(* CONVERTERS *)

  fun parse "" = Fail (None, "")
  |   parse s = OK (ord s, "")

  fun read i =
        case Instream.lookahead i of
          "" => Fail None
  	| s  => (Instream.input1 i; OK (ord s))
end
