(*$Byte : BYTE *)

loadSig "BYTE";

structure Byte: BYTE =

(* BYTES

Created by:	Dave Berry LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		22 Sep 1989

Maintenance:	Author

RCS LOG

$Log: Byte.sml,v $
Revision 1.1  1994/02/08 00:23:13  jkh
Initial revision

Revision 1.1  91/09/13  14:16:15  14:16:15  db (Dave Berry)
Initial revision



*)

struct


(* TYPES *)

  type Byte = int
  type T = Byte


(* OBSERVERS *)

  val lt = Int.lt
  val gt = Int.gt
  val le = Int.le
  val ge = Int.ge
  val eq = Int.eq
  val ne = Int.ne

  val fixedWidth = true


(* CONVERTERS *)

  fun string b = chr b

  fun print os b = output (os, string b)

end
