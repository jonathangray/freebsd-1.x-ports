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
Revision 1.1  1994/02/08 00:23:17  jkh
Initial revision

Revision 1.6  91/03/06  16:38:10  16:38:10  db (Dave Berry)
Added print function(s).

Revision 1.5  91/02/11  19:54:52  19:54:52  db (Dave Berry)
Removed Object sub-structure.  Added type synonym T, string function and
comparison functions.  This forms part of the major reorganisation of
the library.

Revision 1.4  91/01/30  19:01:15  19:01:15  db (Dave Berry)
Renamed loadFun and loadStr to loadEntry.

Revision 1.3  91/01/25  20:16:56  20:16:56  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:20:55  17:20:55  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:50:56  14:50:56  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type Byte = string
  type T = Byte


(* OBSERVERS *)

  fun lt x y = ord x <  ord y
  fun gt x y = ord x >  ord y
  fun le x y = ord x <= ord y
  fun ge x y = ord x >= ord y
  fun eq x y = ord x =  ord y
  fun ne x y = ord x <> ord y

  val fixedWidth = true


(* CONVERTERS *)

  val string = General.id;

  val print = General.curry output

end
