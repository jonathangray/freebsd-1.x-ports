(*$Const: CONST *)

loadSig "CONST";

structure Const: CONST =

(* TAGGED VALUES

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk

Date:		12 Dec 1989

Maintenance:	Author


NOTES

   An ML implementation could incorporate this package as equality on
   addresses.


RCS LOG

$Log: Const.sml,v $
Revision 1.1  1994/02/08 00:23:17  jkh
Initial revision

Revision 1.4  91/02/11  19:59:36  19:59:36  db (Dave Berry)
Renamed Create to create.

Revision 1.3  91/01/25  20:17:09  20:17:09  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:21:05  17:21:05  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:52:04  14:52:04  db (Dave Berry)
Initial revision

*)

struct


(* TYPES *)

  datatype 'a Const = Const of 'a * (unit ref)


(* CREATORS *)

  fun create x = Const (x, ref ())


(* OBSERVERS *)

  fun eq (Const (_, t1)) (Const (_, t2)) = (t1 = t2)

  fun ne (Const (_, t1)) (Const (_, t2)) = (t1 <> t2)


(* SELECTORS *)

  fun !! (Const (x, _)) = x

end

