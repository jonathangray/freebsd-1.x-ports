(*$System : SYSTEM General *)

loadSig "SYSTEM";

structure System: SYSTEM =

(* SYSTEM FUNCTIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk

Date:	        14 Nov 1989

Maintenance:	Author


RCS LOG

$Log: System.sml,v $
Revision 1.1  1994/02/08 00:23:17  jkh
Initial revision

Revision 1.5  91/04/10  16:57:14  16:57:14  db (Dave Berry)
Added polymorphic hash function.

Revision 1.4  91/01/25  20:21:42  20:21:42  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.3  91/01/25  16:02:09  16:02:09  db (Dave Berry)
Added isDir function.

Revision 1.2  91/01/24  17:28:04  17:28:04  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  15:52:47  15:52:47  db (Dave Berry)
Initial revision


*)

struct


(* ML SYSTEM *)

  fun quit () = raise General.NotImplemented "quit";

  fun collect () = ();

  fun eq x y = false;

  fun hash _ = raise General.NotImplemented "hash";


(* INTERFACE TO OPERATING SYSTEM *)

  exception NoFile of string * string

  exception Permission of string * string

  val use = NonStandard.use

  fun cd _ = raise General.NotImplemented "cd"

  fun isDir _ = raise General.NotImplemented "isDir"

  fun pwd _ = raise General.NotImplemented "pwd"

  fun dir _ = raise General.NotImplemented "dir"

  fun delete _ = raise General.NotImplemented "delete"

  fun system _ = raise General.NotImplemented "system"

  fun getenv _ = raise General.NotImplemented "getenv"

end
