(*$System : SYSTEM General *)

loadSig "SYSTEM";

structure System: SYSTEM =

(* SYSTEM FUNCTIONS FOR POLY/ML

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk

Date:	        14 Nov 1989

Maintenance:	Author


DESCRIPTION

   Poly/ML provides quit and cd functions.


RCS LOG

$Log: System.sml,v $
Revision 1.1  1994/02/08 00:23:15  jkh
Initial revision

Revision 1.2  91/04/10  16:58:08  16:58:08  db (Dave Berry)
Added polymorphic hash function.

Revision 1.1  91/01/10  13:35:59  13:35:59  db (Dave Berry)
Initial revision


*)

struct


(* ML SYSTEM *)

  val quit = PolyML.quit

  fun collect () = ();

  fun eq x y = false;

  fun hash _ = raise General.NotImplemented "hash"


(* ML SOURCE FILES *)

  val use = NonStandard.use


(* INTERFACE TO OPERATING SYSTEM *)

  exception NoFile of string * string

  exception Permission of string * string

  val cd = PolyML.cd

  fun isDir _ = raise General.NotImplemented "isDir"

  fun pwd _ = raise General.NotImplemented "pwd"

  fun dir _ = raise General.NotImplemented "dir"

  fun delete _ = raise General.NotImplemented "delete"

  fun system _ = raise General.NotImplemented "system"

  fun getenv _ = raise General.NotImplemented "getenv"

end
