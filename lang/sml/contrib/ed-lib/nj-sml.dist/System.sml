(*$System : SYSTEM General *)

loadSig "SYSTEM";

structure System: SYSTEM =

(* SYSTEM FUNCTIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk

Date:	        14 Nov 1989

Maintenance:	Author


DESCRIPTION

   SML/NJ implements the cd, pwd, dir, quit and system functions.


RCS LOG

$Log: System.sml,v $
Revision 1.1  1994/02/08 00:23:13  jkh
Initial revision

Revision 1.1  91/09/13  14:26:19  14:26:19  db (Dave Berry)
Initial revision



*)

struct


(* ML SYSTEM *)

  val quit = SML_NJ.Unsafe.CleanUp.shutdown;

  fun collect () = ();

  fun eq x y = false;

  fun hash _ = raise General.NotImplemented "hash";


(* INTERFACE TO OPERATING SYSTEM *)

  exception NoFile of string * string

  exception Permission of string * string

  val use = NonStandard.use

  val cd = SML_NJ.Directory.cd

  fun isDir _ = raise General.NotImplemented "isDir"

  val pwd = SML_NJ.Directory.getWD

  val dir = SML_NJ.Directory.listDir

  fun delete _ = raise General.NotImplemented "delete"

  fun system s = raise General.NotImplemented "system"

  fun getenv _ = raise General.NotImplemented "getenv"

end
