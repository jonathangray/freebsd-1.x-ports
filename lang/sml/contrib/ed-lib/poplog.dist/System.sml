(*$System : SYSTEM General *)

loadSig "SYSTEM";

(* SYSTEM FUNCTIONS FOR POPLOG ML

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk

Date:	        26 Mar 1991

Maintenance:	Author


DESCRIPTION

   Poplog ML provides quit and cd functions.


RCS LOG

$Log: System.sml,v $
Revision 1.1  1994/02/08 00:23:16  jkh
Initial revision

Revision 1.1  91/04/10  16:59:33  16:59:33  db (Dave Berry)
Initial revision



*)

external structure OS = struct

ml_exception Error of string * string * string list;
lconstant Error = ml_valof("Error");

define lconstant error(msg, culprits, fn);
        lvars msg, culprits, fn;
        ml_raise(Error(ml_constuple(fn, msg, [%
                for msg in culprits do msg sys_>< '' endfor
        %], 3)));
end;

ml_val pwd : unit -> string =
procedure(unit);
        lvars   unit;
        dlocal  prmishap = error(% "pwd" %);
        current_directory;
endprocedure;

ml_val cd : string -> unit =
procedure(dir);
        lvars   dir;
        dlocal  prmishap = error(% "cd" %);
        dir -> current_directory;
        ml_unit;
endprocedure;

ml_val isDir : string -> bool =
procedure(s);
        lvars   s;
        dlocal  prmishap = error(% "isDir" %);
        sysisdirectory(s);
endprocedure;

end;


structure System: SYSTEM =


struct



(* ML SYSTEM *)

  val quit = PML.System.exit

  fun collect () = PML.System.Memory.gc ();

  fun eq x y = false;

  fun hash _ = raise General.NotImplemented "hash"


(* ML SOURCE FILES *)

  val use = NonStandard.use


(* INTERFACE TO OPERATING SYSTEM *)

  exception NoFile of string * string

  exception Permission of string * string

  (* The Poplog exceptions don't match the ones that I've defined,
     but this is better than nothing. *)
  fun cd d = OS.cd d
	     handle OS.Error _ => raise NoFile ("cd", d)

  fun isDir d = OS.isDir d
	     handle OS.Error _ => raise NoFile ("isDir", d)

  fun pwd () = OS.pwd ()

  fun dir _ = raise General.NotImplemented "dir"

  fun delete _ = raise General.NotImplemented "delete"

  fun system _ = raise General.NotImplemented "system"

  fun getenv _ = raise General.NotImplemented "getenv"

end
