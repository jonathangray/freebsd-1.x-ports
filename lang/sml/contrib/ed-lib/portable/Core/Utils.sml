loadLocalSig "UTILS";

structure CoreUtils: CORE_UTILS =

(* CORE UTILITY FUNCTIONS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           24 Jan 1991

Maintenance:    Author



RCS LOG

$Log: Utils.sml,v $
Revision 1.1  1994/02/08 00:23:23  jkh
Initial revision

Revision 1.5  91/02/12  17:27:03  17:27:03  db (Dave Berry)
Removed listEq.

Revision 1.4  91/01/30  18:03:10  18:03:10  db (Dave Berry)
Removed DropPrefix function.  This is no longer used by the various
parse functions in the library.

Revision 1.3  91/01/25  20:00:42  20:00:42  db (Dave Berry)
Moved inputLine to Make/Global.

Revision 1.2  91/01/25  15:44:39  15:44:39  db (Dave Berry)
Moved option, substring and fold to Make/Global.
Deleted old version of member, replaced it with eqMember.

Revision 1.1  91/01/25  11:29:41  11:29:41  db (Dave Berry)
Initial revision


*)

struct
  infix before

  fun x before _ = x;

  fun unzip []           = ([] ,[])
  |   unzip ((x,y)::xys) =
        let val (xs, ys) = unzip xys
	in (x :: xs, y :: ys)
	end

  local
    fun string' 0 = "0"
    |   string' 1 = "1"
    |   string' 2 = "2"
    |   string' 3 = "3"
    |   string' 4 = "4"
    |   string' 5 = "5"
    |   string' 6 = "6"
    |   string' 7 = "7"
    |   string' 8 = "8"
    |   string' 9 = "9"
    |   string' n = string' (n div 10) ^ string' (n mod 10)
  in
    fun intToString n = if n < 0 then "~" ^ string' (~n) else string' n
  end

  fun length [] = 0
  |   length (_::t) = 1 + length t

  fun member _ [] = false
  |   member x (h::t) =
	x = h orelse member x t

  open NonStandard
end;

