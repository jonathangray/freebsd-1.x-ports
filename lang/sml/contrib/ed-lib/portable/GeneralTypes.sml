(*$GeneralTypes: GENERAL_TYPES General *)

loadEntry "General";
loadSig "GENERAL_TYPES";
structure Types: GENERAL_TYPES = General;
open Types;
infix 3 oo;
infix 0 before;
structure Types = struct end;

(* GENERAL TYPES AND EXCEPTIONS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           22 Sep 1989

Maintenance:    Author

RCS LOG

$Log: GeneralTypes.sml,v $
Revision 1.1  1994/02/08 00:23:17  jkh
Initial revision

Revision 1.4  91/02/12  18:14:50  18:14:50  db (Dave Berry)
The GENERAL_TYPES signature now includes the infix functions defined in
General.sml, so this entry makes them infix at top-level.

Revision 1.3  91/01/30  19:01:18  19:01:18  db (Dave Berry)
Renamed loadFun and loadStr to loadEntry.

Revision 1.2  91/01/25  20:17:14  20:17:14  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.1  90/12/20  14:52:44  14:52:44  db (Dave Berry)
Initial revision


*)

