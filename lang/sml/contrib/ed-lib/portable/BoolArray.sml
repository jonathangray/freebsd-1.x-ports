(*$BoolArray : Bool MonoArray *)

structure BoolArray = MonoArray (
  structure Element = Bool
);

(* BOOLEAN ARRAYS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           22 Sep 1989

Maintenance:    Author

RCS LOG

$Log: BoolArray.sml,v $
Revision 1.1  1994/02/08 00:23:17  jkh
Initial revision

Revision 1.3  91/02/11  19:53:29  19:53:29  db (Dave Berry)
Renamed Object to Element, since it isn't an OBJECT anymore.  This forms
part of the major reorganisation of the library.

Revision 1.2  91/01/26  15:13:47  15:13:47  db (Dave Berry)
Renamed RefVector and REF_VECTOR to Array and ARRAY, respectively.

Revision 1.1  90/12/20  14:50:04  14:50:04  db (Dave Berry)
Initial revision


*)
