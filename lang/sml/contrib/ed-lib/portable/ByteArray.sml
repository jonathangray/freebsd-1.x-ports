(*$ByteArray : Byte MonoArray *)

structure ByteArray = MonoArray (
  structure Element = Byte
);

(* BYTE ARRAYS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           22 Sep 1989

Maintenance:    Author

RCS LOG

$Log: ByteArray.sml,v $
Revision 1.1  1994/02/08 00:23:18  jkh
Initial revision

Revision 1.3  91/02/11  19:54:17  19:54:17  db (Dave Berry)
Renamed Object to Element, since it isn't an OBJECT anymore.  This forms
part of the major reorganisation of the library.

Revision 1.2  91/01/26  15:13:50  15:13:50  db (Dave Berry)
Renamed RefVector and REF_VECTOR to Array and ARRAY, respectively.

Revision 1.1  90/12/20  14:51:34  14:51:34  db (Dave Berry)
Initial revision


*)

