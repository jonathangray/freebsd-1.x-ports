(*$ByteVector : Byte MonoVector *)

structure ByteVector = MonoVector (
  structure Element = Byte
);

(* BYTE VECTORS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           22 Sep 1989

Maintenance:    Author

RCS LOG

$Log: ByteVector.sml,v $
Revision 1.1  1994/02/08 00:23:18  jkh
Initial revision

Revision 1.2  91/02/11  19:56:20  19:56:20  db (Dave Berry)
Renamed Object to Element, since it isn't an OBJECT anymore.  This forms
part of the major reorganisation of the library.

Revision 1.1  90/12/20  14:51:49  14:51:49  db (Dave Berry)
Initial revision


*)

