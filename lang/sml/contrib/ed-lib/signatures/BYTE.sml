(*$BYTE *)

signature BYTE =
sig

(* BYTES

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:     	22 Sep 1989

Maintenance:	Author

DESCRIPTION

   Functions on 8-bit bytes.


NOTES

   This exists mainly to support ByteVectors and ByteArrays.


RCS LOG

$Log: BYTE.sml,v $
Revision 1.1  1994/02/08 00:23:24  jkh
Initial revision

Revision 1.11  91/03/06  16:28:45  16:28:45  db (Dave Berry)
Added print function(s).

Revision 1.10  91/02/12  12:17:58  12:17:58  db (Dave Berry)
Changed type to eqtype.

Revision 1.9  91/02/11  18:18:01  18:18:01  db (Dave Berry)
Removed the read and parse functions, and the Object substructure, as part
of the major reorganisation of the library.

Revision 1.8  91/02/04  15:38:29  15:38:29  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.7  91/01/30  18:07:24  18:07:24  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed the parse' functions.

Revision 1.6  91/01/26  13:17:46  13:17:46  db (Dave Berry)
Renamed RefVectors to Arrays, to match common practice.

Revision 1.5  91/01/25  19:30:41  19:30:41  db (Dave Berry)
Added dependence on OBJECT, fixed include specification.

Revision 1.4  91/01/25  19:02:18  19:02:18  db (Dave Berry)
Added dependence on InStreamType and/or GeneralTypes.

Revision 1.3  91/01/25  16:54:43  16:54:43  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:06:06  17:06:06  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:47:04  16:47:04  db (Dave Berry)
Initial revision


*)


(* TYPES *)

  eqtype Byte

  eqtype T
    sharing type T = Byte


(* CONVERTERS *)

  val string: Byte -> string

  val print: outstream -> Byte -> unit


(* OBSERVERS *)

  val eq: Byte -> Byte -> bool

  val ne: Byte -> Byte -> bool

  val lt: Byte -> Byte -> bool

  val le: Byte -> Byte -> bool

  val gt: Byte -> Byte -> bool

  val ge: Byte -> Byte -> bool

  val fixedWidth: bool
   (* fixedWidth = true *)

end
