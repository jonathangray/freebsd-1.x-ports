(*$OBJECT *)

signature OBJECT =
sig

(* OBJECT DEFINITION

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        24 Jan 1990

Maintenance:	Author


DESCRIPTION

   An Object provides a type T, functions for comparing values of that
   type, and a function for producing a string representation of that
   type.  A full set of comparison functions is given, not just lt and eq.


SEE ALSO

   EQUALITY, ORDERING, PRINT, EQ_ORD, EQ_PRINT, EQTYPE_ORD, EQTYPE_PRINT,
   SEQUENCE.


RCS LOG

$Log: OBJECT.sml,v $
Revision 1.1  1994/02/08 00:23:27  jkh
Initial revision

Revision 1.9  91/03/06  16:29:18  16:29:18  db (Dave Berry)
Added print function(s).

Revision 1.8  91/02/11  18:53:37  18:53:37  db (Dave Berry)
Moved read and parse functions to PARSE.sml, as part of the major
reorganisation of the library.

Revision 1.7  91/02/04  15:38:46  15:38:46  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.6  91/01/30  18:07:38  18:07:38  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed the parse' functions.

Revision 1.5  91/01/26  13:43:47  13:43:47  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.4  91/01/26  13:19:45  13:19:45  db (Dave Berry)
Renamed RefVectors to Arrays, to match common practice.

Revision 1.3  91/01/25  19:02:47  19:02:47  db (Dave Berry)
Added dependence on InStreamType and/or GeneralTypes.

Revision 1.2  91/01/25  16:55:22  16:55:22  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.1  90/12/17  16:53:09  16:53:09  db (Dave Berry)
Initial revision


*)

(* TYPES *)

  type T


(* CONVERTERS *)

  val string: T -> string
   (* string t; returns the string representation of t. *)

  val print: outstream -> T -> unit
   (* print os x; send the usual string representation of x to
      the stream os. *)


(* OBSERVERS *)

  val eq: T -> T -> bool
   (* eq x y; returns true if x and y are equal.  Usually this is x = y. *)

  val ne: T -> T -> bool
   (* ne x y; returns true if x and y are unequal.  Usually this is x <> y. *)

  val lt: T -> T -> bool
   (* lt x y; returns true if x is less than y. *)

  val le: T -> T -> bool
   (* le x y; returns true if x is less than or equal to y. *)

  val gt: T -> T -> bool
   (* gt x y; returns true if x is less than y. *)

  val ge: T -> T -> bool
   (* ge x y; returns true if x is less than or equal to y. *)

  val fixedWidth: bool
   (* fixedWidth; is true if the usual string representation of type T uses
      a fixed number of characters for all values. *)
end
