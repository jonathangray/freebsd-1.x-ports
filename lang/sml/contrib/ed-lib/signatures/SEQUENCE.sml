(*$SEQUENCE *)

signature SEQUENCE =

(* GENERAL POLYMORPHIC CONSTANT SEQUENCES

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        21 Feb 1989

Maintenance:	Author


DESCRIPTION

   These functions form a general interface for one-dimensional constant
   sequences, such as lists and vectors.

   The stringSep function takes arbitrary strings as its start, finish
   and separating symbols.  The other functions are more limited.

   
SEE ALSO

   OBJECT, SEQ_ORD, SEQ_PARSE.


RCS LOG

$Log: SEQUENCE.sml,v $
Revision 1.1  1994/02/08 00:23:29  jkh
Initial revision

Revision 1.10  91/03/06  16:29:47  16:29:47  db (Dave Berry)
Added print function(s).

Revision 1.9  91/02/12  12:35:45  12:35:45  db (Dave Berry)
Removed reference to old signatures (REF_SEQUENCE and MONO_SEQUENCE)
from the SEE ALSO section.

Revision 1.8  91/02/11  19:23:26  19:23:26  db (Dave Berry)
Moved the ordering functions to SEQ_ORD.sml and the read and parse
functions to SEQ_PARSE.sml, as part of the major reorganisation of the
library.

Revision 1.7  91/02/04  15:38:55  15:38:55  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.6  91/01/30  18:08:18  18:08:18  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed the parse' functions.

Revision 1.5  91/01/26  13:43:57  13:43:57  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.4  91/01/25  19:10:05  19:10:05  db (Dave Berry)
Added dependence on GeneralTypes and/or InStreamType.

Revision 1.3  91/01/25  16:57:28  16:57:28  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/10  18:01:18  18:01:18  db (Dave Berry)
Changed ge and le to expect le and ge functions as parameters instead
of lt and gt (which people found confusing).

Revision 1.1  90/12/17  16:56:20  16:56:20  db (Dave Berry)
Initial revision


*)

sig

(* TYPES *)

  type 'a T


(* OBSERVERS *)

  val eq: ('a -> 'a -> bool) -> 'a T -> 'a T -> bool
   (* eq p x y; returns true if (size x = size y) and for all i,
      0 <= i < size x, (p (x sub i) (y sub i)). *)

  val ne: ('a -> 'a -> bool) -> 'a T -> 'a T -> bool
   (* ne p x y; returns true if (size x <> size y) or there exists
      an i such that 0 <= i < size x and (p (x sub i) (y sub i)). *)


(* CONVERTORS *)

  val stringSep: string -> string -> string ->
		 ('a -> string) -> 'a T -> string
  (* stringSep start finish sep p s; returns the string representation of s,
      beginning with start, ending with finish, and with the elements
      separated by sep. *)

  val string: ('a -> string) -> 'a T -> string
   (* string p l; returns the canonical string representation of l. *)

  val printSep: outstream -> string -> string -> string ->
                (outstream -> 'a -> unit) -> 'a T -> unit
   (* printSep os start finish sep p l; sends the string representation of l
      to the stream os, beginning with start, ending with finish, and with
      the elements separated by sep. *)

  val print: outstream -> (outstream -> 'a -> unit) -> 'a T -> unit
   (* print os p l; sends the canonical string representation of l to
      the stream os. *)

end
