(*$VectorParse : SEQ_PARSE Vector ListParse Instream Outstream *)

structure VectorParse: SEQ_PARSE =

(* CONSTANT VECTORS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		4 Oct 1989

Maintenance:	Author


SEE ALSO

   Vector, ArrayParse.


RCS LOG

$Log: VectorParse.sml,v $
Revision 1.1  1994/02/08 00:23:21  jkh
Initial revision

Revision 1.10  91/02/22  19:11:08  19:11:08  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.

Revision 1.9  91/02/11  20:57:17  20:57:17  db (Dave Berry)
Changed the name of this structure from Vector to VectorParse.
Moved the string and stringSep functions to Vector.sml.
Removed the inclusion of Vector'.
Changed several references to functions in other structures to track
their movements between structures.
This forms part of the major reorganisation of the library.

Revision 1.8  91/02/04  15:10:59  15:10:59  db (Dave Berry)
InStream and OutStream renamed to Instream and OutStream, as part of  the
reorganisation of the stream entries.

Revision 1.7  91/01/30  17:43:14  17:43:14  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed parse' functions.

Revision 1.6  91/01/28  12:59:54  12:59:54  db (Dave Berry)
Changed implementation to use CoreVector instead of List.

Revision 1.5  91/01/26  15:14:00  15:14:00  db (Dave Berry)
Renamed RefVector and REF_VECTOR to Array and ARRAY, respectively.

Revision 1.4  91/01/25  20:21:48  20:21:48  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.3  91/01/24  17:28:09  17:28:09  db (Dave Berry)
Removed version value.

Revision 1.2  91/01/15  10:59:27  10:59:27  db (Dave Berry)
Renamed "empty" function to "null", added "empty" constant.

Revision 1.1  90/12/20  15:53:39  15:53:39  db (Dave Berry)
Initial revision

*)

struct

  type 'a T = 'a Vector.Vector

  exception Sep of string * string * string * string

  exception Size of string * int

  fun parseSepN start finish sep p n s =
        ( case ListParse.parseSepN start finish sep p n s of
	    OK (l, s') => OK (Vector.fromList l, s')
	  | Fail (Some l, s') => Fail (Some (Vector.fromList l), s')
	  | Fail (None, s') => Fail (None, s')
        )
	handle ListParse.Sep x => raise Sep x

  fun parseSep start finish sep p s =
	( case ListParse.parseSep start finish sep p s of
	    OK (l, s') => OK (Vector.fromList l, s')
	  | Fail (Some l, s') => Fail (Some (Vector.fromList l), s')
	  | Fail (None, s') => Fail (None, s')
	)
	handle ListParse.Sep x => raise Sep x

  (* The parse functions assume that entries
     are separated by formatting characters. *)

  fun parseN p n s =
	if n < 0 then raise Size ("parseN", n)
	else parseSepN "" "" "" p n s

  fun parse p s = parseSep "" "" "" p s

  fun readSepN start finish sep p n i =
	( case ListParse.readSepN start finish sep p n i of
	    OK l  => OK (Vector.fromList l)
	  | Fail (Some l) => Fail (Some (Vector.fromList l))
	  | Fail None => Fail None
	)
	handle ListParse.Sep x => raise Sep x

  fun readSep start finish sep p i =
	case ListParse.readSep start finish sep p i of
	  OK l  => OK (Vector.fromList l)
	| Fail (Some l) => Fail (Some (Vector.fromList l))
	| Fail None => Fail None
	handle ListParse.Sep x => raise Sep x

  (* The read functions assume that entries
     are separated by formatting characters. *)

  fun readN p n i =
	if n < 0 then raise Size ("readN", n)
	else readSepN "" "" "" p n i

  fun read p i = readSep "" "" "" p i

  fun fromFile p name =
	let fun readList i =
		  case p i
		  of Fail _ => (Instream.closeIn i; [])
		  |  OK x => x :: readList i
	in Vector.fromList (readList (Instream.openIn name))
	end

  fun file p v name =
	let val os = Outstream.openOut name
	    fun out s = Outstream.output (os, s)
	in Vector.apply (out o p) v;
	   Outstream.closeOut os
	end

end
