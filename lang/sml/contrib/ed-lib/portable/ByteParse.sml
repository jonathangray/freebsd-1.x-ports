(*$ByteParse: PARSE String General Instream	*)

structure ByteParse: PARSE =

(* BYTES

Created by:	Dave Berry LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		22 Sep 1989

Maintenance:	Author

RCS LOG

$Log: ByteParse.sml,v $
Revision 1.1  1994/02/08 00:23:18  jkh
Initial revision

Revision 1.7  91/02/11  19:58:04  19:58:04  db (Dave Berry)
Changed the name of this structure from ByteObject to ByteParse.  Moved
comparison functions and string function to Byte.sml.  This forms part
of the major reorganisation of the library.

Revision 1.6  91/02/05  11:06:18  11:06:18  db (Dave Berry)
Changed read functions slightly to use new definition of Instream.eof.

Revision 1.5  91/02/04  15:10:43  15:10:43  db (Dave Berry)
InStream and OutSream renamed to Instream and OutStream, as part of  the
reorganisation of the stream entries.

Revision 1.4  91/01/30  17:42:54  17:42:54  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed parse' functions.

Revision 1.3  91/01/25  20:22:34  20:22:34  db (Dave Berry)
Changed signature names to all upper case.
Added tag declaration.

Revision 1.2  91/01/24  17:20:59  17:20:59  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:51:14  14:51:14  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type T = string


(* OBSERVERS *)

  val fixedWidth = true


(* CONVERTERS *)

  fun parse "" = Fail (None, "")
  |   parse s = OK (String.sub (s, 0), String.extract 1 (size s) s)

  fun read i =
        case Instream.lookahead i of
          "" => Fail None
  	| s  => (Instream.input1 i; OK s)
end
