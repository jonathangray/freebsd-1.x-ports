(*$AsciiOrdString: FULL_ORD *)

structure AsciiOrdString: FULL_ORD =

(* ASCII STRINGS: CASE SENSITIVE LEXICAL ORDERING

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Feb 1990

Maintenance:	Author

DESCRIPTION

   SML/NJ provides case-sensitive comparison functions for strings.


SEE ALSO

   String.


RCS LOG

$Log: AsciiOrdString.sml,v $
Revision 1.1  1994/02/08 00:23:13  jkh
Initial revision

Revision 1.1  91/09/13  14:15:04  14:15:04  db (Dave Berry)
Initial revision


*)

struct

(* TYPES *)

  type T = string


(* OBSERVERS *)

  fun lt x y = OldString.< (x, y)

  fun le x y = OldString.<= (x, y)

  fun gt x y = OldString.> (x, y)

  fun ge x y = OldString.>= (x, y)

end
