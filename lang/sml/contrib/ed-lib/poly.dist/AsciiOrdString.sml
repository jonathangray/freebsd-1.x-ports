(*$AsciiOrdString: FULL_ORD *)

structure AsciiOrdString: FULL_ORD =

(* ASCII STRINGS: CASE SENSITIVE LEXICAL ORDERING

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        17 Sep 1991

Maintenance:	Author

DESCRIPTION

   Poly/ML provides case-sensitive comparison functions for strings.


SEE ALSO

   String.


RCS LOG

$Log: AsciiOrdString.sml,v $
Revision 1.1  1994/02/08 00:23:15  jkh
Initial revision

# Revision 1.1  1991/10/22  17:54:11  db
# Initial revision
#


*)

struct

(* TYPES *)

  type T = string


(* OBSERVERS *)

  fun lt x y = PolyML.StringBuiltIns.lt (x, y)

  fun le x y = PolyML.StringBuiltIns.ge (x, y)

  fun gt x y = PolyML.StringBuiltIns.gt (x, y)

  fun ge x y = PolyML.StringBuiltIns.le (x, y)

end
