(*$LexOrdString: String FULL_ORD *)

structure LexOrdString: FULL_ORD =

(* ASCII STRINGS: CASE-INSENSITIVE LEXICAL ORDERING

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Feb 1990

Maintenance:	Author


DESCRIPTION

   Comparison functions on strings, using case-insensitive lexcial ordering.


SEE ALSO

   AsciiOrdString, STRING, STRING_TYPE.


RCS LOG

$Log: LexOrdString.sml,v $
Revision 1.1  1994/02/08 00:23:19  jkh
Initial revision

Revision 1.3  91/02/11  20:06:04  20:06:04  db (Dave Berry)
Changed the name of this structure from StringObject' to LexOrdString.
Changed the implementation so that it doesn't rely on the List structure.
 This forms part of the major reorganisation of the library.

Revision 1.2  91/01/25  20:24:11  20:24:11  db (Dave Berry)
Changed signature names to all upper case.
Added tag declaration.

Revision 1.1  90/12/20  15:51:56  15:51:56  db (Dave Berry)
Initial revision


*)

struct

(* TYPES *)

  type T = string


(* OBSERVERS *)

  infix 9 sub
  val op sub = String.sub

  local
    fun lt' index x y =
	  if index >= String.size y then false
	  else if index >= String.size x then true
	  else if ord (String.lower (x sub index)) <
		  ord (String.lower (y sub index))
	  then true
	  else if ord (String.lower (x sub index)) =
		  ord (String.lower (y sub index))
	  then lt' (index + 1) x y
	  else false
  in
    fun lt x y = lt' 0 x y
  end

  local
    fun le' index x y =
	  if index >= String.size x then true
	  else if index >= String.size y then false
	  else if ord (String.lower (x sub index)) <
		  ord (String.lower (y sub index))
	  then true
	  else if ord (String.lower (x sub index)) =
		  ord (String.lower (y sub index))
	  then le' (index + 1) x y
	  else false
  in
    fun le x y = le' 0 x y
  end

  local
    fun gt' index x y =
	  if index >= String.size x then false
	  else if index >= String.size y then true
	  else if ord (String.lower (x sub index)) <
		  ord (String.lower (y sub index))
	  then true
	  else if ord (String.lower (x sub index)) =
		  ord (String.lower (y sub index))
	  then gt' (index + 1) x y
	  else false
  in
    fun gt x y = gt' 0 x y
  end

  local
    fun ge' index x y =
	  if index >= String.size y then true
	  else if index >= String.size x then false
	  else if ord (String.lower (x sub index)) <
		  ord (String.lower (y sub index))
	  then true
	  else if ord (String.lower (x sub index)) =
		  ord (String.lower (y sub index))
	  then ge' (index + 1) x y
	  else false
  in
    fun ge x y = ge' 0 x y
  end

end
