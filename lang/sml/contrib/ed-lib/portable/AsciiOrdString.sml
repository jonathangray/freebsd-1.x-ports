(*$AsciiOrdString: String FULL_ORD *)

structure AsciiOrdString: FULL_ORD =

(* ASCII STRINGS: CASE SENSITIVE LEXICAL ORDERING

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Feb 1990

Maintenance:	Author


DESCRIPTION

   Comparison functions for strings, using case-sensitive lexical ordering.


SEE ALSO

   LexOrdString, STRING, STRING_TYPE.


RCS LOG

$Log: AsciiOrdString.sml,v $
Revision 1.1  1994/02/08 00:23:17  jkh
Initial revision

Revision 1.3  91/02/11  19:50:13  19:50:13  db (Dave Berry)
Changed the name of this signature to AsciiOrdString from StringObject.
Changed the implementation so that it no longer depends on the List (a.k.a.
List') structure.
This forms part of the major reorganisation of the library.

Revision 1.2  91/01/25  20:23:50  20:23:50  db (Dave Berry)
Changed signature names to all upper case.
Added tag declaration.

Revision 1.1  90/12/20  15:49:31  15:49:31  db (Dave Berry)
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
	  else if ord (x sub index) <
		  ord (y sub index)
	  then true
	  else if ord (x sub index) =
		  ord (y sub index)
	  then lt' (index + 1) x y
	  else false
  in
    fun lt x y = lt' 0 x y
  end

  local
    fun le' index x y =
	  if index >= String.size x then true
	  else if index >= String.size y then false
	  else if ord (x sub index) <
		  ord (y sub index)
	  then true
	  else if ord (x sub index) =
		  ord (y sub index)
	  then le' (index + 1) x y
	  else false
  in
    fun le x y = le' 0 x y
  end

  local
    fun gt' index x y =
	  if index >= String.size x then false
	  else if index >= String.size y then true
	  else if ord (x sub index) <
		  ord (y sub index)
	  then true
	  else if ord (x sub index) =
		  ord (y sub index)
	  then gt' (index + 1) x y
	  else false
  in
    fun gt x y = gt' 0 x y
  end

  local
    fun ge' index x y =
	  if index >= String.size y then true
	  else if index >= String.size x then false
	  else if ord (x sub index) <
		  ord (y sub index)
	  then true
	  else if ord (x sub index) =
		  ord (y sub index)
	  then ge' (index + 1) x y
	  else false
  in
    fun ge x y = ge' 0 x y
  end

end
