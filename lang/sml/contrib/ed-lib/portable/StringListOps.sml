(*$StringListOps : STRING_LIST_OPS String List *)

loadSig "STRING_LIST_OPS";

structure StringListOps: STRING_LIST_OPS =

(* LIST-LIKE OPERATION ON ASCII STRINGS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        10 Feb 1991

Maintenance:	Author


NOTES

   These functions were originally in the main String structure.


RCS LOG

$Log: StringListOps.sml,v $
Revision 1.1  1994/02/08 00:23:21  jkh
Initial revision

Revision 1.2  91/02/22  19:10:09  19:10:09  db (Dave Berry)
Renamed Sub to Subscript, to match existing convention.

Revision 1.1  91/02/11  20:51:41  20:51:41  db (Dave Berry)
Initial revision



*)

struct


(* LOCAL *)

  fun pairApply f (x, y) = (f x, f y)


(* MANIPULATING THE NTH ELEMENT *)

  exception Subscript of string * int
  infix 9 sub
  fun s sub n = List.sub (explode s, n)
		handle List.Subscript x => raise Subscript x

  fun nth n s = s sub n
		handle Subscript _ => raise Subscript ("nth", n)

  fun dropNth n s =
	implode (List.dropNth n (explode s))
	handle List.Subscript p => raise Subscript p

  fun removeNth n s =
	let val (x, xs) = List.removeNth n (explode s)
	in (x, implode xs)
	end
	handle List.Subscript p => raise Subscript p

  fun splitNth n s =
	pairApply implode (List.splitNth n (explode s))
	handle List.Subscript p => raise Subscript p

  exception Char of string * string

  fun updateNth n s' s =
	if size s' <> 1 then raise Char ("updateNth", s')
	else implode (List.updateNth n s' (explode s))
	handle List.Subscript p => raise Subscript p

  fun changeNth n f s =
	implode (List.changeNth n f (explode s))
	handle List.Subscript p => raise Subscript p

  fun insertNth n s' s =
	implode (List.insertNth n (explode s') (explode s))
	handle List.Subscript p => raise Subscript p

  fun appendNth n s' s =
	implode (List.appendNth n (explode s') (explode s))
	handle List.Subscript p => raise Subscript p

  fun spliceNth n s' s =
	implode (List.spliceNth n (explode s') (explode s))
	handle List.Subscript p => raise Subscript p


(* SEARCHING AND INDEXING *)

  local
    fun search' _ nil _ = Fail ()
    |   search' s (s' as _::t') n =
      if List.prefixes s s' then OK n
      else search' s t' (n+1)	(* Boyer and Moore?  Never heard of them! *)
  in
    fun search s' s n =
	  if n < 0 orelse n >= size s then raise Subscript ("search", n)
	  else if s' = "" then OK n
	  else search' (explode s') (explode (String.extract n (size s) s)) n
    fun revSearch s' s n =
	  if n < 0 orelse n > size s then raise Subscript ("revSearch", n)
	  else if s' = "" then OK (size s - 1)
	  else case search' (List.rev (explode s'))
	  		    (List.rev (explode (String.extract 0 n s)))
			    0
	       of (OK i) => OK (n - i - size s')
	       |  Fail () => Fail ()
  end

  fun occurs s' s n =
	case search s' s n 
	of OK _ => true
	|  Fail () => false
	handle Subscript _ => raise Subscript ("occurs", n)

  fun revOccurs s' s n =
	case revSearch s' s n 
	of OK _ => true
	|  Fail () => false
	handle Subscript _ => raise Subscript ("revOccurs", n)

  fun index p s n =
	( case List.index p (explode (String.extract n (size s) s)) of
	    OK i => OK (i + n)
	  | x => x
	)
	handle String.Extract _ => raise Subscript ("index", n)

  fun revIndex p s n =
	( case List.index p (rev (explode (String.extract 0 n s))) of
	    OK i => OK (n - i - 1)
	  | x => x
	)
	handle String.Extract _ => raise Subscript ("revIndex", n)


(* MANIPULATING THE FIRST ELEMENT THAT SATISFIES A PREDICATE *)

  exception First of string

  fun first p s =
	List.first p (explode s)
	handle List.First p => raise First p

  fun dropFirst p s =
	implode (List.dropFirst p (explode s))
	handle List.First p => raise First p

  fun splitFirst p s =
	pairApply implode (List.splitFirst p (explode s))
	handle List.First p => raise First p

  fun removeFirst p s =
	let val (x, xs) = List.removeFirst p (explode s)
	in (x, implode xs)
	end
	handle List.First p => raise First p

  fun updateFirst p s' s =
	if size s' <> 1 then raise Char ("updateFirst", s')
	else implode (List.updateFirst p s' (explode s))
	handle List.First p => raise First p

  fun changeFirst p f s =
	implode (List.changeFirst p f (explode s))
	handle List.First p => raise First p

  fun insertFirst p s' s =
	implode (List.insertFirst p (explode s') (explode s))
	handle List.First p => raise First p

  fun appendFirst p s' s =
	implode (List.appendFirst p (explode s') (explode s))
	handle List.First p => raise First p

  fun spliceFirst p s' s =
	implode (List.spliceFirst p (explode s') (explode s))
	handle List.First p => raise First p


(* TAKING A PREFIX OF ELEMENTS THAT SATISFY A PREDICATE *)

  fun prefix p s = implode (List.prefix p (explode s))

  fun dropPrefix p s = implode (List.dropPrefix p (explode s))

  fun removePrefix p s = pairApply implode (List.removePrefix p (explode s))


(* MANIPULATING ALL ELEMENTS THAT SATISFY A PREDICATE *)

  fun all p s = implode (List.all p (explode s))

  fun dropAll p s = implode (List.dropAll p (explode s))

  fun removeAll p s = pairApply implode (List.removeAll p (explode s))

  fun updateAll p s' s =
	if size s' <> 1 then raise Char ("updateAll", s')
	else implode (List.updateAll p s' (explode s))

  fun changeAll p f s = implode (List.changeAll p f (explode s))

  fun insertAll p s' s = implode (List.insertAll p (explode s') (explode s))

  fun appendAll p s' s = implode (List.appendAll p (explode s') (explode s))

  fun spliceAll p s' s = implode (List.spliceAll p (explode s') (explode s))

end
