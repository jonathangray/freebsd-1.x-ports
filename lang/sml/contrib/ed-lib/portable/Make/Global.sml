loadLocalSig "GLOBAL";

functor MakeGlobal(CoreUtils: CORE_UTILS): MAKE_GLOBAL =

(* GLOBAL DEFINITIONS

Created by:     Nick Rothwell, LFCS, University of Edinburgh
                nick@lfcs.ed.ac.uk
Date:           30 Oct 1990

                Modified to fit the library structure by Dave Berry,
                24 Jan 1991.

Maintenance:    Author


DESCRIPTION

   This is part of the portable make system.


RCS LOG

$Log: Global.sml,v $
Revision 1.1  1994/02/08 00:23:22  jkh
Initial revision

Revision 1.3  91/01/25  20:13:24  20:13:24  db (Dave Berry)
Prefixed local signature and functor names with MAKE_ or Make respectively.

Revision 1.2  91/01/25  15:49:00  db
Moved option, substring and fold here from CoreUtils.
Deleted disj_sum.

Revision 1.1  91/01/25  11:41:19  11:41:19  db (Dave Berry)
Initial revision


*)

struct
  structure CoreUtils = CoreUtils

  type 'a pair = ('a * 'a)
   and 'a triple = ('a * 'a * 'a)
   and 'a predicate = ('a -> bool)
   and 'a procedure = ('a -> unit)
   and ('a, 'b) currying = 'a -> ('a -> 'b)
   and 'a mapping = ('a -> 'a)
   and 'a generator = (unit -> 'a)
  type 'a relation = ('a pair) predicate

  fun never(_) = false

  fun fst(x, _) = x
  fun snd(_, x) = x

  datatype 'a option = NONE | SOME of 'a

  local infix f
  in
     fun fold (op f) (h :: t) e = fold (op f) t (h f e)
     |   fold _ [] e = e
  end;


  (* substring(s, i, len) returns a string of length "len" with offset "i" *)
  exception SubString;
  fun substring (s, i, len) =
       let
       (* Remove the first "i" items from the list, then put "len" items onto
          the result. *)
         fun getl (_, 0, 0) = []
         |   getl (a :: b, 0, len) = a :: getl(b, 0, len-1)
         |   getl (a :: b, i, len) = getl(b, i-1, len)
         |   getl _ = raise SubString
       in implode(getl(explode s, i, len))
       end;

  local
    fun line i s =
        case input (i, 1) of
           ""  => s
        | "\n" => s ^ "\n"
        |   c  => line i (s ^ c)
  in
    fun inputLine i = line i ""
  end
end;
