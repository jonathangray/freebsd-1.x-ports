(* Copyright 1989 by AT&T Bell Laboratories *)
(* dynenv.sml *)

structure DynamicEnv : DYNENV =
struct

  type object = System.Unsafe.object
  datatype dynenv = NORM of System.Unsafe.object IntmapF.intmap * dynenv
                  | SPECIAL of (Access.lvar -> System.Unsafe.object) * dynenv
		  | EMPTY
  (* chain invariant: only one NORM in a row. *)

  exception Unbound = IntmapF.IntmapF
  exception SpecialEnv

  val empty = EMPTY

  fun special (f,next) = SPECIAL(f,next)

  fun look (NORM(map,next)) lv = ((IntmapF.lookup map lv)
				  handle Unbound => look next lv)
    | look (SPECIAL(f,next)) lv = ((f lv) handle Unbound => look next lv)
    | look EMPTY lv = raise Unbound

  fun bind (lv,binding,NORM(map,next)) = NORM(IntmapF.add(map,lv,binding),next)
    | bind (lv,binding,x) = NORM(IntmapF.add(IntmapF.empty,lv,binding),x)

  fun atop(NORM(topmap,EMPTY),NORM(bottommap,next)) = 
      NORM(IntmapF.overlay(topmap,bottommap),next)
    | atop(NORM(topmap,EMPTY),bottom) = NORM(topmap,bottom)
    | atop(NORM(topmap,nexttop),bottom) = NORM(topmap,atop(nexttop,bottom))
    | atop(SPECIAL(f,nexttop),bottom) = SPECIAL(f,atop(nexttop,bottom))
    | atop(EMPTY,bottom) = bottom
       
  fun remove(lvars: Access.lvar list, NORM(map,next)) =
      NORM(fold IntmapF.delete lvars map,remove(lvars,next))
    | remove(lvars,SPECIAL(f,next)) = raise SpecialEnv
    | remove(lvars,EMPTY) = EMPTY
      
  fun consolidate e = e

end (* structure DynamicEnv *)
