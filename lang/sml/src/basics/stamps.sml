(* Copyright 1990, 1991 by AT&T Bell Laboratories *)
(* stamps.sml *)

abstraction Stamps : STAMPS =
struct

  type boundscope = int ref
  datatype scope = FREESCOPE
                 | BOUNDSCOPE of boundscope
  
  datatype stamp =
      BOUND of boundscope * int 
    | GENFREE of PersStamps.persstamp  (* only found in signatures *)
    | FREE of PersStamps.persstamp
    | NULL
    | ERROR

  val nextStamp = ref 1
  val nextBoundScope = ref 1

  val freeScope = FREESCOPE

  fun newBoundScope () =
          BOUNDSCOPE (ref (!nextBoundScope)) before inc nextBoundScope

  fun newStamp FREESCOPE = 
          (fn () => FREE(PersStamps.newStamp()))
    | newStamp (BOUNDSCOPE s) =
          (fn () => BOUND(s,(!nextStamp before inc nextStamp)))

  fun newGenStamp FREESCOPE = 
          (fn () => GENFREE(PersStamps.newStamp()))
    | newGenStamp (BOUNDSCOPE s) =
          (fn () => BOUND(s,(!nextStamp before inc nextStamp)))

  val newFree = newStamp FREESCOPE

  val null = NULL
  val error = ERROR

  fun isBound FREESCOPE = (fn FREE _ => true | _ => false)
    | isBound (BOUNDSCOPE s) = (fn BOUND(s',_) => s=s' | _ => false)

  fun less (BOUND(s1,n1), BOUND(s2,n2)) =
      if s1 <> s2 then ErrorMsg.impossible "Stamps.less -- scopes"
	  else n1 < n2
    | less (FREE s1,FREE s2) = PersStamps.less (s1,s2)
    | less _ =  ErrorMsg.impossible "Stamps.less - bad arg"

  fun greater (BOUND(s1,n1), BOUND(s2,n2)) =
      if s1 <> s2 then ErrorMsg.impossible "Stamps.greater -- scopes"
	  else n1 > n2
    | greater (FREE s1,FREE s2) = PersStamps.greater(s1,s2)
    | greater _ = ErrorMsg.impossible "Stamps.greater - bad arg"

  val stampToString =
      fn BOUND(ref i,j) => "BOUND(scope="^makestring i^
                             ",stamp="^makestring j^")"
       | FREE i => "FREE("^PersStamps.stampToString i^")"
       | GENFREE i => "GENFREE("^PersStamps.stampToString i^")"
       | NULL => "NULL"
       | ERROR => "ERROR"

  abstype 'a stampMap = STAMPMAP of ('a Intmap.intmap * 'a PersStamps.stampMap)
  with
     fun newMap ex = STAMPMAP(Intmap.new (20, ex),PersStamps.newMap ex)

     fun updateMap (STAMPMAP (map,_)) (BOUND(_,st),v) = Intmap.add map (st, v)
       | updateMap (STAMPMAP (_,map)) (FREE st,v) = PersStamps.updateMap map (st, v)
       | updateMap _ _ = ErrorMsg.impossible "Stamps.updateMap"

     fun applyMap (STAMPMAP (map,_), BOUND (_,s)) = Intmap.map map s
       | applyMap (STAMPMAP (_,map), FREE s) = PersStamps.applyMap (map,s)
       | applyMap _ = ErrorMsg.impossible "Stamps.applyMap"
  end
    
end (* structure Stamps *)
