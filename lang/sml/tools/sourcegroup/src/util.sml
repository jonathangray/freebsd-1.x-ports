(* Copyright (c) 1992 by Carnegie Mellon University *)

functor UtilFun
  (structure Data :DATA
   structure Pathname :PATHNAME
   structure Execute :EXECUTE
   structure StringXtra :STRINGXTRA
  ) :UTIL = struct

structure Data = Data

val issueWarnings = ref true

val say = System.Print.say

fun stringEqual (x:string, y:string) = (x=y)

fun printSep (sep :string) ([] :string list) :unit = ()
  | printSep sep (a::[]) = print a
  | printSep sep (a::(rest as (b::c))) =
      (say a; say sep; printSep sep rest)

fun warn (lst :string list) :unit =
  if !issueWarnings then (say "% "; app say lst; say "\n") else ()
fun err (lst :string list) :'a =
  (say "? "; app say lst; say "\n"; raise Data.CompilingError)

val zeroTime :System.Timer.time = System.Timer.TIME{sec=0,usec=0}
fun isZeroTime (System.Timer.TIME {sec,usec}) = (sec=0) andalso (usec=0)

val currentTime :unit -> System.Timer.time =
      System.Unsafe.CInterface.c_function "timeofday"

fun seconds (System.Timer.TIME{sec,...}) = sec
fun microSeconds (System.Timer.TIME{usec,...}) = usec
fun newer (x,y) = System.Timer.earlier (y, x)
fun newAs (x,y) = not (System.Timer.earlier (x, y))

fun trim (path:string) :string =
  let val parts = Pathname.explodePath path
      val len = length parts
      val strip' = len - (!System.Print.pathnames) - 1
      val strip = if strip'<=1 then 0 else if strip'>len then len else strip'
      val showParts' = nthtail (parts, strip)
      val showParts = if strip>0 then ("..."::showParts') else showParts'
  in
    Pathname.implodePath showParts
  end

fun timeStamp () =
  let val System.Timer.TIME {sec,usec} = currentTime ()
  in (makestring sec)^"#"^(makestring usec) end

fun remDuplicates (x:string list) (y:string list) =
  let fun rmdup (([]:string list), (x:string list)) = ([], x)
        | rmdup (x, []) = (x, [])
        | rmdup (d as (dh::dt), r as (rh::rt)) =
            if dh = rh
                then let val (d',r')=rmdup(dt,rt) in (dh::d',r') end
              else if dh < rh
                then let val (d',r')=rmdup(dt,r) in (dh::d',r') end
                else let val (d',r')=rmdup(d,rt) in (d',rh::r') end
  in rmdup (x,y) end

val toolIsDefined :(string * int -> bool) ref = ref (fn (s,i) => false)
end
