signature TIMED_LOG =
sig
  type time
  exception Logtime of time (* found *) * time (* desired *)
  include LOG
  (* All basic operations are the same, but entries are tagged with times;
     if there is a mismatch, Time is raised.
     Additional function: *)
  val next: mark -> (time * entry)
      (* Like read, but returns the time rather than checking it. *)
end

functor TimedLog(type entry) : TIMED_LOG =
struct
  open DebugKernel
  type basicEntry = entry
  structure Log = Log(type entry=time * basicEntry)
  open Log
  exception Logtime of time * time
  type entry = basicEntry
  fun append m x = Log.append m (currentTime(),x)
  fun get m = 
    let val (t,x) = Log.get m
    in if t <> currentTime() then
         raise Logtime(t,currentTime())
       else x
    end
  fun read m = 
    let val (t,x) = Log.read m
    in if t <> currentTime() then
         raise Logtime(t,currentTime())
       else x
    end
  val next = Log.read
  fun replace m x = Log.replace m (currentTime(),x)
  fun write m x = Log.write m (currentTime(),x)
end

