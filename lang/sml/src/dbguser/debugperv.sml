structure DEBUG_PERV_X = struct
   structure IO = HistoricalIO
   structure Integer = struct open Integer
			      val print = IO.outputc IO.std_out o makestring
		       end
   structure Real = struct open Real
			   val print = IO.outputc IO.std_out o makestring
		       end
   structure Bool = struct open Bool
			   val print = IO.outputc IO.std_out o makestring
		       end
   structure String = struct open String
			     val print = IO.outputc IO.std_out
		       end
   structure Array = HistoricalArray
   structure Ref = HistoricalRef
   structure List = DebugList
   structure General = DebugGeneral
   open Ref String IO Bool List Integer Real General
   structure System = struct open System
			     structure Signals = HistoricalSignals
		      end
end

structure DEBUG_PERV = struct
    open DEBUG_PERV_X
    overload makestring : ('a -> string)
	  as Bool.makestring and Integer.makestring and Real.makestring
    overload print : ('a -> unit)
	  as Bool.print and Integer.print and Real.print and String.print
    overload ~ :   ('a -> 'a)        as Integer.~   and Real.~
    overload + :   ('a * 'a -> 'a)   as Integer.+   and Real.+
    overload - :   ('a * 'a -> 'a)   as Integer.-   and Real.-
    overload * :   ('a * 'a -> 'a)   as Integer.*   and Real.*
    overload < :   ('a * 'a -> bool) as Integer.<   and Real.<  and String.<
    overload > :   ('a * 'a -> bool) as Integer.>   and Real.>  and String.>
    overload <= :  ('a * 'a -> bool) as Integer.<=  and Real.<= and String.<=
    overload >= :  ('a * 'a -> bool) as Integer.>=  and Real.>= and String.>=
    overload abs : ('a -> 'a)        as Integer.abs and Real.abs
end

