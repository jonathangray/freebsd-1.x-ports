(* Copyright 1989 by AT&T Bell Laboratories *)
signature Startup =
 sig 
     val core : System.Unsafe.object
     val initial : System.Unsafe.object
     val math : System.Unsafe.object
     val name : string
 end

functor Loader ( S : Startup ) : sig end =
  struct

    open System
    type object = System.Unsafe.object

    val applyCode : Code.code -> unit -> ((object list -> object) * string list)
	  = Code.apply

    val dict : (string*object) list ref = 
	  ref [("Initial",S.initial),("Core",S.core),("Math",S.math)]

    val _ = (System.Unsafe.pstruct := {core=S.core,math=S.math,initial=S.initial})

    exception Notfound_Loader

    fun lookup s = let
	  fun f ((s1,stru)::r) = if s=s1 then stru else f r
	    | f [] = raise Notfound_Loader
	  in
	    f (!dict)
	  end

    fun enter pair = (dict := pair::(!dict))

    fun readfile s = let
          val stream = open_in s
	  val code = Code.inputCode(stream, (can_input stream))
	  in
	    close_in stream;
	    applyCode code
	end

    fun getmo s = let
	  open System.Unsafe
	  fun f DATANIL = readfile s
	    | f (DATACONS(s',t,x)) = if s=s' then (cast t) else f x
          in
	    f datalist
	  end

    val say = System.Print.say

    fun getstruct s = (lookup s)
	  handle Notfound_Loader =>
	    let val _ = (say "[Loading "; say s; say "]\n")
		val g = getmo ("mo/" ^ s ^ ".mo");
	        val (exec,sl) = g ()
	        val structs = map getstruct sl
	        val _ = (say "[Executing "; say s; say "]\n")
	        val str = exec structs
	    in  enter (s,str);
		str
	    end

    val _ = let open System.Unsafe.CleanUp in
	    (getstruct S.name; cleanup CleanForQuit)
	    (* this is the global exception handler of the sml system *)
	      handle Io s => (
		     say "uncaught Io exception (Loader): ";
		     say s;
		     say "\n";
		     cleanup CleanForQuit)
		   | exn => (
		     say "uncaught exception (Loader): ";
		     say (exn_name exn);
		     say "\n";
		     cleanup CleanForQuit)
	  end

  end (* functor Loader *)
