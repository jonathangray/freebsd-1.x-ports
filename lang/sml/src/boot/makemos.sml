(* Copyright 1989 by AT&T Bell Laboratories *)

structure MakeMos = 
  struct

    local
      structure C = System.Code
    in
    fun makeMos {modName, mosDir} = let
	  type object = System.Unsafe.object
	  val applyCode : C.code -> unit -> ((object list -> object) * string list)
		= C.apply

	  val dict = ref ["Core"]
	  fun lookup s = let
		fun f (s1::r) = s=s1 orelse f r
		  | f nil = false
		in
		  f (!dict)
		end
	  fun enter s = dict := s::(!dict)

	  fun readfile s = let
		val stream = open_in s
		val file = C.inputCode (stream, (can_input stream))
		in
		  close_in stream;
		  applyCode file
		end

	  val f = open_out (implode[mosDir, "/", modName, ".mos"])
	  val say = outputc f
	  fun getstruct s = if (lookup s)
		then ()
		else let
		  val s' = "mo/" ^ s ^ ".mo"
		  val _ = (say s'; say "\n")
		  val g = readfile s'
		  val (_, sl) = g ()
		  in
		    app getstruct sl;
		    enter s
		  end
	  in
	    output(std_out, modName ^ ".mos\n");
	    say "mo/CoreFunc.mo\n";
	    app getstruct ["Initial", "Loader", modName];
	    close_out f
	  end (* makeMos *)
    end (* local *)

    val targets = [
	    "Vax", "M68", "Sparc", "MipsLittle", "MipsBig", 
	    "RS6000", "I386"
	  ]

    fun makeMosForTarget {mosDir} target = (
	  makeMos {modName = "Int"^target, mosDir = mosDir};
	  makeMos {modName = "Int"^target^"D", mosDir = mosDir};
	  makeMos {modName = "Comp"^target, mosDir = mosDir})

    fun makePervMos {mosDir} = let
	  val f = open_out (mosDir ^ "/Perv.mos")
	  in
	    output (std_out, "Perv.mos");
	    output (f, "mo/CoreFunc.mo\n");
	    output (f, "mo/Math.mo\n");
	    output (f, "mo/Initial.mo\n");
	    close_out f
	  end

    fun makeAllMos {mosDir} = (
	  makePervMos {mosDir = mosDir};
	  makeMos {modName = "IntNull", mosDir = mosDir};
	  makeMos {modName = "IntNullD", mosDir = mosDir};
	  app (makeMosForTarget {mosDir = mosDir}) targets)

  end; (* MakeMos *)
