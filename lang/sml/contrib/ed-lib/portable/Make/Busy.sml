(* BUSY.SML

Created by:     Nick Rothwell, LFCS, University of Edinburgh
                nick@lfcs.ed.ac.uk
Date:           30 Oct 1990

		Modified to fit the library structure by Dave Berry,
		24 Jan 1991.

Maintenance:    Author


DESCRIPTION

   This is part of the portable make system.


RCS LOG

$Log: Busy.sml,v $
Revision 1.1  1994/02/08 00:23:21  jkh
Initial revision

Revision 1.3  91/01/25  20:12:45  20:12:45  db (Dave Berry)
Prefixed local signature and functor names with MAKE_ or Make respectively.

Revision 1.2  91/01/25  20:01:51  20:01:51  db (Dave Berry)
Put infix before above the declaration, as SML/NJ was treatingit as infix
anyway.

Revision 1.1  91/01/25  11:40:42  11:40:42  db (Dave Berry)
Initial revision


*)


signature MAKE_BUSY =
   sig
      val dot: unit -> unit
      val star: unit -> unit
      val print: string -> unit
      val println: string -> unit

      val withSpace: ('a -> unit) -> ('a -> unit)
      val withNewline: ('a -> unit) -> ('a -> unit)

      val withDot: ('a -> 'b) -> 'a -> 'b
   end;

functor MakeBusy (CoreUtils: CORE_UTILS): MAKE_BUSY =
   struct
      infix before
      val op before = CoreUtils.before

      val len = ref 0
      val dots = ref false

      fun P x = output(std_out, x)

      fun sym s = (if !len = 75 then (P "\n"; len := 0) else ();
                   CoreUtils.flush_out std_out;
                   P s;
                   len := !len + 1;
                   dots := true
                  )

      fun dot() = sym "."
      fun star() = sym "*"

      fun print s = (if !dots then (P "\n"; len := 0) else ();
                     dots := false;
                     P s;
                     len := !len + size s
                    );

      fun println s = (print s; P "\n"; len := 0)

      fun withSpace pr = fn x => (pr x; print " ")
      fun withNewline pr = fn x => (pr x; println " ")

      fun withDot f a = f a before dot()
   end;
