signature NON_STANDARD =

(* NON-STANDARD FUNCTIONS REQUIRED BY THE LIBRARY.

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           24 Jan 1991

Maintenance:    Author


DESCRIPTION

   These functions are needed to load the library, but are not defined
   in the Definition of Standard ML.  The load file for each compiler
   must define a structure to match this signature.


RCS LOG

$Log: NON_STANDARD.sml,v $
Revision 1.1  1994/02/08 00:23:23  jkh
Initial revision

Revision 1.1  91/01/25  11:29:16  11:29:16  db (Dave Berry)
Initial revision


*)

sig
  val use: string -> unit
   (* use file; load the SML code in named file into the current top-level
      environment. *)

  val flush_out: outstream -> unit
   (* flush_out os; flush any buffered characters on os. *)
end
