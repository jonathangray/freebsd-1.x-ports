(*$GENERAL_TYPES: General *)

signature GENERAL_TYPES =

(* GENERAL TYPES AND EXCEPTIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        21 Feb 1989

Maintenance:	Author


DESCRIPTION

   The types, exceptions and infix operators defined in General; these
   will be made available globally.


RCS LOG

$Log: GENERAL_TYPES.sml,v $
Revision 1.1  1994/02/08 00:23:26  jkh
Initial revision

Revision 1.4  91/02/12  18:21:28  18:21:28  db (Dave Berry)
Added the infix functions declared in GENERAL.sml, so that they can
be declared infix at top level by GeneralTypes, and thus used as
infix functions throughout the library.

Revision 1.3  91/01/25  19:16:00  19:16:00  db (Dave Berry)
Added dependence on General.

Revision 1.2  91/01/25  16:55:03  16:55:03  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.1  90/12/17  16:48:04  16:48:04  db (Dave Berry)
Initial revision


*)
sig
  datatype 'a Option = None | Some of 'a
    sharing type Option = General.Option

  type Nat
    sharing type Nat = int

  datatype ('a, 'b) Result = OK of 'a | Fail of 'b
    sharing type Result = General.Result

  (* The next two exceptions are used by the redefinition of the
     pervasive arithmetic operations. *)
  exception Overflow
  and OldDiv

  val oo: ('c -> 'd) * ('a -> 'b -> 'c) -> 'a -> 'b -> 'd

  val before: 'a * 'b -> 'a
end
