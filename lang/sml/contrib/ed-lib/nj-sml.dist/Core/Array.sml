loadLocalSig "ARRAY";

structure CoreArray: CORE_ARRAY = Array

(* CORE ARRAY FUNCTIONS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           13 Mar 1991

Maintenance:    Author


DESCRIPTION

   SML/NJ provides arrays as a built-in type.  In version 0.74
   this matches the CORE_ARRAY signature directly.  In earlier
   versions the code commented out below should be used instead.


RCS LOG

$Log: Array.sml,v $
Revision 1.1  1994/02/08 00:23:14  jkh
Initial revision

Revision 1.2  1991/10/29  19:28:17  db
Updated to SML/NJ 0.74.

Revision 1.1  91/03/27  17:31:03  17:31:03  db (Dave Berry)
Initial revision



*)
(*
struct

  infix sub

  type 'a array = 'a Array.array

  exception Size = CoreVector.Size

  exception Subscript = Array.Subscript

  val array = Array.array

  val arrayoflist = Array.arrayoflist

  fun tabulate (i, f) =
        let fun tab j = if j < i then f j :: tab (j+1) else nil
        in if i < 0 then raise Size else arrayoflist (tab 0)
        end

  val op sub = Array.sub

  val update = Array.update

  val length = Array.length
end

*)
