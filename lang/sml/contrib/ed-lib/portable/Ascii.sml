(*$Ascii *)

loadSig "ASCII";

structure Ascii: ASCII =

(* ASCII CONTROL CHARACTERS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        4 Feb 1991

Maintenance:	Author


DESCRIPTION

   Bindings of strings that contain a single ASCII control character to
   the standard names of those characters.


RCS LOG

$Log: Ascii.sml,v $
Revision 1.1  1994/02/08 00:23:17  jkh
Initial revision

Revision 1.1  91/02/11  19:49:22  19:49:22  db (Dave Berry)
Initial revision


*)

struct


(* CONSTANTS *)

  val nul = "\000"
  val soh = "\001"
  val stx = "\002"
  val etx = "\003"
  val eot = "\004"
  val enq = "\005"
  val ack = "\006"
  val bel = "\007"
  val bs  = "\008"
  val ht  = "\009"
  val nl  = "\010"
  val vt  = "\011"
  val np  = "\012"
  val cr  = "\013"
  val so  = "\014"
  val si  = "\015"
  val dle = "\016"
  val dc1 = "\017"
  val dc2 = "\018"
  val dc3 = "\019"
  val dc4 = "\020"
  val nak = "\021"
  val syn = "\022"
  val etb = "\023"
  val can = "\024"
  val em  = "\025"
  val sub = "\026"
  val esc = "\027"
  val fs  = "\028"
  val gs  = "\029"
  val rs  = "\030"
  val us  = "\031"
  val del = "\127"

end
