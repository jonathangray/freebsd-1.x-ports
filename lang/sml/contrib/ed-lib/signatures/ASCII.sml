(*$ASCII *)

signature ASCII =
sig

(* ASCII CONTROL CODES

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        4 Feb 1991

Maintenance:	Author


DESCRIPTION

   Bindings of the ASCII control characters to their standard names.


NOTES

   These definitions used to be in the STRING signature.


RCS LOG

$Log: ASCII.sml,v $
Revision 1.1  1994/02/08 00:23:23  jkh
Initial revision

Revision 1.1  91/02/11  18:12:23  18:12:23  db (Dave Berry)
Initial revision


*)


(* CONSTANTS *)


  val nul: string
   (* nul = "\000" *)
  val soh: string
   (* soh = "\001" *)
  val stx: string
   (* stx = "\002" *)
  val etx: string
   (* etx = "\003" *)
  val eot: string
   (* eot = "\004" *)
  val enq: string
   (* enq = "\005" *)
  val ack: string
   (* ack = "\006" *)
  val bel: string
   (* bel = "\007" *)
  val bs: string
   (* bs = "\008" *)
  val ht: string
   (* ht = "\009" *)
  val nl: string
   (* nl = "\010" *)
  val vt: string
   (* vt = "\011" *)
  val np: string
   (* np = "\012" *)
  val cr: string
   (* cr = "\013" *)
  val so: string
   (* so = "\014" *)
  val si: string
   (* si = "\015" *)
  val dle: string
   (* dle = "\016" *)
  val dc1: string
   (* dc1 = "\017" *)
  val dc2: string
   (* dc2 = "\018" *)
  val dc3: string
   (* dc3 = "\019" *)
  val dc4: string
   (* dc4 = "\020" *)
  val nak: string
   (* nak = "\021" *)
  val syn: string
   (* syn = "\022" *)
  val etb: string
   (* etb = "\023" *)
  val can: string
   (* can = "\024" *)
  val em: string
   (* em = "\025" *)
  val sub: string
   (* sub = "\026" *)
  val esc: string
   (* esc = "\027" *)
  val fs: string
   (* fs = "\028" *)
  val gs: string
   (* gs = "\029" *)
  val rs: string
   (* rs = "\030" *)
  val us: string
   (* us = "\031" *)
  val del: string
   (* del = "\127" *)

end
