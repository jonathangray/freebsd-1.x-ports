(*$Instream: INSTREAM General *)

loadSig "INSTREAM";

structure Instream: INSTREAM =

(* INPUT STREAMS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Nov 1989

Maintenance:	Author


SEE ALSO

   Outstream, StreamPair


RCS LOG

$Log: Instream.sml,v $
Revision 1.1  1994/02/08 00:23:29  jkh
Initial revision

Revision 1.6  91/02/12  15:02:39  15:02:39  db (Dave Berry)
Changed datatype to abstype.

Revision 1.5  91/02/05  11:07:49  11:07:49  db (Dave Berry)
Changed input, lookahead and eof so that and end of stream indication
is treated like a character on the stream.  So lookahead recognises it
but doesn't consume it, input recognises it and consumes it, and eof
is defined to be (lookahead i = "") as in the Definition of SML.
This behaviour fits that described in the Definition, and still allows
parsing of polymorphic vectors terminated by end_of_stream.

Revision 1.4  91/02/04  16:59:25  16:59:25  db (Dave Berry)
Renamed InStream to instream, and similarly for structure and signature ids.
Renamed input' to read.
Added Io exception.

Revision 1.3  91/01/25  20:17:16  20:17:16  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:21:15  17:21:15  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:53:37  14:53:37  db (Dave Berry)
Initial revision


*)

struct


(* ABSTYPE *)

  local
    type old_instream = instream
  in
    abstype instream = I of old_instream * (bool ref) * (bool ref)
     (* The second field is true iff the last call to lookahead returned "".
	The last field is true iff the stream is closed. *)
    with


(* SYSTEM *)

      exception NotImplemented of string

      fun openString s = raise NotImplemented "openString"
      fun canInput i n = raise NotImplemented "canInput"
      fun reset i = raise NotImplemented "reset"
      fun interactive i = raise NotImplemented "interactive"
      fun read i n = raise NotImplemented "read"
  

(* MANIPULATORS *)

      exception Io = Io
  
      val stdIn =
  	  I (std_in, ref false, ref false)
      val std_in = stdIn
  
      fun openIn s =
  	  I (open_in s, ref false, ref false)
      val open_in = openIn
  
      fun closeIn (i as I (i', _, closed)) =
  	  (close_in i'; closed := true)
      val close_in = closeIn
  
      val old_input = input
      local
        fun inp (I (i', _, ref true)) n = ""
        |   inp (I (i', eof as ref true, _)) n = (eof := false; "")
        |   inp (I (i', eof, _)) n = old_input (i', n)
      in
        fun input (i, n) =
              if n < 0 then raise General.Nat ("input", n)
              else inp i n
      end
  
      fun input1 i = input (i, 1)
    
      val old_lookahead = lookahead
      fun lookahead (I (_, _, ref true)) = ""
      |   lookahead (I (_, ref true, _)) = ""
      |   lookahead (I (i', eof, _)) =
  	  let val s = old_lookahead i'
  	  in if s = "" then eof := true else eof := false;
  	     s
  	  end
  
      fun end_of_stream i = (lookahead i = "")
      val eof = end_of_stream
    
      local
        fun readString' _ [] = OK ()
        |   readString' i (h::t) =
    	  case input1 i
    	  of "" => Fail ""
    	  |  c  =>
    	      if h = c then 
    		case readString' i t of
    		  OK () => OK ()
    		| Fail s => Fail (c ^ s)
    	      else
    		Fail c
      in
        fun readString i s = readString' i (explode s)
      end
    
      fun skip p i =
    	case lookahead i
    	of "" => ()
    	|  s  => if p s then (input1 i; skip p i) else ()
    
      local
        fun line i s =
            case input (i, 1) of
               ""  => s
            | "\n" => s ^ "\n"
            |   c  => line i (s ^ c)
      in
        fun inputLine i = line i ""
      end
  
    end (* abstype *)

  end (* local *)

end
