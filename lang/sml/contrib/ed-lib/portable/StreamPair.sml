(*$StreamPair : STREAM_PAIR Instream Outstream General IntParse *)

loadSig "STREAM_PAIR";

structure StreamPair: STREAM_PAIR =

(* PAIRS OF ONE INPUT STREAM AND ONE OUTPUT STREAM

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Nov 1989

Maintenance:	Author


SEE ALSO

   Instream, Outstream


RCS LOG

$Log: StreamPair.sml,v $
Revision 1.1  1994/02/08 00:23:20  jkh
Initial revision

Revision 1.6  91/02/12  14:42:48  14:42:48  db (Dave Berry)
Changed datatype to abstype.

Revision 1.5  91/02/11  20:44:26  20:44:26  db (Dave Berry)
The function for reading integers is now in the IntParse structure instead
of the Int structure, as part of the major reorganisation of the library.

Revision 1.4  91/02/04  17:02:30  17:02:30  db (Dave Berry)
Renamed OutStream and InStream to outstream and instream, etc., as part
of the reorganisation of the stream entries.
Renamed input' to read and output' to write.

Revision 1.3  91/01/25  20:21:34  20:21:34  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:25:39  17:25:39  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  15:28:27  15:28:27  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  abstype StreamPair = Pair of instream * outstream
  with


(* CONSTANTS *)

    val std = Pair (Instream.stdIn, Outstream.stdOut)


(* CREATORS *)

    val create = Pair


(* CONVERTORS *)

    fun streams (Pair x) = x


(* SYSTEM *)

    fun openTemporary () = raise General.NotImplemented "openTemporary"
    fun execute s = raise General.NotImplemented "execute"


(* MANIPULATORS *)

    fun openPair s =
	  OK (Pair (Instream.openIn s, Outstream.openOut s))
	  handle Instream.Io s => Fail s
	  |      Outstream.Io s => Fail s

    fun closePair (Pair (i, os)) = (Instream.closeIn i; Outstream.closeOut os)

    fun output (io as Pair (_, os), s) =
	  Outstream.output (os, s)
     
    val write = General.curry output

    fun input (Pair (i, _), n) = Instream.input (i, n)

    fun input1 io = input (io, 1)

    fun read (Pair (i, _)) n = Instream.read i n

    fun lookahead (Pair (i, _)) = Instream.lookahead i

    fun eof (Pair (i, _)) = Instream.eof i

    fun canInput (Pair (i, _)) n =
	  Instream.canInput i n

    fun flush (Pair (_, os)) =
	  Outstream.flush os

    fun reset (Pair (i, _)) =
	  Instream.reset i

    fun interactive (Pair (i, os)) =
	  Instream.interactive i andalso Outstream.interactive os

    fun readString (Pair (i, _)) s =
	  Instream.readString i s

    fun skip p (Pair (i, _)) =
	  Instream.skip p i

    fun inputLine (Pair (i, _)) =
	  Instream.inputLine i

    fun prompt io s =
	  (write io (s ^ " ");
	   inputLine io)

    fun ask (io as Pair (i, os)) s p =
	  (Outstream.write os (s ^ "\n");
	   case p i of
	     Fail _ =>
	       ( Instream.inputLine i;
	         write io ("Invalid input\n");
	         ask io s p
	       )
	   | OK x => x)

    fun confirm io s =
	  (write io (s ^ "\n");
	   case input1 io of
	     "y" => (inputLine io; true)
	   | "n" => (inputLine io; false)
	   |  _  => (inputLine io; confirm io s))

    fun menu io t l =
          let
            fun readInt i =
              case IntParse.read i of
	        OK n =>
		  if n > 0 andalso n <= List.size l then OK n
		  else Fail None
	      | Fail x => Fail x

	    fun outputEntry (x, n) =
	      let val num = String.padL " " 2 (CoreUtils.intToString (n+1))
	      in write io (num ^ ". " ^ x ^ "\n")
	      end
	  in
	    write io (t ^ "\n\n");
	    List.iterate outputEntry l;
	    ask io "Select Entry: " readInt
	  end

  end (* abstype *)

end
