(*$StreamPair : STREAM_PAIR Instream Outstream General IntParse *)

loadSig "STREAM_PAIR";

structure StreamPair: STREAM_PAIR =

(* STREAM PAIRS FOR POLY/ML

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Nov 1989

Maintenance:	Author


DESCRIPTION

   PolyML provides the execute function.


SEE ALSO

   Instream, Outstream


RCS LOG

$Log: StreamPair.sml,v $
Revision 1.1  1994/02/08 00:23:15  jkh
Initial revision

Revision 1.1  91/09/13  16:24:06  16:24:06  db (Dave Berry)
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

    fun execute s =
	OK (Pair (ExtendedIO.execute s))
        handle Io s => Fail s


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
