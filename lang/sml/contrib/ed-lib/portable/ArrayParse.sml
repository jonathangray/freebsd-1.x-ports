(*$ArrayParse: ARRAY_PARSE ListParse Instream Outstream *)

loadSig "ARRAY_PARSE";

structure ArrayParse: ARRAY_PARSE =

(* ARRAYS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		8 Feb 1991

Maintenance:	Author


SEE ALSO

   Array, VectorParse.


NOTES

   These functions were originally part of the main Array structure.


RCS LOG

$Log: ArrayParse.sml,v $
Revision 1.1  1994/02/08 00:23:17  jkh
Initial revision

Revision 1.2  91/02/22  19:12:13  19:12:13  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.

Revision 1.1  91/02/11  19:47:56  19:47:56  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type 'a T = 'a Array.Array


(* CONVERTERS *)

  exception Size of string * int

  exception Sep of string * string * string * string

  (* The parse and read functions assume that entries
     are separated by formatting characters. *)

  fun parseSepN start finish sep p n s =
	( case ListParse.parseSepN start finish sep p n s of
	    OK (l, s') => OK (Array.fromList l, s')
	  | Fail (Some l, s') => Fail (Some (Array.fromList l), s')
	  | Fail (None, s') => Fail (None, s')
	)
	handle ListParse.Sep x => raise Sep x

  fun parseSep start finish sep p s =
        ( case ListParse.parseSep start finish sep p s of
            OK (l, s') => OK (Array.fromList l, s')
          | Fail (Some l, s') => Fail (Some (Array.fromList l), s')
          | Fail (None, s') => Fail (None, s')
        )
        handle ListParse.Sep x => raise Sep x

  fun parseN p n s =
        if n < 0 then raise Size ("parseN", n)
        else parseSepN "" "" "" p n s

  fun parse p s = parseSep "" "" "" p s

  fun readSepN start finish sep p n i =
        ( case ListParse.readSepN start finish sep p n i of
            OK l  => OK (Array.fromList l)
          | Fail (Some l) => Fail (Some (Array.fromList l))
          | Fail None => Fail None
        )
        handle ListParse.Sep x => raise Sep x

  fun readSep start finish sep p i =
        case ListParse.readSep start finish sep p i of
          OK l  => OK (Array.fromList l)
        | Fail (Some l) => Fail (Some (Array.fromList l))
        | Fail None => Fail None
        handle ListParse.Sep x => raise Sep x

  fun readN p n i =
        if n < 0 then raise Size ("readN", n)
        else readSepN "" "" "" p n i

  fun read p i = readSep "" "" "" p i

  fun fromFile p name =
        let fun readList i =
                  case p i
                  of Fail _ => (Instream.closeIn i; [])
                  |  OK x => x :: readList i
        in Array.fromList (readList (Instream.openIn name))
        end

  fun file p a name =
        let val os = Outstream.openOut name
	    fun out s = Outstream.output (os, s)
        in List.apply (out o p) (Array.list a);
           Outstream.closeOut os
        end


end
