(*$MonoVectorParse : PARSE MONO_SEQ_PARSE MonoVector ListParse
		     Instream Outstream *)

functor MonoVectorParse (
  structure MonoVector: MONO_VECTOR
  structure Parse: PARSE
    sharing type Parse.T = MonoVector.Element
): MONO_SEQ_PARSE =

(* PARSE FUNCTIONS FOR MONO_VECTORS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		8 Feb 1991

Maintenance:	Author


DESCRIPTION

   Read and parse functions for MonoVectors.  The functor must be given
   a MonoVector and a structure containing parse and read functions for
   the elements of the MonoVector.


SEE ALSO

   MonoVector, MonoArrayParse.


RCS LOG

$Log: MonoVectorParse.sml,v $
Revision 1.1  1994/02/08 00:23:20  jkh
Initial revision

Revision 1.3  91/02/22  19:10:53  19:10:53  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.

Revision 1.2  91/02/20  16:04:12  16:04:12  db (Dave Berry)
Minor corrections to file and fromFile functions.

Revision 1.1  91/02/11  20:28:55  20:28:55  db (Dave Berry)
Initial revision



*)

struct

(* TYPES *)

  type T = MonoVector.MonoVector


(* OBSERVERS *)

  val fixedWidth = false


(* CONVERTORS *)

  exception Sep of string * string * string * string

  exception Size of string * int

  (* The parse, parse' and read functions assume that entries have a fixed
     width or are separated by formatting characters. *)

  fun parseSepN start finish sep n s =
        (case ListParse.parseSepN start finish sep Parse.parse n s of
	   OK (v, s') => OK (MonoVector.fromList v, s')
	 | Fail (None, s') => Fail (None, s')
	 | Fail (Some v, s') => Fail (Some (MonoVector.fromList v), s')
	)
        handle ListParse.Sep x => raise Sep x

  fun parseSep start finish sep s =
        (case ListParse.parseSep start finish sep Parse.parse s of
	   OK (v, s') => OK (MonoVector.fromList v, s')
	 | Fail (None, s') => Fail (None, s')
	 | Fail (Some v, s') => Fail (Some (MonoVector.fromList v), s')
	)
        handle ListParse.Sep x => raise Sep x

  fun parseN n s =
        if n < 0 then raise Size ("parseN", n)
	else if Parse.fixedWidth
        then parseSepN "" "" "" n s
        else parseSepN "" "" " " n s

  fun parse s =
	if Parse.fixedWidth
        then parseSep "" "" "" s
        else parseSep "" "" " " s

  fun readSepN start finish sep n i =
        (case ListParse.readSepN start finish sep Parse.read n i of
	   OK v => OK (MonoVector.fromList v)
	 | Fail None => Fail None
	 | Fail (Some v) => Fail (Some (MonoVector.fromList v))
	)
        handle ListParse.Sep x => raise Sep x

  fun readSep start finish sep i =
        (case ListParse.readSep start finish sep Parse.read i of
	   OK v => OK (MonoVector.fromList v)
	 | Fail None => Fail None
	 | Fail (Some v) => Fail (Some (MonoVector.fromList v))
	)
        handle ListParse.Sep x => raise Sep x

  fun readN n i =
        if n < 0 then raise Size ("readN", n)
	else if Parse.fixedWidth
        then readSepN "" "" "" n i
        else readSepN "" "" " " n i

  fun read i =
	if Parse.fixedWidth
        then readSep "" "" "" i
        else readSep "" "" " " i

  fun fromFile name =
        let fun readList i =
                  case Parse.read i
                  of Fail _ => (Instream.closeIn i; [])
                  |  OK x => x :: readList i
        in MonoVector.fromList (readList (Instream.openIn name))
        end

  fun file v name =
	let val os = Outstream.openOut name
	in Outstream.output (os, MonoVector.string v);
	   Outstream.closeOut os
	end

end
