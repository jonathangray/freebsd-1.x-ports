(*$MonoArrayParse : PARSE MONO_SEQ_PARSE MonoArray ListParse
		    Instream Outstream *)

functor MonoArrayParse (
  structure MonoArray: MONO_ARRAY
  structure Parse: PARSE
    sharing type Parse.T = MonoArray.Element
): MONO_SEQ_PARSE =

(* PARSE FUNCTIONS FOR MONO_ARRAYS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		8 Feb 1991

Maintenance:	Author


DESCRIPTION

   Read and parse functions for MonoArrays.  The functor must be given
   a MonoArray and a structure containing parse and read functions for
   the elements of the MonoArray.


SEE ALSO

   MonoVectorParse, MonoArray.


RCS LOG

$Log: MonoArrayParse.sml,v $
Revision 1.1  1994/02/08 00:23:19  jkh
Initial revision

Revision 1.3  91/02/22  19:11:01  19:11:01  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.

Revision 1.2  91/02/20  16:04:31  16:04:31  db (Dave Berry)
Minor corrections to file and fromFile functions.

Revision 1.1  91/02/11  20:26:54  20:26:54  db (Dave Berry)
Initial revision



*)

struct


(* TYPES *)

  type T = MonoArray.MonoArray


(* OBSERVERS *)

  val fixedWidth = false


(* CONVERTORS *)

  exception Sep of string * string * string * string

  exception Size of string * int

  (* The parse, parse' and read functions assume that entries have a fixed
     width or are separated by formatting characters. *)

  fun parseSepN start finish sep n s =
        (case ListParse.parseSepN start finish sep Parse.parse n s of
	   OK (v, s') => OK (MonoArray.fromList v, s')
	 | Fail (None, s') => Fail (None, s')
	 | Fail (Some v, s') => Fail (Some (MonoArray.fromList v), s')
	)
        handle ListParse.Sep x => raise Sep x

  fun parseSep start finish sep s =
        (case ListParse.parseSep start finish sep Parse.parse s of
	   OK (v, s') => OK (MonoArray.fromList v, s')
	 | Fail (None, s') => Fail (None, s')
	 | Fail (Some v, s') => Fail (Some (MonoArray.fromList v), s')
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
	   OK v => OK (MonoArray.fromList v)
	 | Fail None => Fail None
	 | Fail (Some v) => Fail (Some (MonoArray.fromList v))
        )
        handle ListParse.Sep x => raise Sep x

  fun readSep start finish sep i =
        (case ListParse.readSep start finish sep Parse.read i of
	   OK v => OK (MonoArray.fromList v)
	 | Fail None => Fail None
	 | Fail (Some v) => Fail (Some (MonoArray.fromList v))
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
        in MonoArray.fromList (readList (Instream.openIn name))
        end

  fun file v name =
	let val os = Outstream.openOut name
	in Outstream.output (os, MonoArray.string v);
	   Outstream.closeOut os
	end


end
