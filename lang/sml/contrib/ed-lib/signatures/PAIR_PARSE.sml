(*$PAIR_PARSE: InstreamType GeneralTypes *)

signature PAIR_PARSE =
sig

(* PARSE FUNCTIONS FOR PAIRS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		8 Feb 1991

Maintenance:	Author


DESCRIPTION

   Parse and read functions on the built-in type ('a * 'b).


NOTES

   These functions were originally in the main PAIR signature.


SEE ALSO

   PAIR.


RCS LOG

$Log: PAIR_PARSE.sml,v $
Revision 1.1  1994/02/08 00:23:26  jkh
Initial revision

Revision 1.1  91/02/11  19:18:36  19:18:36  db (Dave Berry)
Initial revision



*)



(* CONVERTORS *)

  val parse: (string -> ('a * string, 'c Option * string) Result) ->
  	     (string -> ('b * string, 'd Option * string) Result) ->
	     string -> (('a * 'b) * string, 'e Option * string) Result
   (* parse p1 p2 s; parses a pair from the beginning of s, using p1 and p2
      to parse the two elements. *)

  val read: (instream -> ('a, 'c) Result) ->
	    (instream -> ('b, 'd) Result) ->
	    instream -> ('a * 'b, unit) Result
   (* read p1 p2 i; reads a pair from i, using p1 and p2 to parse the
      two elements. *)

end
