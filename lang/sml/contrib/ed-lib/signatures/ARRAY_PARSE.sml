(*$ARRAY_PARSE: GeneralTypes InstreamType Array *)

signature ARRAY_PARSE =
sig

(* FUNCTIONS TO READ AND PARSE ARRAYS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		7 Feb 1991

Maintenance:	Author


DESCRIPTION

   Functions to read and parse arrays.  These functions differ from those
   in SEQ_PARSE in that they have imperative type variables.  See SEQ_PARSE
   for descriptions of each function.


SEE ALSO

   SEQ_PARSE, ARRAY.


NOTES

   These functions were originally in the main ARRAY signature.


RCS LOG

$Log: ARRAY_PARSE.sml,v $
Revision 1.1  1994/02/08 00:23:23  jkh
Initial revision

Revision 1.4  91/02/22  19:06:08  19:06:08  db (Dave Berry)
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.

Revision 1.3  91/02/14  16:50:31  16:50:31  db (Dave Berry)
Removed unnecessary imperative attribute on the type variable in the type
of the file function.

Revision 1.2  91/02/12  12:18:10  12:18:10  db (Dave Berry)
Changed type to eqtype.

Revision 1.1  91/02/11  18:10:39  18:10:39  db (Dave Berry)
Initial revision



*)


(* TYPES *)

  eqtype 'a T 
    sharing type T = Array.Array


(* CONVERTERS *)

  exception Sep of string * string * string * string

  exception Size of string * int

  val parseSepN: string -> string -> string ->
            (string -> ('_a * string, 'b) Result) -> Nat -> string ->
	    ('_a Array.Array * string, '_a Array.Array Option * string) Result

  val parseSep: string -> string -> string ->
            (string -> ('_a * string, 'b) Result) -> string ->
	    ('_a Array.Array * string, '_a Array.Array Option * string) Result

  val parseN: (string -> ('_a * string, 'b) Result) -> Nat -> string ->
	    ('_a Array.Array * string, '_a Array.Array Option * string) Result

  val parse: (string -> ('_a * string, 'b) Result) -> string ->
	    ('_a Array.Array * string, '_a Array.Array Option * string) Result

  val readSep: string -> string -> string ->
            (instream -> ('_a, 'b) Result) -> instream ->
            ('_a Array.Array, '_a Array.Array Option) Result

  val readSepN: string -> string -> string ->
            (instream -> ('_a, 'b) Result) -> Nat -> instream ->
            ('_a Array.Array, '_a Array.Array Option) Result

  val read: (instream -> ('_a, 'b) Result) -> instream ->
	    ('_a Array.Array, '_a Array.Array Option) Result

  val readN: (instream -> ('_a, 'b) Result) -> Nat -> instream ->
            ('_a Array.Array, '_a Array.Array Option) Result

  val fromFile: (instream -> ('_a, 'b) Result) -> string -> '_a Array.Array
   (* fromFile p name; read the contents of the file called name into a vector.
      Stops reading from the file as soon as p returns Fail.
      Raises General.Io if something goes wrong. *)

  val file: ('a -> string) -> 'a Array.Array -> string -> unit
   (* file p v name; write the contents of v to the new file called name.
      Raises General.Io if something goes wrong. *)

end
