(*$MONO_SEQ_PARSE: InstreamType GeneralTypes *)

signature MONO_SEQ_PARSE =

(* PARSE FUNCTIONS FOR MONOMORPHIC SEQUENCES

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        21 Feb 1989

Maintenance:	Author


DESCRIPTION

   Parse and read functions for monomorphic sequences and arrays,
   such as ByteVectors and BoolVectors.

   The design parallels that of the SEQ_PARSE signature, but replaces
   occurrences of 'a T with T.  See SEQ_PARSE for descrptions of the
   functions.


SEE ALSO

   SEQ_PARSE, MonoVectorParse, MonoArrayParse.


RCS LOG

$Log: MONO_SEQ_PARSE.sml,v $
Revision 1.1  1994/02/08 00:23:26  jkh
Initial revision

Revision 1.9  91/02/22  19:05:52  19:05:52  db (Dave Berry)
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.

Revision 1.8  91/02/20  16:00:47  16:00:47  db (Dave Berry)
Added file and fromFile functions.

Revision 1.7  91/02/11  18:49:48  18:49:48  db (Dave Berry)
Changed the name of this signature from MONO_SEQUENCE to MONO_SEQ_PARSE.
Removed inclusion of the OBJECT signature, and the stringSep function.
This forms part of the major reorganisation of the library.

Revision 1.6  91/02/04  15:38:40  15:38:40  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.5  91/01/26  13:43:41  13:43:41  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.4  91/01/25  19:02:44  19:02:44  db (Dave Berry)
Added dependence on InStreamType and/or GeneralTypes.

Revision 1.3  91/01/25  17:54:14  17:54:14  db (Dave Berry)
Added dependency on OBJECT to tag declaration, fixed include specification.

Revision 1.2  91/01/25  16:57:23  16:57:23  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.1  90/12/17  16:51:44  16:51:44  db (Dave Berry)
Initial revision


*)

sig

(* TYPES *)

  type T

(* CONVERTERS *)

  exception Sep of string * string * string * string

  exception Size of string * int

  val parseSepN: string -> string -> string ->
		 Nat -> string -> (T * string, T Option * string) Result

  val parseSep: string -> string -> string ->
                string -> (T * string, T Option * string) Result

  val parseN: Nat -> string -> (T * string, T Option * string) Result

  val parse: string -> (T * string, T Option * string) Result

  val readSep: string -> string -> string -> instream -> (T, T Option) Result

  val readSepN: string -> string -> string ->
		Nat -> instream -> (T, T Option) Result

  val readN: Nat -> instream -> (T, T Option) Result

  val read: instream -> (T, T Option) Result

  val fromFile: string -> T

  val file: T -> string -> unit

end
