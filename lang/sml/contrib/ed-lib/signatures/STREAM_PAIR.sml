(*$STREAM_PAIR: GeneralTypes InstreamType OutstreamType *)

signature STREAM_PAIR =
sig

(* PAIRS OF ONE INPUT STREAM AND ONE OUTPUT STREAM

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Nov 1989

Maintenance:	Author


DESCRIPTION

   A streampair is used to communicate with the user or with another
   process.  This package includes system functions to start other
   process and functions to ask the user for certain input.  It also
   provides most of the functions on outstreams and instreams, with
   the obvious inherited effect.


SEE ALSO

   INSTREAM, OUTSTREAM


NOTES

   System functions raise General.NotImplemented if the implementation
   doesn't support them.

   Possibly this module should provide functions for random access files.
   Alternatively such files could be viewed as (Ref)Vectors.

   Possibly the functions in this module should handle the Io exception
   and raise a local exception instead.

RCS LOG

$Log: STREAM_PAIR.sml,v $
Revision 1.1  1994/02/08 00:23:25  jkh
Initial revision

Revision 1.6  91/02/04  16:46:19  16:46:19  db (Dave Berry)
Renamed InStream, OutStream to instream, outstream, etc., as part of
reorganisation of the stream packages.
Renamed input' to read and output' to write.

Revision 1.5  91/01/26  13:43:59  13:43:59  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.4  91/01/25  19:06:32  19:06:32  db (Dave Berry)
Added dependence on GeneralTypes, InStreamType and OutStreamType.

Revision 1.3  91/01/25  16:57:31  16:57:31  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:08:37  17:08:37  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:56:58  16:56:58  db (Dave Berry)
Initial revision


*)

(* TYPES *)

  type StreamPair


(* CONSTANTS *)

  val std: StreamPair
   (* std = (stdIn, stdOut); *)


(* CREATORS *)

  val create: instream * outstream -> StreamPair
   (* create (i, os); create a StreamPair from two streams. *)


(* CONVERTORS *)

  val streams: StreamPair -> instream * outstream
   (* streams io; split a StreamPair into two streams. *)


(* SYSTEM *)

  val openTemporary: unit -> StreamPair
   (* openTemporary (); returns a pair of streams that are connected to
      each other. *)
 
  val canInput: StreamPair -> Nat -> bool
   (* canInput io n; returns true if n characters can be read from io
      without blocking. *)

  val flush: StreamPair -> unit
   (* flush io; ensures that all characters output on io have been or can
      be received by the associated consumer without being delayed by the
      implementation.  For example, characters output to a stream connected
      to a terminal will appear on that screen, even if they are normally
      buffered. *)

  val reset: StreamPair -> bool
   (* reset io; if io can be reset to the beginning, in some sense,
     this is done and true is returned.  Otherwise false is returned. *)

  val interactive: StreamPair -> bool
   (* interactive io; returns true if io is associated with a terminal. *)

  val execute: string -> (StreamPair, string) Result
   (* execute proc; creates a pair of streams connected to a system process.
      In the case that the process is an SML, C/UNIX or Pascal program, the
      instream is connected to the stdOut stream, stdout stream or output
      file of the process and the outstream is connected to the process's
      stdIn stream, stdin stream or input file, respectively. *)

  val read: StreamPair -> Nat -> string
   (* input io s; reads n characters from io, if io has not been closed.
      Has the same behaviour as Instream.read. *)


(* MANIPULATORS *)

  val openPair: string -> (StreamPair, string) Result
   (* openPair s; returns a pair of streams whose producer and consumer are
      associated with the file named s.  It must be possible to open s
      for both reading and writing.  If s does not exist, it is created
      if this is possible. *)

  val closePair: StreamPair -> unit
   (* closePair io; terminates the streams io. *)

  val output: StreamPair * string -> unit
   (* output (io, s); writes the characters in s to io, if io has not been
      closed.  Raises (General.Io ("output", file, "")) in case of failure. *)

  val write: StreamPair -> string -> unit
   (* write = General.curry output *)

  val input: StreamPair * Nat -> string
   (* input (io, s); reads n characters from io, if io has not been closed.
      Has the same behaviour as Instream.input. *)

  val input1: StreamPair -> string
   (* input1 io; reads 1 charcater from io, if io has not been closed.
      Has the same behaviour as Instream.input1. *)

  val lookahead: StreamPair -> string
   (* lookahead io; returns a single character from io.  Has the same behaviour
      as the pervasive lookahead if there isn't a character available. *)

  val eof: StreamPair -> bool
   (* eof io; returns true if the last attempt to lookahead or input on the
      input stream of io returned the empty string, false otherwise. *)

  val readString: StreamPair -> string -> (unit, string) Result
   (* readString io s; returns true if reading from io gives the characters
      in s.  Returns false as soon as a character is read that doesn't
      match the corresponding one in s.  In either case all characters read
      from i are lost.  Raises (EOF "readString") if the end of file is
      reached before all the charcaters in s have been read. *)

  val skip: (string -> bool) -> StreamPair -> unit
   (* skip p io; reads all characters from io that satisfy p.  Leaves the first
      character that doesn't satisfy p to be read.  Raises (EOF "skip") if
      the end of file is reached before a character is found that doesn't
      satisfy p. *)

  val inputLine: StreamPair -> string
   (* inputLine io; returns a string consisting of characters read from io
      up to and including the next end of line character.  If the end of the
      file is reached first, all characters up to the end of file are returned
      (without a new line character). *)

  val prompt: StreamPair -> string -> string
   (* prompt io s; writes s on io and reads an answer input line using
      inputLine. *)

  val ask: StreamPair -> string -> (instream -> ('a, 'a Option) Result) -> 'a
   (* ask io s p; repeatedly prompts for an answer using s as the prompt;
      reads an answer using p until p succeeds; returns the value read by p. *)

  val confirm: StreamPair -> string -> bool
   (* confirm s io; checks for confirmation with message s.  Returns true or
      false depending on the input.  If io is connected to an ASCII terminal,
      true might be indicated by typing "y" and false by typing "n", with
      other characters being ignored. *)

  val menu: StreamPair -> string -> string list -> int
   (* menu io title entries; writes a menu to io using title as a title
      and the elements of entries as entries.  Returns an integer that
      corresponds to the position of the chosen element in the list
      (0 to (size entries - 1)). *)
end
