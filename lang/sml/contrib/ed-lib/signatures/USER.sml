(*$USER: InstreamType GeneralTypes *)

signature USER =
sig

(* USER INPUT FUNCTIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        24 Mar 1990

Maintenance:	Author


DESCRIPTION

   These functions prompt the user for some input, and return an
   appropriate value.


SEE ALSO

   STREAM_PAIR


NOTES

   When using an ASCII terminal, these routines are the same as those in
   StreamPair, but always use the standard streams.

   A window system should provide an implementation of this signature that
   uses real menus and dialog boxes.


RCS LOG

$Log: USER.sml,v $
Revision 1.1  1994/02/08 00:23:24  jkh
Initial revision

Revision 1.6  91/02/04  15:39:00  15:39:00  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.5  91/01/26  13:44:02  13:44:02  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.4  91/01/25  19:10:29  19:10:29  db (Dave Berry)
Added dependence on GeneralTypes and/or InStreamType.

Revision 1.3  91/01/25  16:55:44  16:55:44  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:08:52  17:08:52  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:58:06  16:58:06  db (Dave Berry)
Initial revision


*)

(* MANIPULATORS *)

  val prompt: string -> string
   (* prompt s; displays s to the user and reads an answer input line. *)

  val ask: string -> (instream -> ('a, 'a Option) Result) -> 'a
   (* ask s p; repeatedly prompts for an answer using s as the prompt;
      reads an answer using p until p succeeds; returns the value read by p. *)

  val confirm: string -> bool
   (* confirm s; checks for confirmation with message s.  Returns true or
      false depending on the input.  When using an ASCII terminal,
      true might be indicated by typing "y" and false by typing "n", with
      other characters being ignored. *)

  val menu: string -> string list -> int
   (* menu title entries; displays a menu using title as a title
      and the elements of entries as entries.  Returns an integer that
      corresponds to the position of the chosen element in the list
      (0 to (size entries - 1)). *)
end
