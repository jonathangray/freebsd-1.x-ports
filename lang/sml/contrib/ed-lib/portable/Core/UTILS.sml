loadLocalSig "NON_STANDARD";

signature CORE_UTILS =

(* CORE UTILITY FUNCTIONS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           24 Jan 1991

Maintenance:    Author


DESCRIPTION

   Most of these functions are used by the make system and the set entries
   (which are also used by the make system).  dropPrefix is used in several
   other entries.  


NOTES

   substring is not defined the same way as String.extract.  substring is
   defined this way by several compilers, but doesn't fit the conventions
   of the main library.


RCS LOG

$Log: UTILS.sml,v $
Revision 1.1  1994/02/08 00:23:22  jkh
Initial revision

Revision 1.5  91/02/12  17:25:26  17:25:26  db (Dave Berry)
Removed listEq.

Revision 1.4  91/01/30  18:02:37  18:02:37  db (Dave Berry)
Removed DropPrefix function.  This is no longer used by the various
parse functions in the library.

Revision 1.3  91/01/25  20:00:24  20:00:24  db (Dave Berry)
Moved inputLine to Make/Global.

Revision 1.2  91/01/25  15:43:29  15:43:29  db (Dave Berry)
Moved option, substring and fold to Make/Global.
Deleted old version of member, replaced it with eqMember.

Revision 1.1  91/01/25  11:29:31  11:29:31  db (Dave Berry)
Initial revision


*)

sig
  include NON_STANDARD

  val before: 'a * 'b -> 'a

  val unzip: ('a * 'b) list -> 'a list * 'b list

  val intToString: int -> string

  val length: 'a list -> int

  val member: ''a -> ''a list -> bool
end
