(*$CONST *)

signature CONST =
sig

(* TAGGED VALUES

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		12 Dec 1989

Maintenance:	Author


DESCRIPTION

   The 'a Const type associates a value and a unique tag.  The tests for
   equality compare the tags of their arguments.


NOTES

   If 'a doesn't admit equality, then neither does 'a Const.  However,
   the functions eq and ne will still work.

   Ideally we would like the constructor Const to be visible, so that
   we could use it like ref.  We can't do that because it would show
   the internal representation.


RCS LOG

$Log: CONST.sml,v $
Revision 1.1  1994/02/08 00:23:24  jkh
Initial revision

Revision 1.4  91/02/11  18:19:22  18:19:22  db (Dave Berry)
Added comments, renamed Create to create.

Revision 1.3  91/01/25  16:54:58  16:54:58  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:06:14  17:06:14  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:47:37  16:47:37  db (Dave Berry)
Initial revision


*)


(* TYPES *)

  type 'a Const


(* CREATORS *)

  val create: 'a -> 'a Const
   (* create x; associate a unique tag with x. *)


(* OBSERVERS *)

  val eq: 'a Const -> 'a Const -> bool
   (* eq x y; returns true if x and y are values associated with the same tag.
      Returns false otherwise. *)

  val ne: 'a Const -> 'a Const -> bool
   (* ne x y; returns true if x and y are values associated with different
      tags.  Returns false otherwise. *)


(* SELECTORS *)

  val !! : 'a Const -> 'a
   (* !! x; returns the value part of x, discarding the tag. *)

end

