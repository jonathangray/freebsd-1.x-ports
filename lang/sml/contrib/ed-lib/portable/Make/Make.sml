loadEntry "Make0";

(* BUILD SEQUENCE

Created by:     Nick Rothwell, LFCS, University of Edinburgh
                nick@lfcs.ed.ac.uk
Date:           30 Oct 1990

                Modified to fit the library structure by Dave Berry,
                24 Jan 1991.

Maintenance:    Author


DESCRIPTION

   This is the build sequence for the Make system.


RCS LOG

$Log: Make.sml,v $
Revision 1.1  1994/02/08 00:23:22  jkh
Initial revision

Revision 1.3  91/01/30  18:59:09  18:59:09  db (Dave Berry)
Renamed loadFun and loadStr to loadEntry.

Revision 1.2  91/01/25  20:13:26  20:13:26  db (Dave Berry)
Prefixed local signature and functor names with MAKE_ or Make respectively.

Revision 1.1  91/01/25  11:41:26  11:41:26  db (Dave Berry)
Initial revision


*)


functor Make(CoreUtils: CORE_UTILS): MAKE =
  Make0(structure CoreUtils = CoreUtils
	structure Global = MakeGlobal(CoreUtils)
	structure Busy = MakeBusy(CoreUtils)
	structure Patches =
	  struct
	    fun Trace _ = ()
	    val Use = CoreUtils.use
	  end
       );

structure Make = Make(open CoreUtils);
