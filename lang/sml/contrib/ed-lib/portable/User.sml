(*$User : USER StreamPair *)

loadSig "USER";

structure User: USER =

(* USER INPUT FUNCTIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        24 Mar 1990

Maintenance:	Author


SEE ALSO

   StreamPair


RCS LOG

$Log: User.sml,v $
Revision 1.1  1994/02/08 00:23:20  jkh
Initial revision

Revision 1.3  91/01/25  20:21:46  20:21:46  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:28:07  17:28:07  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  15:53:25  15:53:25  db (Dave Berry)
Initial revision


*)

struct


(* MANIPULATORS *)

  val prompt = StreamPair.prompt StreamPair.std;

  val ask = StreamPair.ask StreamPair.std;

  val confirm = StreamPair.confirm StreamPair.std;

  val menu = StreamPair.menu StreamPair.std;
end
