(*
	Test the trivial idl server

	@(#)idl0.run.sml	2.1 93/03/07 00:58:13
*)

import          "../idlbase";
structure       idlbase=idlbase();
open    idlbase;
use     "idl0.sml";
idlbase.startserver "idl0" [];


