(*

	Test driver for idlcurses

	@(#)idlcurses.run.sml	2.1 93/03/07 00:58:15

*)

import "../Xidl/idlbase.sig";
import "../Xidl/idlbase";
import "../Xidl/idlbase";
structure idlbase=idlbase();
import "idlcurses";
structure idlcurses= idlcurses(idlbase);

open idlcurses;

fun curse() =
let val go = ref true
    and x  = ref 10
    and y  = ref 10
in
    Sync(); Cbreak(); Noecho();
    while !go do
    case (Refresh(); chr(Getch())) of
	 "."    => go := false
    |    "\n"   => (inc y; x:=0; Mvaddstr(!y, !x, ""))
    |    "^"    => (if !y>0 then dec y else (); Move(!y, !x+1))
    |    "\008" => (if !x>0 then dec x else (); Move(!y, !x+1))
    |     c   => if c>= " "
		 then (inc x; Move(!y, !x); Addstr(c))
		 else (inc x; Move(!y, !x); Addstr("??"); Move(!y, !x); dec x);
    Refresh()
end

fun main(_,_) =
(StartServer "idlcurses" [];
 curse();
 StopServer()
);

exportFn("/tmp/foo", main);
