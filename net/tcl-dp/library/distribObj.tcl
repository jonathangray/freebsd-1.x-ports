# distribObj.tcl --
#
# This file contains utilities to manage distributed objects.
#
# Copyright 1992 Regents of the University of California
# Permission to use, copy, modify, and distribute this
# software and its documentation for any purpose and without
# fee is hereby granted, provided that this copyright
# notice appears in all copies.  The University of California
# makes no representations about the suitability of this
# software for any purpose.  It is provided "as is" without
# express or implied warranty.
#

proc SetTrigger {obj slot args} {
    global trigger
    set trigger($obj,$slot) $args
}


proc DistributeObject {obj processes makeCmd} {
    global slotClients
    foreach conf [$obj configure] {
	set slot [string trimleft [lindex $conf 0] -]
	lappend params -$slot [lindex $conf 2]
	eval lappend slotClients($obj,$slot) $processes;
    }
    set slotClients($obj,$slot) [lremovedup $slotClients($obj,$slot)];
    foreach proc $processes {
	RPC $proc CreateRemoteObject $makeCmd $obj $params
    }
}

proc UndistributeObject {obj processes} {
   global slotClients;

   set slots {};
   foreach conf [$obj configure] {
     set slot [string trimleft [lindex $conf 0] -];
     lappend slots $slot;
   }
   foreach slot $slots {
     if {![catch {set slotClients($obj,$slot)}]} {
       set slotClients($obj,$slot) [lremovedup $slotClients($obj,$slot)];
       foreach client $processes {
	 if {[lsearch $slotClients($obj,$slot) $client] >= 0} {
	   set slotClients($obj,$slot) \
	     [ldelete $slotClients($obj,$slot) $client];
    	   catch {RPC $client unset slotOwner($obj,$slot)};
	 }
       }
     }
   }
}

proc CreateRemoteObject {makeCmd obj params} {
    global rpcFile
    global slotOwner
    global slotClients
    eval $makeCmd $obj $params
    foreach conf [$obj configure] {
	set slot [string trimleft [lindex $conf 0] -]
	set slotOwner($obj,$slot) $rpcFile
    }
}

proc SetSlotOwner {obj slot process} {
    global slotOwner
    global slotClients
    set slotOwner($obj,$slot) $process
    set slotClients($obj,$slot) [ldelete $slotClients($obj,$slot) $process]
    RPC $process unset slotOwner($obj,$slot); \
		 eval lappend slotClients($obj,$slot) \$rpcFile
}

proc setf {obj slot value {force 0}} {
    set currVal [$obj slot-value $slot]
    if {$force || ($currVal != $value)} {
	global slotOwner
	global slotClients
	set local [catch {set owner $slotOwner($obj,$slot)}]
	if $local {
	    downsetf $obj $slot $value
	} else {
	    RDO $owner setf $obj $slot $value
	}
    }
}

proc downsetf {obj slot value} {
    global trigger
    global slotOwner
    global slotClients

    $obj configure -$slot $value
    set error [catch {set cmd $trigger($obj,$slot)}]
    if {!$error} {uplevel #0 eval $cmd}

    set error [catch {set clients $slotClients($obj,$slot)}]
    if {!$error} {
	foreach client $clients {
	    RDO $client downsetf $obj $slot $value
	}
    }
}

proc getf {obj slot} {$obj slot-value $slot}

