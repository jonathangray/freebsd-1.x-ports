# rpc.tcl --
#
# Utilities to create reentrant RPC clients and servers (full duplex)
#
# This file contains the utility procedures to help implement 
# user-friendly remote procedure calls (RPC's) on top of the 
# network primitives provided by the connect, accept, etc. and 
# by various other primitives.
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

########################################################################

proc AcceptRPCConnection {status file} {
    if {[string compare $status e] == 0} {
	filehandler $file
	close $file;
	return; 
    }
    set connection [accept $file]
    atexit "CloseRPC $connection"
    filehandler $connection r ProcessRPCCommand
}

########################################################################

proc MakeRPCClient {host port} {
    set client [lindex [connect $host $port] 0]
    filehandler $client r ProcessRPCCommand
    atexit "CloseRPC $client"
    return $client
}

proc MakeRPCServer {port} {
    if {$port == 0} {
	set rv [connect -server "" 0]
	set server [lindex $rv 0]
	set port [lindex $rv 1]
    } else {
	set server [lindex [connect -server "" $port] 0]
    }
    filehandler $server r AcceptRPCConnection
    atexit "close $server"
    return $port
}

proc CloseRPC {file} {
    RDO $file CloseRPCFile
    filehandler $file
    close $file
}

########################################################################

proc CloseRPCFile {} {
    global rpcFile
    filehandler $rpcFile
    close $rpcFile
}

###########################################################################
#
# Trap read errors on sockets and close the socket
#
proc tkerror {info} {
    case $info in {
        Tcp_PacketReceive*error*reading*file*unknown*error*(0) {
            set f [lindex [split $info] 4]
            filehandler $f
            close $f
            }
        RPC*error*Tcm_ProcessIncomingMessages {
            }
        default {
            error "Unknown error condition"
            }
    }
}

########################################################################

set RPROCtable(null) {};

proc RPROC {name arguments body} {

  # RPROC defines a procedure that is callable by RPC clients;
  # RPROC is semantic sugar.
  #
  proc $name $arguments $body;

  # Record the RPROC in the RPROCtable;
  #
  global RPROCtable;
  set RPROCtable($name) $name;
}

########################################################################
#
# auto_load_all
#
# This procedure source's all Tcl library scripts not already source'd.
# This procedure is useful for when you want to later undefine
# the "proc" command, for making your interpreter RPC safe.
#

proc auto_load_all {} {
  global auto_index;

  foreach name [array names auto_index] {
    if {[string length [info commands $name]] == 0} {
      uplevel #0 source $auto_index($name);
    }
  }
}

