
#
# room.tcl -- Tcl/Tk script for conferencing server;
#

puts stdout "Conference server";
puts stdout "Enter an unsed port number (ex: 7654) : " nonewline;
gets stdin port;

# The following command creates an RPC server socket on
# the given port.
#

dp_MakeRPCServer $port

# names -- list of room occupants
# files -- list of active rpc files
#

set names {};
set files {};

# The following are procedures available through RPC to 
# clients of the conferencing server.
#

proc Enter {name} \
{
  # Before any RPC command (such as this Enter) gets evaluated, 
  # the global variable rpcFile is set to the file handle
  # of the RPC connection currently being serviced.
  #
  global names;
  global files;
  global rpcFile;

  # Remember the name and the associated RPC file handle
  # of the client who just entered the room;
  #
  lappend names $name; 
  lappend files $rpcFile;

  # Arrange to remove the information from the names and files list
  # when the connection is closed. 
  dp_atclose $rpcFile append "Leave $name $rpcFile"
}

# The following command is invoked automatically when a file is closed
# This can happend because a server dies unexpectedly.  The callback is
# created by the "dp_atclose" call above.  The two parameters give the
# rpc file and the name of the person.
#
proc Leave {n f} \
{
    global names;
    global files;

    set files [ldelete $files $f]
    set names [ldelete $names $n]
}

proc Say {message} \
{
  global names;
  global files;
  global rpcFile;

  # Figure out the client who said the message;
  #
  set speaker [lindex $names [lsearch $files $rpcFile]];

  # Tell all clients to Hear the message from the speaker;
  #
  foreach client $files \
    {
      dp_RPC $client Hear $speaker $message;
    }
}

