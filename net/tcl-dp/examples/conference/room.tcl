
#
# room.tcl -- Tcl/Tk script for conferencing server;
#

puts stdout "Conference server";
puts stdout "Enter an unsed port number (ex: 7654) : " nonewline;
gets stdin port;

# The following command creates an RPC server socket on
# the given port.
#

MakeRPCServer $port

# names, files -- list of room occupants and their associated rpcFile's:
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
}

proc Leave {} \
{
  global names;
  global files;
  global rpcFile;

  # Forget the name and the associated RPC file handle
  # of the client who is leaving the room;
  #
  set position [lsearch $files $rpcFile];

  set names [ldelete $names [lindex $names $position]];
  set files [ldelete $files $rpcFile];
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
      RPC $client Hear $speaker $message;
    }
}

