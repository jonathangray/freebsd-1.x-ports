
# idServer.tcl -- Tcl/Tk script to create an integer id server.
#
# The following command will create an RPC server 
# socket on port 4545.
#
# If port 4545 is busy (used by another process), dp_MakeRPCServer 
# will return an error.  In that case, you'll have to choose 
# another (unused) port number, and you'll have to update 
# the idClient.tcl code to use your newly chosen port number.
#

dp_MakeRPCServer 4545;

# The variable myId will be the last id given, which
# we will increment whenever a new id is given.
#

set myId 0;

# Clients will use RPC to remotely invoke the following GetId 
# procedure, which will return to the client process a 
# new integer id.
#

proc GetId {} {
  global myId; 
  incr myId; 
  return $myId;
}

