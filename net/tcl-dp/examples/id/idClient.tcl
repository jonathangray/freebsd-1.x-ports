
# idClient.tcl 	-- Tcl/Tk script that connects to and accesses the 
# 		-- integer id server (idServer).
#
# You will have to change the value of the host variable 
# to the address of the machine where the idServer is running.
# You may also have to change the value of the port variable, too.
# Look in idServer.tcl to also make sure the port variable values match.
#

set host linus.cs.berkeley.edu;
set port 4545;

# The following command connects (as an RPC client) to 
# the process that's running on machine "host" at port number "port".
# This remote process, of course, should be the idServer.
#
# dp_MakeRPCClient will return a file handle to represent the RPC
# connection to the idServer.  We'll save this file handle in
# the variable called server.
#

set server [dp_MakeRPCClient $host $port];

# The following command does an RPC to get a new id from the idServer.
#
# RPC will send the string "GetId" to the remote process, 
# the idServer, which will eval the received string.  Any return 
# value of that remote evaluation is sent back, which we will print.  
# That's it!
#

puts stdout [dp_RPC $server GetId];

