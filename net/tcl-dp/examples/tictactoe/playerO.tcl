
#
# playerO.tcl -- Tcl/Tk script for player O, the RPC client.
#
# This script should be run after running the player X script.
#

set player O;
puts stdout "Tic Tac Toe - player O";

# Get information from user;
#

puts stdout "Enter the host address of player X: " nonewline;
gets stdin host;
puts stdout "Enter the port number of player X: " nonewline;
gets stdin port;

# Connect to server (player X) as an RPC client;
#

set server [MakeRPCClient $host $port];

# Instead of creating a local board object, we ask the server to
# distribute its board object to us;
#

source board.tcl;
RPC $server eval DistributeObject .board \$rpcFile board;

# Initialize the user interface;
#

source interface.tcl;

# Trigger a user interface update whenever the state slot of 
# the distributed board object changes;
#

SetTrigger .board state update;


