
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

set server [dp_MakeRPCClient $host $port];

# Instead of creating a local board object, we ask the server to
# distribute its board object to us;
#

source board.tcl;
set remFile [dp_RPC $server set rpcFile]
dp_RPC $server eval dp_DistributeObject .board $remFile board;

# Initialize the user interface;
#

source interface.tcl;

# Trigger a user interface update whenever the state slot of 
# the distributed board object changes;
#

dp_SetTrigger after .board state DisplayUpdate


