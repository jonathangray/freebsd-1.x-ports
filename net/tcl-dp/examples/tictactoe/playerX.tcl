
#
# playerX.tcl -- Tcl/Tk script for player X, the RPC server.
#
# This script should be run before running the player O script.
#

set player X;
puts stdout "Tic Tac Toe - player X";

# Get information from user;
#

puts stdout "Enter an unsed port number (ex: 8765) : " nonewline;
gets stdin port;

# Make an RPC server socket, which will be waiting for player O 
# to connect through the supplied port number;
#

MakeRPCServer $port

# Create a local board object that will get distributed to player O;
#

source board.tcl;
board .board;

# Initialize the user interface;
#

source interface.tcl;

# Trigger a user interface update whenever the state slot of 
# the distributed board object changes;
#

SetTrigger .board state update;

