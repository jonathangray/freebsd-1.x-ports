
#
# enter.tcl -- Tcl/Tk script for conference client.
#

# Get information from user.
#

puts stdout "Enter the host address of the conference server: " nonewline;
gets stdin host;

puts stdout "Enter the port number of the conference server: " nonewline;
gets stdin port;

puts stdout "Enter your name: " nonewline;
gets stdin name;

# Become an RPC client of the conference server.  Store the
# file handle that represents the RPC connection in the 
# global variable server;
#

set server [MakeRPCClient $host $port]

# The conference server will occasionally RPC a Hear 
# command to us when we need to hear a message from some speaker.
#

proc Hear {speaker message} \
{
  puts stdout "$speaker --> $message";
}

# Commands available to user, who invokes these commands from stdin.
#

proc help {} \
{
  puts stdout "Conferencing Commands: ";
  puts stdout "\thelp";
  puts stdout "\twho";
  puts stdout "\tsay ?message?";
  puts stdout "\tbye or quit or leave";
}

proc who {} \
{
  global server;

  # RPC to the conference server to get the list of all its client names;
  #
  puts stdout [RPC $server set names];
}

proc say {args} \
{
  global server;

  # Tell the conference server, by RPC, what I want to said to all clients.
  # The conference server will repeat my message to all clients, by RPC,
  # for them to Hear.
  #
  RPC $server Say $args;
}

proc leave {} \
{
  global server;

  # Tell the conference server, by RPC, that I'm Leaving the conference.
  #
  RPC $server Leave;
  puts stdout "Left conference.";
  exit;
}

proc bye {} {leave};
proc quit {} {leave}

# On startup, automatically enter the conference.
#

proc enter {} \
{
  global server;
  global name;

  # Tell the conference server, by RPC, that I'm Entering the conference.
  #
  RPC $server Enter $name;
  puts stdout "Entered conference.";
}

enter;

