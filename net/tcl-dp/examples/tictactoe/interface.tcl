
# interface.tcl -- Tcl/Tk script that defines the GUI interface
# of the Tic Tac Toe game;  Since both player X and player O 
# share the same interface, they both use this script.
#

# The global variable player must be set up beforehand;
#

wm title . "TTT - $player";

# The following procedure is the callback for when a user 
# presses a Tic Tac Toe GUI button;
#

proc mark {col row} {
  global player;

  # Get the current entry of the board of the given column and row.
  #
  set entry [.board entryGet $col $row];

  if {[string compare S $entry] == 0} {

    # Set the entry of the board to the player's symbol, 
    # if the current entry is an S (space).
    #
    .board entrySet $col $row $player;

    # Since the board is a distributed object, any slot changes to 
    # it will get automatically distributed the other player.
    #

  } else {

    # Otherwise, complain to the user;
    #
    puts stdout "";
    puts stdout "Cannot mark that entry ($col,$row).";
    puts stdout "Already marked with an $entry.";
  }
}

# Procedure to create a column of Tic Tac Toe GUI buttons;
#

proc column {w i} {
  pack append $w \
    [frame $w.c$i] {left fillx filly};
  pack append $w.c$i \
    [button $w.c$i.0 -command "mark $i 0" -width 2] {top fillx} \
    [button $w.c$i.1 -command "mark $i 1" -width 2] {top fillx} \
    [button $w.c$i.2 -command "mark $i 2" -width 2] {top fillx};
}

# Procedure to update the text of those Tic Tac Toe GUI buttons 
# according to the state of the distributed board object;
#

proc DisplayUpdate {} {
  for {set row 0} {$row <= 2} {incr row} {
    for {set col 0} {$col <= 2} {incr col} {
      set entry [.board entryGet $col $row];
      case $entry in {
	{S} {set symbol " "}
	{default} {set symbol $entry}
      }
      .main.c$col.$row configure -text $symbol;
    }
  }
}

# Instantiate the interface;
#

pack append . [frame .main] top;
column .main 0;
column .main 1;
column .main 2;

pack append .main \
  [button .main.clear -text "Clear" -command ".board clear"] \
    {top fillx} \
  [button .main.quit  -text "Quit"  -command "exit"] \
    {top fillx};

