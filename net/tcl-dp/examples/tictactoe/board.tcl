
#
# board.tcl -- Tcl/Tk script that defines a Tic Tac Toe board object.
#
# The board object is defined using Tcl scripting only.  
# Normally, objects are created in C (as structs), such as 
# with Tk widgets, and have a Tcl access interface.
#
# The board has one slot, state, which is a list of nine entries.
# Each entry may be X, O, or S (space).
#

# board -- creator procedure, works like a Tk widget creator.
#
# 	usage : board name ?configure-options ...?
#

proc board {aBoard args} {

  # Create a procedure to represent a new board object.  
  # This new board object (procedure) will have the name of $aBoard;
  #
  objectCreateProc board $aBoard;

  # Give the new board object one slot, called state;
  #
  objectSlotSet $aBoard state {S S S S S S S S S};

  # Finish configuration of the new board object;
  #
  eval $aBoard configure $args;

  return $aBoard;
}

#
# Method definitions for objects of class Tic Tac Toe board;
#

# Distributed objects must have the methods of configure and slot-value,
# according to Tcl-DP distributed object protocol;
#
# configure  - configure the slots of board object;
# slot-value - return value of one slot of board object;
#
# The methods configure and slot-value that are defined here 
# use the object utility procedures that come 
# with the Tcl-DP package;
#

proc board.configure {aBoard args} {
  return [eval objectConfigure board $aBoard $args];
}

proc board.slot-value {aBoard slot} {
  return [objectSlot $aBoard $slot];
}

# In the following board methods, note the use of setf and getf 
# instead of objectSlotSet and objectSlot;  setf and getf
# are used for slot access to Tcl-DP distributed objects;
#

# clear - clear entries of board;
#

proc board.clear {aBoard} {
  setf $aBoard state {S S S S S S S S S};
  return $aBoard;
}

# entryGet - get entry at position x,y (0-2, 0-2);
# entrySet - set entry at position x,y (0-2, 0-2);
#

proc board.entryGet {aBoard x y} {
  set state [getf $aBoard state];
  set position [expr ($x*3)+$y];
  return [lindex $state $position];
}

proc board.entrySet {aBoard x y value} {
  set state [getf $aBoard state];
  set position [expr ($x*3)+$y];
  setf $aBoard state [lreplace $state $position $position $value];
  return $value;
}

# print - pretty print board to stdout;
#

proc board.print {aBoard} {
  for {set row 0} {$row <= 2} {incr row} {
    for {set col 0} {$col <= 2} {incr col} {
      set entry [$aBoard entryGet $col $row];
      case $entry in {
	{S} {set symbol " "}
	{default} {set symbol $entry}
      }
      puts stdout $symbol nonewline;
    }
    puts stdout "";
  }
}

