# oo.tcl --
#
# Simple object oriented extension to Tcl, in the spirit of the Tk 
# widget "objects".  Like the GUI widgets in Tk, objects in
# this extension are procedures.
#
# Copyright 1992 Regents of the University of California
# Permission to use, copy, modify, and distribute this
# software and its documentation for any purpose and without
# fee is hereby granted, provided that this copyright
# notice appears in all copies.  The University of California
# makes no representations about the suitability of this
# software for any purpose.  It is provided "as is" without
# express or implied warranty.
#

################################################################
#
# objectCreateProc
#
#	creates a procedure <object> to represent an
#	object of given <class>.  
#
#	methods on <class> should be defined using
#	the form <class>.method1, ..., <class>.methodn. 
#

proc objectCreateProc {class object} {

  set    body "";
  append body "eval $class.";
  append body {[lindex $args 0]};
  append body " $object ";
  append body {[lrange $args 1 end]};

  proc $object args $body;

  return $object;
}

################################################################
#
# objectExists
#

proc objectExists {object} {

  if {[lsearch [info procs] $object] < 0} {
    return 0;
  }
  return 1;
}

################################################################
#
# objectFree
#

proc objectFree {object} {
  global _objects;

  if [objectExists $object] {

    foreach slot [objectSlots $object] {
      objectSlotFree $object $slot;
    }

    rename $object "";
  }
}

################################################################
#
# objectSlot
# objectSlotSet
# objectSlotAppend
#
# objectSlotFree
#
#	object slot abstraction implemented 
#	using associative arrays.
#

#------------------------------------------------------------

set _objects(null) {};

#------------------------------------------------------------

proc objectSlotFree {object slot} {
  global _objects;

  catch {unset _objects($object,$slot)};
}

proc objectSlot {object slot} {
  global _objects;

  return [set _objects($object,$slot)];
}

proc objectSlotSet {object slot value} {
  global _objects;

  return [set _objects($object,$slot) $value];
}

proc objectSlotAppend {object slot value} {
  global _objects;

  return [lappend _objects($object,$slot) $value];
}

proc objectSlots {object} {
  global _objects;

  set objectSlots {};

  set slots  [array  names  _objects];
  set length [string length $object,];

  while {1} {

    set index [lsearch $slots $object,*];
    if {$index < 0} {
      return $objectSlots;
    }

    lappend objectSlots \
      [string range [lindex $slots $index] $length end];

    set slots [lrange $slots [incr index] end];
  }
}

################################################################
#
# objectConfigure - configure the slots of an object.
#

proc objectConfigure {class object args} {

  set argc [llength $args];

  if {$argc < 1} {

    # Zero args;  
    #
    # Return a list of all the slotnames and values of the object;
    #
    set configs {};
    foreach slot [objectSlots $object] {
      lappend configs [list -$slot {} [objectSlot $object $slot]];
    }
    return $configs;
  }

  if {$argc == 1} {

    # One arg (slotname);
    #
    # Return the slotname and value for the given slotname in the object;
    #
    set slot [string trimleft [lindex $args 0] \-];

    if {[string length $slot] > 0} {
      return [list -$slot {} [objectSlot $object $slot]];
    }
  }

  if {$argc > 1} {

    # More than one args (slotname and value pairs);  
    #
    # Recursively set the value of each of the given slots in the object
    # to the given values;
    #
    set slot [string trimleft [lindex $args 0] \-];

    eval $class.configure $object [lrange $args 2 end];

    objectSlotSet $object $slot [lindex $args 1];

    return $object;
  }
}

