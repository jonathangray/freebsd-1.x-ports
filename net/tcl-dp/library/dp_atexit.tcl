
# dp_atexit -- command to install a Tcl callback to be invoked when
#	 -- the exit command is evalutated.
#
#   exit -- command to exit process, after all callbacks installed by
#	 -- the dp_atexit command have been invoked.
#

#######################################################################
#
# dp_atexit -- manages dp_atexit callbacks.
#

proc dp_atexit {{option list} args} {

  # The option may be appendUnique, append, prepend, insert, delete,
  # clear, set, or list.
  # The args depends on the option specified.
  #

  # The dp_atexit_callbacks list is where we store the 
  # installed dp_atexit callbacks.
  #
  global dp_atexit_callbacks;
  if {[catch {set dp_atexit_callbacks}]} {
    set dp_atexit_callbacks {};
  }

  case $option in {
    set {
      #
      # set callbacks list.
      #
      set dp_atexit_callbacks $args;
    }
    appendUnique {
      #
      # append callback to end of the callbacks list.
      #
      if {[llength $args] != 1} {
	error {wrong # args : should be "dp_atexit appendUnique callback"};
      }
      set callback [lindex $args 0];
      if {[lsearch $dp_atexit_callbacks $callback] == -1} {
        lappend dp_atexit_callbacks $callback;
      }
    }
    append {
      #
      # append callback to end of the callbacks list.
      #
      if {[llength $args] != 1} {
	error {wrong # args : should be "dp_atexit append callback"};
      }
      set callback [lindex $args 0];
      lappend dp_atexit_callbacks $callback;
    }
    prepend {
      #
      # prepend callback to front of the callbacks list.
      #
      if {[llength $args] != 1} {
	error {wrong # args : should be "dp_atexit prepend callback"};
      }
      set callback [lindex $args 0];
      set dp_atexit_callbacks "\{$callback\} $dp_atexit_callbacks";
    }
    insert {
      #
      # insert callback before the "before" callback in the callbacks list.
      #
      if {[llength $args] != 2} {
	error {wrong # args : should be "dp_atexit insert before callback"};
      }
      set before   [lindex $args 0];
      set callback [lindex $args 1];
      set l {};
      foreach c $dp_atexit_callbacks {
	if {[string compare $before $c] == 0} {
	  lappend l $callback;
	}
	lappend l $c;
      }
      set dp_atexit_callbacks $l;
    }
    delete {
      #
      # delete callback from the callbacks list.
      #
      if {[llength $args] != 1} {
	error {wrong # args : should be "dp_atexit delete callback"};
      }
      set callback [lindex $args 0];
      set l {};
      foreach c $dp_atexit_callbacks {
	if {[string compare $callback $c] != 0} {
	  lappend l $c;
	}
      }
      set dp_atexit_callbacks $l;
    }
    clear {
      #
      # clear callbacks list.
      #
      if {[llength $args] != 0} {
	error {wrong # args : should be "dp_atexit clear"};
      }
      set dp_atexit_callbacks {};
    }
    list {
      #
      # list currently installed callbacks.
      #
    }
    default {
      error {options, append, prepend, insert, delete, clear, set, or list};
    }
  }
  return $dp_atexit_callbacks;
}

#######################################################################
#
# Hide real exit command.
#

rename exit dp_atexit_exit;

#######################################################################
#
# exit -- Wrapper exit command that first invokes all callbacks installed
#      -- by the dp_atexit command before doing real exit.
#

proc exit {{code 0}} {
  global dp_atexit_callbacks;

  while {1} {

    # Every iteration, we rescan dp_atexit_callbacks, in case
    # some callback modifies it.
    #
    if {[catch {set dp_atexit_callbacks} callbacks]} {
      break;
    }
    if {[llength $callbacks] <= 0} {
      break;
    }
    set        callback  [lindex $callbacks 0];
    set dp_atexit_callbacks [lrange $callbacks 1 end];

    catch {uplevel #0 $callback};
  }

  catch {unset dp_atexit_callbacks};
  catch {dp_atexit_exit $code};
}

