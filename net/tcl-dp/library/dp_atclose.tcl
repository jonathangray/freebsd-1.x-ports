
# dp_atclose -- command to install a Tcl callback to be invoked when
#	  -- the close command is evalutated.
#
#   close -- command to close process, after all callbacks installed by
#	  -- the dp_atclose command have been invoked.
#

#######################################################################
#
# dp_atclose -- manages atclose callbacks.
#

proc dp_atclose {fileId {option list} args} {

  # The option may be appendUnique, append, prepend, insert, delete,
  # clear, set, or list.
  # The args depends on the option specified.
  #

  # The dp_atclose_callbacks array holds the installed dp_atclose callbacks,
  # indexed by fileId.
  #
  global dp_atclose_callbacks;
  if {[catch {set dp_atclose_callbacks($fileId)}]} {
    set dp_atclose_callbacks($fileId) {};
  }

  case $option in {
    set {
      #
      # set callbacks list.
      #
      set dp_atclose_callbacks($fileId) $args;
    }
    appendUnique {
      #
      # append callback to end of the callbacks list.
      #
      if {[llength $args] != 1} {
	error {wrong # args: try "dp_atclose fileId appendUnique callback"};
      }
      set callback [lindex $args 0];
      if {[lsearch $dp_atclose_callbacks($fileId) $callback] == -1} {
	  lappend dp_atclose_callbacks($fileId) $callback;
      }
    }
    append {
      #
      # append callback to end of the callbacks list.
      #
      if {[llength $args] != 1} {
	error {wrong # args: try "dp_atclose fileId append callback"};
      }
      set callback [lindex $args 0];
      lappend dp_atclose_callbacks($fileId) $callback;
    }
    prepend {
      #
      # prepend callback to front of the callbacks list.
      #
      if {[llength $args] != 1} {
	error {wrong # args: try "dp_atclose fileId prepend callback"};
      }
      set callback [lindex $args 0];
      set dp_atclose_callbacks($fileId) \
	"\{$callback\} $dp_atclose_callbacks($fileId)";
    }
    insert {
      #
      # insert callback before the "before" callback in the callbacks list.
      #
      if {[llength $args] != 2} {
	error {wrong # args: try "dp_atclose fileId insert before callback"};
      }
      set before   [lindex $args 0];
      set callback [lindex $args 1];
      set l {};
      foreach c $dp_atclose_callbacks($fileId) {
	if {[string compare $before $c] == 0} {
	  lappend l $callback;
	}
	lappend l $c;
      }
      set dp_atclose_callbacks($fileId) $l;
    }
    delete {
      #
      # delete callback from the callbacks list.
      #
      if {[llength $args] != 1} {
	error {wrong # args : should be "dp_atclose fileId delete callback"};
      }
      set callback [lindex $args 0];
      set l {};
      foreach c $dp_atclose_callbacks($fileId) {
	if {[string compare $callback $c] != 0} {
	  lappend l $c;
	}
      }
      set dp_atclose_callbacks($fileId) $l;
    }
    clear {
      #
      # clear callbacks list.
      #
      if {[llength $args] != 0} {
	error {wrong # args : should be "dp_atclose fileId clear"};
      }
      set dp_atclose_callbacks($fileId) {};
    }
    list {
      #
      # list currently installed callbacks.
      #
    }
    default {
      error {options: appendUnique, append, prepend, insert, delete, clear, set, or list};
    }
  }
  return $dp_atclose_callbacks($fileId);
}

#######################################################################
#
# Hide real close command.
#

rename close dp_atclose_close;

#######################################################################
#
# close -- Wrapper close command that first invokes all callbacks installed
#       -- by the dp_atclose command before doing real close.
#

proc close {fileId} {
  global dp_atclose_callbacks;

  while {1} {

    # Every iteration, we rescan dp_atclose_callbacks, in case
    # some callback modifies it.
    #
    if {[catch {set dp_atclose_callbacks($fileId)} callbacks]} {
      break;
    }
    if {[llength $callbacks] <= 0} {
      break;
    }

    set         callback           [lindex $callbacks 0];
    set dp_atclose_callbacks($fileId) [lrange $callbacks 1 end];

    catch {uplevel #0 $callback};
  }

  catch {unset dp_atclose_callbacks($fileId)};
  dp_atclose_close $fileId
}
