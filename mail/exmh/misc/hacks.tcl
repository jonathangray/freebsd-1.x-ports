#
# hacks.tcl
#
# This is a collection of TCL procs that various folks have added to exmh
# In most cases you want to bind these procs to buttons or keystrokes.
# See the man page about dinking with your own version of buttons.tcl
# or bindings.tcl
#

# Inc_Show is appropriate for the Inc button, but not for the
# background inc process.  It incs, changes to inbox, and shows
# the first unseen message in the folder.  (That may or may not
# be the first one just inc'ed.)

proc Inc_Show {} {
    Inc
    Folder_Change inbox
    Msg_ShowUnseen
}

# UserNextImpliedAction is a gross hack that depends on the implementation
# of the WidgetTextYview procedure.  It either pages the message display
# or changes to the next message.  The "implied" arg to Ftoc_Next really
# changes that to "Next or Prev - same as last time".

proc UserNextImpliedAction {} {
    global widgetText exwin
    set t $exwin(mtext)
    set bottom [lindex $widgetText(view,$t) 3]
    set end [expr [$t index end]-1]
    if {$bottom >= $end} {
	Ftoc_Next show implied
    } else {
	Widget_TextPageDown $t
    }
}

