# msg.tcl
#
# Operations on messages
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Msg_Init {} {
    Preferences_Resource msg(tagnames) m_tagnames general
    Preferences_Resource msg(tag,general) m_general {-relief flat}
    Msg_Reset 0
}

proc Msg_Reset { numMsgs {folder {}} } {
    # Reset state after scanning a new folder
    global msg
    set msg(seen) {}			;# list of seen messages
    set msg(seenOld) {}			;# seen, then deleted or moved
    set msg(dpy)  {}			;# Currently displayed message
    set msg(id) [Mh_Cur $folder]	;# pick current message
    Buttons_Current [expr {$msg(id) != {}}]	;# Enable/disable buttons
    Ftoc_Reset $numMsgs $msg(id) $folder	;# Reset display
}
proc Msg_CheckPoint {} {
    # push current MH state to disk
    global exmh msg
    if {$msg(id) != {}} {
	Mh_SetCur $exmh(folder) $msg(id)
    }
    if {$msg(seen) != {}} {
	Mh_MarkSeen $exmh(folder) $msg(seen)
	set msg(seen) {}
    }
    set msg(seenOld) {}
}
proc Msg_Pick { line {show show} } {
    # Select a message
    global exwin msg
    Exmh_Debug Msg_Pick line=$line
    set msgNum [Ftoc_MsgNumber $line]
    if {$msgNum != {} && $msgNum != 0} {
	Ftoc_RangeUnHighlight
	Msg_Change $msgNum $show $line
    } else {
	Msg_ClearCurrent
    }
}

proc Msg_ShowCurrent { {show show} } {
    global msg
    if {$msg(id) != {}} {
	Msg_Change $msg(id) $show
	return 1
    } else {
	Msg_ClearCurrent
	return 0
    }
}
proc Msg_ShowUnseen { {show show} } {
    global exmh
    set unseen [Flist_UnseenMsgs $exmh(folder)]
    if {[llength $unseen] > 0} then {
	Msg_Change [lindex $unseen 0] show
	return 1
    } else {
	Msg_ClearCurrent
	return 0
    }
}
proc Msg_ClearCurrent { } {
    global msg exmh
    set msg(id) {}		;# Clear current message
    set msg(dpy) {}		;# and currently displayed message
    Mh_ClearCur $exmh(folder)
    MsgClear
    Buttons_Current 0
}
proc MsgClear {} {
    global exwin
    Label_Message ""
    $exwin(mtext) configure -state normal
    $exwin(mtext) delete 0.0 end
    $exwin(mtext) configure -state disabled
    Face_Delete
}
proc Msg_ShowSomething {} {
    global exmh msg
    set order {unseen cur}
    foreach pick $order {
	if {[catch {exec pick +$exmh(folder) $pick} tmp] == 0} then {
	    Msg_Change [lindex $tmp 0] show
	    return
	}
    }
    # Hack
    global ftoc
    Msg_Pick $ftoc(numMsgs) show
}
proc Msg_ShowWhat { {what last} {show show} } {
    if {$what == {}} {
	Msg_ClearCurrent
	return 0
    } else {
	Msg_Change {} $show $what
	return 1
    }
}

proc Msg_Change {msgid {show show} {line {}} } {
    Exmh_Debug Msg_Change id=$msgid line=$line
    Exmh_Debug Msg_Change [time [list MsgChange $msgid $show $line]]
}
proc MsgChange {msgid {show show} {line {}} } {
    global exmh exwin msg

    if {$msgid != {}} {
	# Allow null msgid from Msg_ShowWhat, which supplies line instead
	if {$msgid < 0}  return
    } else {
	set msgid [Ftoc_MsgNumber [Ftoc_FindMsg $msgid $line]]
    }
    Ftoc_ClearCurrent
    if {! [Ftoc_Change $msgid $line $show]} {
	Exmh_Status "Cannot find msg $msgid - Rescan?"
    } else {
	if {$msg(id) == {}} {
	    Buttons_Current 1
	}
	set msg(id) $msgid
	if {$show == "show"} {
	    MsgShow $msgid
	} else {
	    MsgClear
	}
	if {$line != {}} {
	    Ftoc_MoveFeedback $msgid $line
	}
    }
}

proc MsgSeen { msgid } {
    # Suppress duplicates or else mark does the wrong thing.
    global msg
    if {[lsearch $msg(seen) $msgid] < 0} {
	lappend msg(seen) $msgid
    }
    Flag_MsgSeen
    Flist_MsgSeen $msgid
}
proc Msg_UnSeen { msgid } {
    # We nuke deleted and moved messages from the seen list because
    # marking them confuses MH.  However, we still need to remember
    # them to properly maintain our unseen state in the presense of
    # background Flist_FindUnseen calls.  Hence msg(seenOld)
    global msg
    set ix [lsearch $msg(seen) $msgid]
    if {$ix >= 0} {
	set msg(seen) [lreplace $msg(seen) $ix $ix]
	if {[lsearch $msg(seenOld) $msgid] < 0} {
	    lappend msg(seenOld) $msgid
	}
    }
}
proc Msg_Seen {} {
    global msg
    return [concat $msg(seenOld) $msg(seen)]
}


# Message operations.
# The ones that generate messages take a callback that sets up
# the draft folder so you can override this initial setup.

proc Msg_Compose { {compProc Mh_CompSetup} } {
    if [catch {$compProc} err] {			;# Setup draft msg
	Exmh_Status "${compProc}: $err" purple
    } else {
	Edit_Draft					;# Run the editor
    }
}

proc Msg_Reply { {replyProc Mh_ReplySetup} } {
    global exmh msg

    if [MsgOk $msg(id) m] {
	Mh_AtLink $exmh(folder) $m			;# Symlink to @
	if [catch {$replyProc $exmh(folder) $m} err] {	;# Setup draft msg
	    Exmh_Status "${replyProc}: $err" purple
	    Mh_AtLinkCleanup				;# Nuke @ link
	} else {
	    Edit_Draft					;# Run the editor
	}
    }
}

proc Msg_Forward { {forwProc Mh_ForwSetup} } {
    global exmh msg

    set ids {}
    Ftoc_Iterate line {
	set msgid [Ftoc_MsgNumber $line]
	if {$msgid != {}} {
	    lappend ids $msgid
	}
    }
    if {[llength $ids] > 0} {
	if [catch {$forwProc $exmh(folder) $ids} err] {	;# Setup draft msg
	    Exmh_Status "${forwProc}: $err" purple
	    Mh_AtLinkCleanup				;# Nuke @ link
	} else {
	    Edit_Draft					;# Run the editor
	}
    }
}

proc Msg_Dist { {distProc Mh_DistSetup} } {
    global exmh msg

    if [MsgOk $msg(id) m] {
      if [catch {$distProc $exmh(folder) $m} err] {   ;# Setup draft msg
          Exmh_Status "${distProc}: $err" purple
          Mh_AtLinkCleanup                            ;# Nuke @ link
      } else {
          Edit_Draft                                  ;# Run the editor
      }
    }
}
 
proc MsgOk { number msgvar } {
    upvar $msgvar msg
    if {$number != ""} {
	set msg $number
	return 1
    } else {
	Exmh_Status "No valid message number" red
	return 0
    }
}



proc Msg_Remove { {rmProc Ftoc_RemoveMark} } {
    Exmh_Debug Msg_Remove $rmProc
    Ftoc_Iterate line {
	set msgid [Ftoc_MsgNumber $line]
	Exmh_Debug Msg_Remove l=$line m=$msgid
	$rmProc $line $msgid
	# to avoid MH mark bug
#	MsgUnSeen $msgid
    }
    if {[Ftoc_PickSize] == 1} {
	Ftoc_Next show soft
    }
}
proc Msg_RemoveById { msgid {rmProc Ftoc_Delete} } {
    global msg
    set line [Ftoc_FindMsg $msgid]
    $rmProc $line $msgid
    Msg_UnSeen $msgid
    if {$msg(id) == $msgid} {
	Msg_ClearCurrent
    }
}
proc Msg_Move { {moveProc Ftoc_MoveMark} } {
    global exmh

    if {$exmh(target) == ""} {
	Exmh_Status "Right click on folder label to pick destination" purple
	return
    }
    if { $exmh(target) != $exmh(folder)} then {
	Exmh_Debug Msg_Move picksize=[Ftoc_PickSize]
	Ftoc_Iterate line {
	    set msgid [Ftoc_MsgNumber $line]
	    Exmh_Debug $moveProc l=$line id=$msgid
	    $moveProc $line $msgid
	    # To avoid MH mark bug
#	    MsgUnSeen $msgid
	}
	if {[Ftoc_PickSize] == 1} {
	    Ftoc_Next show soft
	}
    } else {
	Exmh_Status "Move requires target folder != current folder"
    }
}

proc Msg_Clip {} {
    # "Tear off" a message into a top-level text widget
    global mhProfile exmh msg exwin
    if {$msg(id) == {}} {
	Exmh_Status "Select a message to clip first" red
	return
    }
    if ![info exists msg(tearid)] {
	set msg(tearid) 0
    } else {
	incr msg(tearid)
    }
    set self [Widget_Toplevel .tear$msg(tearid) "$exmh(folder) $msg(id)" Clip]

    Widget_Frame $self but Menubar {top fill}
    Widget_AddBut $self.but quit "Dismiss" [list destroy $self]
    Widget_Label $self.but label {left fill} -text $exmh(folder)/$msg(id)
    set t [Widget_Text $self $exwin(mtextLines) -cursor xterm -setgrid true]
    Msg_Setup $t
    MsgShowInText $t $mhProfile(path)/$exmh(folder)/$msg(id)
}
proc Msg_FindMatch {L string} {
    global exwin
    return [FindTextMatch $exwin(mtext) $L $string]
}
proc Msg_BurstDigest {} {
    global msg exmh mhProfile

    if {$msg(id) == {}} {
	Exmh_Status "No message selected to burst" purple
	return
    }
    Exmh_Status "Bursting message $msg(id) in $exmh(folder)..." blue

    # burst the digest; catch the output
    Msg_CheckPoint ;# clear cached seen/unseen state
    if [catch { exec burst -verbose $msg(id) +$exmh(folder)} out] {
	Exmh_Status "Error bursting digest: $out"
    } else {
	# burst OK, split up the output
	set allids {}
	foreach line [ split $out \n] {
	    #extract the new message number and save in $allids
	    if [regexp {of digest .* becomes message ([0-9]+)} $line match msgid] {
		lappend allids $msgid
	    }
	}
	# mark new messages as unread
	Exmh_Debug burst created msgs $allids
	if {$allids != {}} {
	    eval { exec mark +$exmh(folder) -sequence $mhProfile(unseen-sequence) } $allids
	}
	# rescan to pick them up
	Scan_Folder $exmh(folder)
	Msg_ClearCurrent
	Exmh_Status "Bursting message $msg(id) in $exmh(folder)...done" blue
    }
}
proc Msg_Save {} {
    global exmh mhProfile
    set files {}
    Ftoc_Iterate line {
	set msgid [Ftoc_MsgNumber $line]
	lappend files $mhProfile(path)/$exmh(folder)/$msgid
    }

    set name [FSBox "Select file to create/append to:" ]
    if {$name != {}} {
	set exists [file exists $name]
	if [catch {eval {exec cat} $files {>> $name}} err] {
	    Exmh_Status $err error
	} else {
	    set plural [expr {([llength $files] > 1) ? "s" : ""}]
	    if {$exists} {
		Exmh_Status "Message$plural appended to $name"
	    } else {
		Exmh_Status "Message$plural stored in $name"
	    }
	}
    } else {
	Exmh_Status "canceled"
    }
}


