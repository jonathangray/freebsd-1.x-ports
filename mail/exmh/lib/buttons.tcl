# buttons.tcl
#
# Action buttons for EXMH.  These are divided into three sets:
# Main - global things like Help and Quit
# Folder - operations on folders like Pack, or Inc.
# Message - operations on the current message.
#
# Support routines for buttons (and menus) in exmh.  The main abstraction
# is the notion of sets of buttons that are enabled and disabled to
# reflect different modes in exmh.  For example, some buttons (and menu
# entries) are disabled when there is no current message.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Buttons_Init {} {
    global buttons
    set buttons(draftMode) 0
    set buttons(group,current) {}
    set buttons(group,nodraft) {}
    set buttons(group,range) {}
    set buttons(group,comp) {}
    set buttons(groupMenu,current) {}
    set buttons(groupMenu,nodraft) {}
    set buttons(groupMenu,range) {}
    set buttons(groupMenu,comp) {}
}
proc Buttons_Group { frame name buts } {
    global buttons
    foreach but $buts {
	lappend buttons(group,$name) $frame.$but
    }
    if {$name == "comp"} {
	# TODO - eliminate this comp special case and add a comp -use button
	set w [lindex $buts 0]
	if {$w != {}} {
	    set buttons(comp) $frame.$w
	}
    }
}

proc Buttons_GroupMenu { menub name labels } {
    global buttons
    foreach l $labels {
	lappend buttons(groupMenu,$name) [list $menub $l]
    }
}

proc ButtonsGroupState { group state } {
    global buttons
    foreach button $buttons(group,$group) {
	$button configure -state $state
    }
    foreach item $buttons(groupMenu,$group) {
	set menub [lindex $item 0]
	set label [lindex $item 1]
	$menub.m entryconfigure $label -state $state
    }
}

proc Buttons_Current { curMsg } {
    # if curMsg is false, then disable inappropriate buttons
    # otherwise, reenable them.
    # This gets called before Buttons_DraftMode when entering
    # the drafts folder (i.e., buttons(draftMode) may be wrong)
    global buttons
    set buttons(curMsg) $curMsg
    if {$curMsg} {
	ButtonsGroupState current normal
	if $buttons(draftMode) {
	    ButtonsGroupState nodraft disabled
	    ButtonsGroupState comp normal
	}
    } else {
	ButtonsGroupState current disabled
	if $buttons(draftMode) {
	    ButtonsGroupState comp disabled
	}
    }
}

proc Buttons_DraftMode { inDraftMode } {
    # This procedure is called when entering the drafts folder
    # in order to dink the buttons so you can edit and send
    # a message in the drafts folder.  The inDraftMode
    # parameter is true when entering the drafts folder,
    # and it is false when leaving it.
    global buttons
    set buttons(draftMode) $inDraftMode
    if {$inDraftMode} {
	# Disable inappropriate buttons
	ButtonsGroupState nodraft disabled
	ButtonsGroupState comp normal
	# Override the Send button
	if [info exists buttons(comp)] {
	    set buttons(comp,label) [lindex [$buttons(comp) configure -text] 4]
	    set buttons(comp,cmd) [lindex [$buttons(comp) configure -command] 4]
	    $buttons(comp) configure -text EDIT -command Edit_Draft
	    if {! $buttons(curMsg)} {
		$buttons(comp) configure -state disabled
	    }
	}
    } else {
	# Reenable buttons
	ButtonsGroupState nodraft normal
	ButtonsGroupState comp normal
	# Restore Send button
	if {[info exists buttons(comp,cmd)] && \
	    [info exists buttons(comp,label)]} { 
	    $buttons(comp) configure -command $buttons(comp,cmd) \
		    -text $buttons(comp,label) -state normal
	    unset buttons(comp,cmd)
	    unset buttons(comp,label)
	}
    }
    return
}

proc Buttons_Range {} {
    ButtonsGroupState range normal
}

#####################################################################

proc Buttons_Main { frame } {
    # Note that the unused space in $frame is used
    # by Exmh_MainLabel to hold the version string
    global buttons
    set buttons(mainF) $frame

    foreach b [concat [option get $frame buttonlist {}] \
		      [option get $frame ubuttonlist {}]] {
	Widget_AddButDef $frame $b
    }
}

proc Buttons_Folder { frame } {
    # Create the buttons for operations on items in MH folders
    # Note that the unsed space in $frame is used by
    # Folder_Label to display the status of the current folder.
    global buttons inc
    set buttons(folderF) $frame

    # Menu for extra stuff
    foreach M [concat [option get $frame menulist {}] \
		      [option get $frame umenulist {}]] {
	ButtonMenuInner $frame $M
    }

    foreach b [concat [option get $frame buttonlist {}] \
		      [option get $frame ubuttonlist {}]] {
	if {$inc(style) == "none" && $b == "inc"} continue
	Widget_AddButDef $frame $b
    }
}

proc Buttons_Message { frame } {
    global buttons
    set buttons(msgF) $frame

    # Menu for extra stuff
    # Loop through system and user-defined menus
    foreach M [concat [option get $frame menulist {}] \
		      [option get $frame umenulist {}]] {
	set menub [ButtonMenuInner $frame $M]

	# but only deal with system-defined groups
	foreach g [option get $frame groups {}] {
	    Buttons_GroupMenu $menub $g [option get $frame gm_$g {}]
	    Buttons_GroupMenu $menub $g [option get $frame ugm_$g {}]
	}
    }

    foreach b [concat [option get $frame buttonlist {}] \
		      [option get $frame ubuttonlist {}]] {
	Widget_AddButDef $frame $b
    }

    # The group assignments associate buttons with states.

    foreach g [option get $frame groups {}] {
	Buttons_Group $frame $g [option get $frame g_$g {}]
	Buttons_Group $frame $g [option get $frame ug_$g {}]
    }
}
proc ButtonMenuInner { frame M } {
    set menub [Widget_AddMenuBDef $frame $M {right padx 1}]
    foreach e [concat [option get $menub entrylist {}] \
		      [option get $menub uentrylist {}]] {
	set l [option get $menub l_$e {}]
	set c [option get $menub c_$e {}]
	set v [option get $menub v_$e {}]
	case [option get $menub t_$e {}] {
	    default {Widget_AddMenuItem $menub $l $c}
	    check   {Widget_CheckMenuItem $menub $l $c $v}
	    radio   {Widget_RadioMenuItem $menub $l $c $v}
	    separator {Widget_AddMenuSeparator $menub}
	}
    }
    return $menub
}
