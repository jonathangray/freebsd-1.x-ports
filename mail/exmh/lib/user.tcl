# user.tcl
#
# User hooks.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc User_Init {} {
    # The main routine calls User_Init early on, after only
    # Mh_Init, Preferences_Init, and ExmhLogInit (for Exmh_Debug)

    if {0} {
	# Arrange to have some folders labels displayed as icons, not text
	global folderInfo
	set folderInfo(bitmap,exmh) @/tilde/welch/bitmaps/exmh
    }
    return
}

proc User_Layout {} {
    # This is called just after Exwin_Layout that creates the main 
    # widget tree.  Here you could wedge in more buttons, or override
    # some of their behavior.  Look at exwin.tcl and buttons.tcl to
    # find what elements of the exwin() array and the buttons() array
    # are used to store widget pathnames.
    if {0} {
	global buttons
	set incButton $buttons(folderF).inc
	$incButton configure -command UserInc
    }
}

proc UserInc {} {
    #
    # The default Inc procedure only calls Inc_Inbox.
    # The following configuration does an Flist call that
    # hunts around for new messages in all folders, which
    # is useful if an external agent is delivering mail to
    # some folders (like newsgroups)
    #
    Inc_Inbox
    Flist_FindUnseen
}

proc User_Bindings { w } {
    #
    # This is called from Bindings_Main to bind acceleration keystrokes
    # to the message display and folder table-of-contents windows
    # w is a text widget
}

