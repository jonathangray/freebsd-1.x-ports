# 
# flag.tcl
#
# Manage the iconic flag feedback.  The flag module understands three states,
# but the icon only shows two states:
#
# State 0 - no unseen messages.
# State 1 - newly arrived unseen messages.
# State 2 - messages viewed while in State 1, but not necessarily all unseen
#	messages viewed yet.
#
# The mailbox flag goes up on the transition to State 1 (from either State 0
# or State 2), and the flag goes down on the transition to State 2.  So,
# it is possible to have the flag down and still have unseen messages.  The
# idea is that the flag means new mail has arrived since you last looked
# at *something*.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Flag_Init {} {
    global flag
    set flag(state) init
    FlagDown
}
proc Flag_NewMail { {folder {}} } {
    FlagUp
}
proc Flag_MsgSeen { {folder {}} } {
    FlagDown
}
proc Flag_NoUnseen {} {
    FlagDown
}

# Note - if you change the icon, there is some code in ExmhArgv
# that positions icons that can depend on the iconsize.
proc FlagUp {} {
    global exmh flag
    if {$flag(state) != "up"} {
	wm iconbitmap . @$exmh(library)/flagup.bitmap
	set flag(state) up
    }
}
proc FlagDown {} {
    global exmh flag
    if {$flag(state) != "down"} {
	wm iconbitmap . @$exmh(library)/flagdown.bitmap
	set flag(state) down
    }
}
