# bindings.tcl
#
# Keystroke bindings
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Bindings_Main { w } {
    # Keystroke bindings for operations on messages and folders
    Widget_ReadOnlyText $w
    bind $w <Any-Key> {if {"%A" != {}} {Exmh_Status "bad key %A"} }
    bind $w <Return> 	{Folder_Commit}
    bind $w <i> {Inc}
    bind $w <c> {Msg_Compose}
    bind $w <d> {Msg_Remove}
    bind $w <p> {Ftoc_Prev show}
    bind $w <P> {Ftoc_Prev noshow}
    bind $w <minus> {Ftoc_PrevMarked show}
    bind $w <n> {Ftoc_Next show}
    bind $w <N> {Ftoc_Next noshow}
    bind $w <m> {Msg_Move}
    bind $w <r> {Msg_Reply Mh_ReplySetup}
    bind $w <R> {Msg_Reply Mh_ReplyAllSetup}
    bind $w <f> {Msg_Forward}
    bind $w <s> {Msg_ShowCurrent}
    bind $w <u> {Ftoc_Unmark}
    bind $w <U> {Msg_ShowUnseen}
    bind $w <asciicircum> {Msg_ShowWhat first noshow}
    bind $w <dollar> {Msg_ShowWhat last noshow}
    bind $w <Control-s> {Find_It forw}
    bind $w <Control-r> {Find_It prev}
    # Page message
    bind $w <space>		{Widget_TextPageOrNext $exwin(mtext)}
    bind $w <BackSpace>		{Widget_TextPageUp $exwin(mtext)}
    bind $w <Next>		{Widget_TextPageDown $exwin(mtext)}
    bind $w <Down>		{Widget_TextPageDown $exwin(mtext)}
    bind $w <Prior>		{Widget_TextPageUp $exwin(mtext)}
    bind $w <Up>		{Widget_TextPageUp $exwin(mtext)}
    # Page Ftoc
    bind $w <Control-n>		{Widget_TextPageDown $exwin(ftext)}
    bind $w <Shift-Next>	{Widget_TextPageDown $exwin(ftext)}
    bind $w <Control-p>		{Widget_TextPageUp $exwin(ftext)}
    bind $w <Shift-Prior>	{Widget_TextPageUp $exwin(ftext)}
    #
    bind $w <Control-w>		\
	{catch {set sedit(killbuf) [$exwin(mtext) get sel.first sel.last]}}
    Select_Bindings $w
    if {[info command User_Bindings] != ""} {
	User_Bindings $w
    }
}

proc Bindings_Search { entry } {
    # Bindings for the search entry widget
    bind $entry <Return> { Find_It }
    bind $entry <Control-r> { Find_It prev }
    bind $entry <Control-s> { Find_It forw }
}

proc UserCommitAction { } {
    global bind
    if [info exists bind(commitAction)] {
	if [eval $bind(commitAction)] {
	    unset bind(commitAction)
	}
    }
}
