#
# exmh_editor.tcl --
#	Editor interactions
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

# Message composing and editor interaction

proc Edit_Init {} {
    global mhProfile
    case $mhProfile(editor) {
	{*mxedit* sedit} {set prog $mhProfile(editor)}
	default {set prog sedit}
    }
    Preferences_Add "Editor Support" \
"Exmh has a built-in editor for composing messages, named \"sedit\".  It can also interface to other editors.  For non-TK editors like vi or emacs, preface the editor command with \"exmh-async\" so the editor runs as a detached process. Example commands include:
sedit
mxedit
exmh-async emacs
exmh-async emacsclient
exmh-async xterm -e vi" \
    [list \
	[list editor(prog) editCmd $prog {Editor command} \
"The editor command used to compose mail messages.
The filename will be appended to the command."] \
    ]

    # Setup draft folder hook
    global folderHook
    set folderHook(enter,$mhProfile(draft-folder)) EditDraftFolderHook
    set folderHook(leave,$mhProfile(draft-folder)) EditDraftFolderCleanup
}

proc Edit_Draft {} {
    global mhProfile exmh
    # Run the editor on the current draft
    if {$exmh(folder) == $mhProfile(draft-folder)} {
	Msg_CheckPoint	;# Update cur message
    }
    set draftID [Mh_Cur $mhProfile(draft-folder)]
    if {$draftID == {}} {
	Exmh_Status "No current message in Draft-Folder $mhProfile(draft-folder)" error
	return
    }
    EditWhatNow $draftID
}

proc EditDraftFolderHook { f what } {
    # Called when we change to the drafts folder
    Buttons_DraftMode 1
}
proc EditDraftFolderCleanup { f what } {
    Buttons_DraftMode 0
}

# Throw up a dialog asking whether user wants to
# send, re-edit, save as draft, abort.
# As with the command line whatnowproc, this also starts up the
# editor on the draft.

proc EditWhatNow {draftID} {
    global mhProfile

    Exmh_Debug EditWhatNow $draftID
    if [EditStart [Mh_Path $mhProfile(draft-folder) $draftID]] {
	# Editor has run synchronously, so we go right into the dialog
	EditDialog $draftID
    }
}

# The following can be invoked by remote editor interpreters

proc EditDialog {draftID} {
    global exmh mhProfile
    Exmh_Debug EditDialog $draftID
    set exmh(curDraft) $draftID
    EditShowDialog [file tail $draftID] "What should I do with draft $draftID?"
}

# For compatibility with old versions of mxedit
proc WhatNowDialog {args} {
    global exmh
    EditShowDialog mxedit $args
}
proc EditShowDialog {id msg} {
    global exwin
    # Create the buttons and arrange them from left to right in the RH
    # frame. Embed the left button in an additional sunken frame to indicate
    # that it is the default button.

    if [Exwin_Toplevel .edit$id "What Now?" WhatNow nomenu] {
	set d .edit$id
	wm transient $d
	$d config -relief raised -borderwidth 2
	Widget_AddBut $d b1 "Kill" "EditCurDone abort" {right padx 15}
	Widget_AddBut $d b2 "Dismiss" "EditCurDone dismiss"
	Widget_AddBut $d b3 "Whom" "EditCurDone whom nohide"
	Widget_AddBut $d b4 "Re-edit" "EditCurDone reedit"
     
	frame $d.b5 -relief sunken -border 1
	pack append $d $d.b5 {right padx 6 }
    
	button $d.b5.button -text "Send" -command "EditCurDone send"
	pack append $d.b5 $d.b5.button {expand padx 6}
	Widget_Message $d msg -text $msg -aspect 300
    } else {
        catch {destroy .edit$id.f}
    }
}
proc EditDialogMsg {id msg} {
    global tk_version
    set d .edit$id
    catch {destroy $d.f}
    Widget_Frame $d f
    if {$tk_version >= 3.3} {
	pack $d.f -before [lindex [pack slaves $d] 0] -side bottom
    } else {
	pack before [lindex [pack info $d] 0] $d.f {bottom}
    }
    set lines [llength [split $msg \n]]
    Widget_Text $d.f [expr {$lines > 8 ? 8 : $lines}]
    $d.f.t insert 1.0 $msg
    $d.f.t config -state disabled -width 40
}


proc EditHideDialog {id} {
    # Clear the dialog box for future use
    Exwin_Dismiss .edit$id nosize
}

proc EditStart { draft } {
    # Start the editor, reusing an existing session if possible
    global editor exmh
    case $editor(prog) {
	{ *mxedit* } {
	    if ![info exists exmh(editInterp)] {
		set exmh(editInterp) "mxedit"
	    }
	    if [catch {send "$exmh(editInterp) $draft" mxReset}] {
		if [catch {send $exmh(editInterp) {set mxVersion}}] {
		    Exmh_Status "Starting mxedit..." warn
		    # Start the editor and tell it to make a callback
		    # that identifies the TCL interpreter in the editor
		    exec $editor(prog) -globalCmd [list mxSendInterpName [winfo name .] Edit_Ident] $draft &
		} else {
		    Exmh_Status "Opening mxedit..." warn
		    catch {send $exmh(editInterp) [list mxOpen $draft]}
		}
	    } else {
		Exmh_Status "Reopening mxedit..." warn
		catch {send "$exmh(editInterp) $draft" {wm deiconify .}}
	    }
	    return 0		;# Asynchronous edit
	}
	{ sedit } {
	    Sedit_Start $draft
	    return 0		;# Asynchronous edit
	}
	{ *exmh-async* } {
	    Exmh_Status "Starting ASYNC $editor(prog) ..." warn
	    eval exec exmh-async \"[winfo name .]\" \
		[lrange $editor(prog) 1 end] $draft &
	    return 0		;# Asynchronous edit
	}
	default {
	    Exmh_Status "Starting $editor(prog) ..." warn
	    if [catch {eval exec $editor(prog) $draft} err] {
		Exmh_Status $err error
	    }
	    return 1		;# Synchronous edit
	}
    }
}

# The following is invoked via "send" by mxedit when it
# starts up the first time in order to identify itself to us.

proc Edit_Ident { interpName } {
    global exmh
    set exmh(editInterp) $interpName
}
proc EditCurDone {act {hide hide}} {
    global exmh
    if [string match hide $hide] {
	after 10 [list EditHideDialog $exmh(curDraft)]
    }
    Edit_Done $act $exmh(curDraft)
}

# The following is invoked by remote editor interpreters
proc EditDone {act msg} {
    Edit_Done $act $msg
}
proc Edit_Done {act msg} {
    # Commit or abort an edit session
    global mhProfile exmh env
    Exmh_Debug action = $act msg = $msg
    case $act in {
	send	{
	    set anno [Mh_AnnoEnviron $msg]
	    Exmh_Debug Edit_Done send: anno=$anno
	    Mh_Send $msg
	    if {$exmh(folder) == $mhProfile(draft-folder)} {
		# Clean up scan listing
		if [catch {Msg_RemoveById $msg} err] {
		    Exmh_Debug Msg_RemoveById $msg $err
		}
	    }
	    Exmh_Status "Draft $msg sent" normal
	    Mh_AtLinkCleanup
	    if {$anno} {
		if {[string compare $exmh($msg,folder) $exmh(folder)] == 0} {
		    set ix [Ftoc_FindMsg $exmh($msg,mhmessages)]
		    Exmh_Debug Edit_Done ix=$ix
		    if {$ix != {}} {
			Ftoc_RescanLine $ix dash
		    }
		}
	    }
	}
	reedit	{
	    Exmh_Status " "
	    EditWhatNow $msg
	}
	abort	{
	    catch {Mh_Rmm $mhProfile(draft-folder) $msg}
	    if {$exmh(folder) == $mhProfile(draft-folder)} {
		# Clean up scan listing
		if [catch {Msg_RemoveById $msg} err] {
		    Exmh_Debug Msg_RemoveById $msg $err
		}
	    }
	    Exmh_Status "Draft $msg aborted" error
	    Mh_AtLinkCleanup
	}
	dismiss	{
	    Exmh_Status "Draft $msg dismissed" normal
	    Mh_AtLinkCleanup
	}
	whom	{
	    catch {Mh_Whom $msg} result
	    EditDialogMsg $msg $result
	}
	default	{
	    Exmh_Error "Unknown action in Edit_Done"
	}
    }
    Mh_AnnoCleanup $msg
}
