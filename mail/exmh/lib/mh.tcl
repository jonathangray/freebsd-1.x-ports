#
# mh.tcl --
#	MH support. This is divided into two parts:
#		Thin layers on the MH commands
#		Parsing and setting up the mhProfile
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Mh_Init {} {
    MhParseProfile
    MhSetMailDrops
}

# The following are default comp, repl, and forw setup procedures
# passed to Msg_Comp, Msg_Reply, and Msg_Forward, respectively.
proc Mh_CompSetup {} {
    Exmh_Status "comp"
    exec comp -nowhatnowproc
}
proc Mh_ReplySetup { folder msg } {
    Exmh_Status "repl +$folder $msg"
    exec repl +$folder $msg -nowhatnowproc -nocc cc -nocc to
    MhAnnoSetup $folder $msg repl
}
proc Mh_ReplyAllSetup { folder msg } {
    Exmh_Status "repl +$folder $msg"
    exec repl +$folder $msg -nowhatnowproc -cc cc -cc to
    MhAnnoSetup $folder $msg repl
}
proc Mh_ForwSetup { folder msgs } {
    Exmh_Status "forw +$folder $msgs"
    eval {exec forw +$folder} $msgs -nowhatnowproc
    MhAnnoSetup $folder $msgs forw
}
proc Mh_DistSetup { folder msg } {
    global exmh mhProfile
    Exmh_Status "dist +$folder $msg"
    exec dist +$folder $msg -nowhatnowproc
    MhAnnoSetup $folder $msg dist
}
proc MhAnnoSetup { folder msg key } {
    global mhProfile exmh
    set draftID [Mh_Cur $mhProfile(draft-folder)]
    set exmh($draftID,mhaltmsg) $mhProfile(path)/$folder/$msg
    set exmh($draftID,mhfolder) $mhProfile(path)/$folder
    set exmh($draftID,folder) $folder
    set exmh($draftID,mhmessages) $msg
    set exmh($draftID,mhanno$key) $exmh(anno,$key)
}
proc Mh_AnnoEnviron { draftID } {
    global exmh env
    if ![info exists exmh($draftID,mhaltmsg)] {
	return 0
    }
    set env(mhaltmsg) $exmh($draftID,mhaltmsg)
    set env(mhfolder) $exmh($draftID,mhfolder)
    set env(mhmessages) $exmh($draftID,mhmessages)
    if [info exists exmh($draftID,mhannodist)] {
	set env(mhdist) 1
	set env(mhannodist) 1
	set env(mhannotate) "Resent"
	return $exmh($draftID,mhannodist)
    }
    if [info exists exmh($draftID,mhannorepl)] {
	set env(mhdist) 0
	set env(mhannorepl) 1
	set env(mhannotate) "Replied"
	return $exmh($draftID,mhannorepl)
    }
    if [info exists exmh($draftID,mhannoforw)] {
	set env(mhdist) 0
	set env(mhannoforw) 1
	set env(mhannotate) "Forwarded"
	return $exmh($draftID,mhannoforw)
    }
}
proc Mh_AnnoCleanup { draftID } {
    global exmh env
    foreach key {mhannoforw mhannorepl mhannodist mhannotate mhdist \
		 mhaltmsg mhfolder mhmessages folder} {
	catch {unset exmh($draftID,$key)}
	catch {unset env($key)}
    }
}

proc Mh_Folder { f } {
    if [catch {exec folder +$f < /dev/null} err] {
	Exmh_Debug $err
    }
}
proc Mh_MsgChk {} {
    catch {exec msgchk -nodate -notify mail} result
    return $result
}
proc Mh_MsgCount { spool } {
    return [exec egrep "^From " $spool | wc -l]
}
proc Mh_CurSafe { folder } {
    exec folder +$folder -push < /dev/null
    if [catch {exec pick +$folder -list cur} cur] {
	set cur {}
    }
    exec folder -pop < /dev/null
    return $cur
}
proc Mh_Unseen { folder } {
    global mhProfile
    if [catch {exec pick +$folder -list $mhProfile(unseen-sequence)} unseen] {
	return {}
    } else {
	return [split $unseen \n]
    }
}
proc Mh_MarkSeen { folder ids } {
    global mhProfile
    if [catch {
	eval {exec mark +$folder -seq $mhProfile(unseen-sequence) -delete} $ids
    } err] {
	Exmh_Debug Mh_MarkSeen $err
    }
}
proc Mh_SetCur { f msgid } {
    global mhPriv
    if {$mhPriv(cur,$f) == $msgid} {
	return
    }
    Exmh_Debug Mh_SetCur +$f cur $msgid
    catch {exec mark +$f $msgid -seq cur}
    set mhPriv(cur,$f) $msgid
}
proc Mh_Cur { f } {
    global mhPriv
    if [catch {MhCur $f} cur] {
	set cur [Mh_CurSafe $f]
    }
    set mhPriv(cur,$f) $cur
    return $mhPriv(cur,$f)
}
proc MhCur { f } {
    # pick +folder cur changes the context, so we access the files directly
    global mhProfile mhPriv
    if {$f == {}} {
	return {}
    }
    if {[catch {open $mhProfile(path)/$f/$mhProfile(mh-sequences) r} in] == 0} {
	set old [read $in]
	close $in
	foreach line [split $old \n] {
	    if [regexp cur: $line] {
		set cur [lindex $line 1]
		if [file exists $mhProfile(path)/$f/$cur] {
		    return $cur
		} else {
		    return {}
		}
	    }
	}
    }
    # private sequences
    if {[catch {open $mhProfile(path)/$mhProfile(context) r} in] == 0} {
	set old [read $in]
	close $in
	foreach line [split $old \n] {
	    if [regexp atr-cur-$mhProfile(path)/$f: $line] {
		set cur [lindex $line 1]
		if [file exists $mhProfile(path)/$f/$cur] {
		    return $cur
		} else {
		    return {}
		}
	    }
	}
    }
    return {}
}
proc Mh_ClearCur { f } {
    # mark will not clear the current sequence - have to muck with context
    global mhProfile
    Exmh_Debug Mh_ClearCur $f
    if {[catch {open $mhProfile(path)/$f/$mhProfile(mh-sequences) r} in] == 0} {
	set old [read $in]
	close $in
	# Don't fiddle if there isn't a cur message anyway - keep .xmhcache
	set done 1
	foreach line [split $old \n] {
	    if [regexp cur: $line] {
		set done 0
	    }
	}
	if {$done} {
	    return
	}
	if {[catch {open $mhProfile(path)/$f/$mhProfile(mh-sequences).new w} out] == 0} {
	    foreach line [split $old \n] {
		if [regexp cur: $line] {
		    continue
		} else {
		    if {$line != {}} {
			puts $out $line
		    }
		}
	    }
	    close $out
	    exec mv $mhProfile(path)/$f/$mhProfile(mh-sequences).new $mhProfile(path)/$f/$mhProfile(mh-sequences)
	    return
	}
    }
    # private sequences
    if {[catch {open $mhProfile(path)/$mhProfile(context) r} in] == 0} {
	if {[catch {open $mhProfile(path)/$mhProfile(context).new w} out] == 0} {
	    while {[gets $in line] >= 0} {
		if [string match atr-cur-$mhProfile(path)/$f:* $line] {
		    continue
		} else {
		    if {$line != {}} {
			puts $out $line
		    }
		}
	    }
	    close $in
	    close $out
	    exec mv $mhProfile(path)/$mhProfile(context).new $mhProfile(path)/$mhProfile(context)
	    return
	}
	close $in
    }
}

proc Mh_AtLink { folder msg } {
    global mhProfile
    if [file isdirectory "@"] {
	return	;# Don't pollute user's @ directory
    }
    catch {exec rm -f "@"}
    if [catch {exec ln -s $mhProfile(path)/$folder/$msg "@"} err] {
	Exmh_Debug "Mh_AtLink: $err"
    }
}
proc Mh_AtLinkCleanup {} {
    catch {exec rm -f "@"}
}

proc Mh_Path { folder msg } {
    global mhProfile
    if [file exists $mhProfile(path)/$folder/$msg] {
	return $mhProfile(path)/$folder/$msg
    } else {
	return [exec mhpath +$folder $msg]
    }
}
proc Mh_Refile {srcFolder msgs folder} {
    while {[llength $msgs] > 0} {
	set chunk [lrange $msgs 0 19]
	set msgs [lrange $msgs 20 end]
	Exmh_Debug exec refile $chunk -src +$srcFolder +$folder
	eval {exec refile} $chunk {-src +$srcFolder +$folder}
    }
}
proc Mh_Rmm { folder msgs } {
    while {[llength $msgs] > 0} {
	set chunk [lrange $msgs 0 19]
	set msgs [lrange $msgs 20 end]
	Exmh_Debug exec rmm +$folder $chunk
	eval {exec rmm +$folder} $chunk
    }
}
proc Mh_Send { msg } {
    global mhProfile
    exec send -draftf +$mhProfile(draft-folder) -draftm $msg -push -forward
}
proc Mh_Whom { msg } {
    global mhProfile
    exec whom -draftf +$mhProfile(draft-folder) -draftm $msg
}

proc Mh_Sort { f } {
    if [catch {exec sortm +$f} err] {
	Exmh_Status $err
    }
}
proc Mh_Pack { f } {
    if [catch {exec folder +$f -pack} err] {
	Exmh_Status $err
    }
}

proc MhParseProfile {} {
    global mhProfile env
    if [catch {open $env(HOME)/.mh_profile "r"} input] {
	if [info exists mhProfile(FAIL)] {
	    puts stderr "Cannot open $env(HOME)/.mh_profile: $input"
	    exit 1
	} else {
	    set mhProfile(FAIL) 1
	    MhSetupNewUser
	    MhParseProfile
	    unset mhProfile(FAIL)
	    return
	}
    }
    while {![eof $input]} {
	set numBytes [gets $input line]
	if {$numBytes > 0} {
	    set parts [split $line :]
	    set key [string tolower [lindex $parts 0]]
	    set other [lindex $parts 1]
	    set value [string trim $other]
	    set mhProfile($key) $value
	}
    }
    if ![info exists mhProfile(path)] {
	if [info exists mhProfile(FAIL)] {
	    puts stderr "No Path entry in your .mh_profile file."
	    puts stderr "Run the \"inc\" command to get your"
	    puts stderr "MH environment initialized right."
	    exit 1
	} else {
	    set mhProfile(FAIL) 1
	    MhSetupNewUser
	    MhParseProfile
	    unset mhProfile(FAIL)
	    return
	}
    } else {
	if {[string index $mhProfile(path) 0] != "/"} {
	    set mhProfile(path) [glob ~/$mhProfile(path)]
	}
    }
    if ![info exists mhProfile(context)] {
	set mhProfile(context) context
    }
    if ![info exists mhProfile(mh-sequences)] {
	set mhProfile(mh-sequences) .mh_sequences
    }
    if {$mhProfile(mh-sequences) == {}} {
	set mhProfile(mh-sequences) .mh_sequences
    }
    if ![info exists mhProfile(editor)] {
	if [info exists env(EDITOR)] {
	    set mhProfile(editor) $env(EDITOR)
	} else {
	    set mhProfile(editor) sedit
	}
    }
    if ![info exists mhProfile(draft-folder)] {
	MhSetupDraftFolder
    } else {
	set mhProfile(draft-folder) [string trim $mhProfile(draft-folder) +]
	if ![file isdirectory $mhProfile(path)/$mhProfile(draft-folder)] {
	    Exmh_Status "Creating drafts folder"
	    if [catch {exec mkdir $mhProfile(path)/$mhProfile(draft-folder)} msg] {
		catch {
		    puts stderr "Cannot create drafts folder $mhProfile(path)/$mhProfile(draft-folder)"
		}
	    }
	}
    }
    if ![info exists mhProfile(unseen-sequence)] {
	MhSetupUnseenSequence
    }
    if ![info exists mhProfile(header-suppress)] {
	set mhProfile(header-suppress) {.*}
    } else {
	set suppress {}
	foreach item $mhProfile(header-suppress) {
	    lappend suppress [string tolower $item]
	}
	set mhProfile(header-suppress) $suppress
    }
    if ![info exists mhProfile(header-display)] {
	set mhProfile(header-display) {subject from date to cc}
    } else {
	set display {}
	foreach item $mhProfile(header-display) {
	    lappend display [string tolower $item]
	}
	set mhProfile(header-display) $display
    }
    if ![info exists mhProfile(folder-order)] {
	set mhProfile(folder-order) {*}
    }
    if ![info exists mhProfile(folder-ignore)] {
	set mhProfile(folder-ignore) {.*}
    }
    foreach key {dist forw repl} {
	global exmh
	set exmh(anno,$key) 0
	if [info exists mhProfile($key)] {
	    if {[lsearch $mhProfile($key) -annotate] >= 0} {
		set exmh(anno,$key) 1
		Exmh_Debug "MH anno $key"
	    }
	}
    }
}
proc MhSetupNewUser {} {
    global mhProfile
    Widget_Toplevel .newuser "Setup MH environment"
    Widget_Message .newuser msg -aspect 1000 -text "
Exmh is a front end to the MH mail handling system.
Feel free to send comments and bug reports to
	exmhbugs@parc.xerox.com

It appears you have not used the MH mail system before.
Normally it creates a directory named ~/Mail and puts
its mail folders and some other files under there.
If you want your folders elsewhere, you will have to
exit Exmh and run the program install-mh by hand.

Is it ok if Exmh sets up your MH environment for you?
"

    Widget_Frame .newuser rim Pad {top expand fill}
    .newuser.rim configure -bd 10

    Widget_Frame .newuser.rim but Menubar {top fill}
    Widget_AddBut .newuser.rim.but yes "Yes" MhSetupNewUserInner
    Widget_AddBut .newuser.rim.but no "No, Exit" { destroy .newuser ; exit }
    update
    tkwait window .newuser
}
proc MhSetupNewUserInner {} {
    catch {exec inc < /dev/null} result
    Exmh_Status $result
    destroy .newuser
}
proc MhSetupDraftFolder {} {
    global mhProfile
    Widget_Toplevel .draft "Setup Draft Folder"
    Widget_Message .draft msg -aspect 1000 -text "
For the Compose, Reply, and Forward operations to work,
you need to have an MH drafts folder.  Creating one
requires making a directory (you choose the name)
and adding a draft-folder: entry
to your .mh_profile.

Should Exmh help you do that now?"

    Widget_Frame .draft rim Pad {top expand fill}
    .draft.rim configure -bd 10

    Widget_Label .draft.rim l {left} -text "Folder name: "
    Widget_Entry .draft.rim e {left fill}  -bg white
    .draft.rim.e insert 0 drafts

    Widget_Frame .draft.rim but Menubar {top fill}
    Widget_AddBut .draft.rim.but yes "Yes" MhSetupDraftFolderInner
    Widget_AddBut .draft.rim.but no "Exit" { exit }
    update
    tkwait window .draft
}
proc MhSetupDraftFolderInner {} {
    global mhProfile

    set dirname [.draft.rim.e get]
    set mhProfile(draft-folder) $dirname

    set dir $mhProfile(path)/$mhProfile(draft-folder)
    if ![file isdirectory $dir] {
	if [catch {
	    exec mkdir $dir
	    Exmh_Status "Created drafts folder \"+drafts\""
	} err] {
	    Exmh_Status "Cannot create a drafts folder! $err" purple
	    unset mhProfile(draft-folder)
	    destroy .draft
	    return
	}
    }
    if [catch {open ~/.mh_profile a} out] {
	Exmh_Status "Cannot open ~/.mh_profile: $out" purple
	unset mhProfile(draft-folder)
	destroy .draft
	return
    }
    puts $out "draft-folder: $dirname"
    Exmh_Status "draft-folder: $dirname"
    close $out

    destroy .draft
}
proc MhSetupUnseenSequence {} {
    global mhProfile
    Widget_Toplevel .unseen "Setup Unseen Sequence"
    Widget_Message .unseen msg -aspect 1000 -text "
Exmh will highlight unread messages for you so you
can easily find them.  However, this depends on an
unseen-sequence: entry in your .mh_profile file.

Should Exmh add one  for you now?"

    Widget_Frame .unseen rim Pad {top expand fill}
    .unseen.rim configure -bd 10
    Widget_Frame .unseen.rim but Menubar {top fill}
    Widget_AddBut .unseen.rim.but yes "Yes" MhSetupUnseenSequenceInner
    Widget_AddBut .unseen.rim.but no "No" { destroy .unseen }
    update
    tkwait window .unseen
}
proc MhSetupUnseenSequenceInner {} {
    global mhProfile
    set mhProfile(unseen-sequence) unseen

    if [catch {open ~/.mh_profile a} out] {
	Exmh_Status "Cannot open ~/.mh_profile: $out" purple
	unset mhProfile(unseen-sequence)
	destroy .unseen
	return
    }
    puts $out "unseen-sequence: $mhProfile(unseen-sequence)"
    close $out
    destroy .unseen
    Exmh_Status "Added unseen-sequence to .mh_profile" red
}
proc MhSetMailDrops {} {
    global exdrops env mhProfile

    catch {
	foreach folder [array names exdrops] {
	    unset exdrops($folder)
	}
    }
    foreach name {.exmhdrop .xmhcheck} {
	if {[file exists $env(HOME)/$name]} then {
	    set df [open $env(HOME)/$name]
	    while {![eof $df]} {
		# The second field is either a dropbox pathname
		# (absolute or env(HOME) relative), or it is
		# a POP hostname followed by an optional POP username
		gets $df line
		set fields [scan $line "%s %s %s" f d u]
		if {$fields >= 2} {
		    if {[string first / $d] > 0} {
			# hostnames ought not to have /'s
			set d "$env(HOME)/$d"
		    }
		    set folderDirectory "$mhProfile(path)/$f"
		    if ![file isdirectory $folderDirectory] {
			Exmh_Debug "No directory for folder $f ($name)"
			continue
		    }
		    if {$fields == 3} {
			set exdrops($f) [list $d $u]
		    } else {
			set exdrops($f) $d
		    }
		}
	    }
	    close $df
	    break
	}
    }
}

