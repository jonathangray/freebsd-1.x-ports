# 
# inc.tcl
#
# Incorporate new mail into folders.
# The routines here are prepared to be operating in a different
# interpreter than the main UI.  After they do the inc thing,
# they use BgRPC to invoke the UI-related routines in the
# correct interpreter.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Inc_Init {} {
    global exmh inc
    if {$exmh(slocal) == {}} {
	set exmh(slocal) slocal
    }
    if [info exists exmh(incStyle)] {
	# Backward compatibility
	set inc(style) $exmh(incStyle)
    }
    if [info exists exmh(incOnStart)] {
	# Backward compatibility
	set inc(onStartup) $exmh(incOnStart)
    }
    Preferences_Add "Incorporate Mail" \
"Exmh knows several ways to incorporate mail from your system's spool file into the MH folder hierarchy." {
	{inc(style) incStyle	{CHOICE inbox presort multidrop none} {Ways to Inc}
"inbox - basic MH inc from your spool file to inbox folder.
presort - slocal filtering directly into various folders.
multidrop - slocal filtering or POP delivery into various drop boxes,
as specified by ~/.xmhcheck, that are in turn inc'ed into folders.
none - you have an external agent that handles inc for you, so
don't bother trying it from within exmh."}
	{inc(onStartup) incOnStartup OFF	  {Inc at startup}
"Run your flavor of inc when exmh starts up."}
	{inc(onMapping) incOnMapping OFF	  {Inc on mapping}
"Run your flavor of inc when exmh is opened
after being iconic."}
	{inc(presortFeedback) incSortFeedback ON	  {Presort Inc feedback}
"This option causes a message to be generated each time a
new message is sorted into a folder.  There isn't much info,
it's just sort of a heart-beat feature..."}
    }
}
proc Inc_Startup {} {
    global inc
    if {$inc(onStartup)} {
	set s [Sound_Off]
	Inc
	if {$s} { Sound_On }
    } else {
	Exmh_Status "Checking folders"
	Flist_FindUnseen
    }
}
proc Inc_Mapped {} {
    global inc
    if {$inc(onMapping)} {
	Inc
    }
}
proc Inc {} {
    # There are three different styles of inc'ing messages...
    global inc
    case $inc(style) {
	{default inbox}	{ busy Inc_Inbox }
	presort		{ busy Inc_Presort }
	multidrop	{ busy Inc_All }
	none		{ return }
    }
}

proc Inc_Inbox {} {
    # Inc from the spool file to the inbox folder
    global exmh mhProfile
    if [info exists mhProfile(inbox)] {
	set inbox [string trimleft $mhProfile(inbox) +]
    } else {
	set inbox inbox
    }
    if [catch {exec inc +$inbox -truncate -nochangecur} incout] {
	Exmh_Debug Inc_Inbox +${inbox}: $incout
	set incout {}
    }
    BgRPC Inc_InboxFinish $inbox $incout Flist_Done
}
proc Inc_InboxFinish { f incout {hook {}} } {
    global exmh
    set msgids {}
    Scan_Iterate $incout line {
	set id [Ftoc_MsgNumberRaw $line]
	if {$id != {}} {
	    lappend msgids $id
	}
    }
    Exmh_Debug Inc_InboxFinish $f $msgids hook=$hook
    if {[llength $msgids] == 0} {
	return
    }
    Flist_AddUnseen $f $msgids
    if {$hook != {}} {
	eval $hook
    }
    if {$exmh(folder) == $f} {
	Scan_Inc $f $incout
    } else {
	Exmh_Status "New messages in $f" blue
    }
}

proc Inc_Presort {} {
    # Transfer from the mail spool file directly into folders
    global exmh mhProfile env inc
    # Just use inc to copy stuff into a temp directory
    exec folder -push +[exec folder -fast] < /dev/null > /dev/null
    exec folder +MyIncTmp < /dev/null > /dev/null
    if {[catch {exec inc +MyIncTmp -silent} err]} {
	# Some versions of inc exit non-zero when they should not.
	Exmh_Debug Inc_Presort +MyIncTmp: $err
    }
    if {[catch {glob $mhProfile(path)/MyIncTmp/*} files] == 0} {
	Exmh_Status "presort inc ..." red
	set rmFiles {}
	foreach file $files {
	    if {$file == "++"} {
		lappend rmFiles $file
		continue
	    }
	    # now use slocal to (presumably) shift things to a folder
	    if [catch {exec $exmh(slocal) -user $env(USER) < $file}] {
		Exmh_Status "Cannot classify $file"
	    } else {
		lappend rmFiles $file
		if {$inc(presortFeedback)} {
		    Exmh_Status [file tail $file] red
		}
	    }
	}
	if [llength $rmFiles] {
	    if [catch {eval exec rm $rmFiles} err] {
		Exmh_Debug Inc_Presort: $err
	    }
	}
    }
    if [catch {exec folder -pop < /dev/null > /dev/null} err] {
	Exmh_Debug $err
    }
    catch {exec rm -f $mhProfile(path)/MyIncTmp/$mhProfile(mh-sequences)}
    catch {exec rmdir $mhProfile(path)/MyIncTmp}

    Flist_FindUnseen		;# Needed to set new message state.
    BgRPC Inc_PresortFinish
}
proc Inc_PresortFinish {} {
    global exmh
    Mh_Folder $exmh(folder)	;# prestort inc has changed this to MyIncTmp
    if {[Flist_NumUnseen $exmh(folder)] > 0} {
	Scan_Folder $exmh(folder)
	Msg_ShowCurrent
    }
}

# The following are useful if you have lots of drop boxes
# because you use maildelivery to divert messages into dropboxes.
# This also works with POP hosts.

proc Inc_All {} {
    global exdrops exmh

    Exmh_Status "Checking dropboxes..." red
    set hits {}
    foreach folder [array names exdrops] {
	set dropname $exdrops($folder)
	if {[string first / $dropname] < 0} {
	    # Not a pathname, but a POP hostname
	    set host [lindex $dropname 0]
	    Exmh_Status $folder @ $host
	    if {[llength $dropname] == 2} {
		set user [lindex $dropname 1]
		set cmd [list exec inc +$folder -host $host -user $user -silent -truncate]
	    } else {
		set cmd [list exec inc +$folder -host $host -truncate]
	    }
	} else {
	    if { [file exists $dropname] && [file size $dropname] } {
		lappend hits $folder
		set cmd [list exec inc +$folder -file $dropname -truncate ]
	    } else {
		set cmd {}
	    }
	}
	if [llength $cmd] {
	    if {[catch $cmd incout] == 0} {
		BgRPC Inc_InboxFinish $folder $incout
	    } else {
		Exmh_Debug Inc_All +${folder}: $incout
	    }
	}
    }
    if {[llength $hits] > 0} {
	BgRPC Flist_Done
	Exmh_Status "New mail in: $hits" blue
    } else {
	Exmh_Status ""
    }
}
proc Inc_Pending {} {
    # Figure out which folders have stuff in their inbox
    global exdrops exwin
    set active {}
    foreach folder [array names exdrops] {
	set dropfile $exdrops($folder)
	if {[file exists $dropfile] && [file size $dropfile]} {
	    lappend active $folder
	}
    }
    if [llength $active] {
	# Output the list of active folder
	Exmh_Status "Active: $active" blue
    } else {
	Exmh_Status "No active folders" red
    }
    Flist_FindUnseen
}
