#
# exmh_background.tcl --
#	Periodic background processing
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

# Background processing

proc Background_Init {} {
    global exmh env background

    if ![info exists env(MAIL)] {
	set spool /usr/spool/mail/$env(USER)
    } else {
	set spool $env(MAIL)
    }
    Preferences_Add "Background Processing" \
"A second process is used to perform various background tasks for Exmh.  These options control its behavior." \
    [list \
	{ exmh(bgAsync) bgAsync ON {Separate background process}
"This setting determines whether or not the background
processing is done in a separate process or not.  A
separate process causes less interference with the user
interface, but might take up more machine resources."} \
	{ exmh(background) bgAction {CHOICE off count msgchk flist inc} {Background processing}
"exmh can periodically do some things for you:
count - count new messages sitting in your spool file.
msgchk - run the MH msgchk program.
flist - check for new mail in all folders.
inc - just like clicking the Inc button yourself.
off - do nothing in the background."} \
	{ exmh(bgPeriod) bgPeriod 10 {Period (minutes)}
"How often to do background task"} \
	[list exmh(spool) bgSpool $spool {Mail spool pathname} \
"Pathname for the mail spool file that gets new messages."] \
    ]
    # due to a TK bug I cannot trace the radio button variable directly.
    # I can hack around it by tracing the bgPeriod, which is always
    # set by Preferences because it is an entry
    trace variable exmh(bgPeriod) w BackgroundFixup
    set exmh(lastBackground) $exmh(background)
    set background(lastMsgChk) {}
    set exmh(sendErrors) 0
}
proc Background_Startup {} {
    global exmh inc

    if [info exists exmh(interp)] {
	# Already in the background interpreter.
	# Invoked because the style of background processing changed
	Background_DoPeriodic
	return
    }
    if [info exists exmh(bgInterp)] {
	if {[catch {send $exmh(bgInterp) {Background_Startup}}] == 0} {
	    # Background interp already running
	    return
	}
    }
    Background_Cleanup	;# In case the bg process is really there anyway

    if {! $exmh(bgAsync) } {
	# Do not run a separate process
	Background_DoPeriodic
	return
    }
    global tk_version mh_path
    set prog exmh-bg
    set cmd [list exec $prog [winfo name .] $exmh(library) $mh_path &]
    if [catch {
	if {$tk_version <= 3.2} {
	    eval $cmd
	    set pid [exec ps | grep exmh-bg | sed /grep/d]
	    set pid [lindex $pid 0]
	} else {
	    set pid [eval $cmd]
	}
	set exmh(bgPid) $pid
	Exmh_Debug Background_Startup $exmh(background) pid $pid
	after [expr 10*1000*60] BackgroundCheckup
    } err] {
	Exmh_Status "exmh-bg error: $err"
	Background_DoPeriodic
    }
}
proc BackgroundCheckup {} {
    global exmh
    if [BgLostPid $exmh(bgPid) exmh-bg] {
	catch {unset exmh(bgInterp)}
	Exmh_Debug Restarting exmh-bg
	Background_Startup
    } else {
	after [expr 10*1000*60] BackgroundCheckup
    }
}
proc Background_Register { bgInterp {bgPid junk} } {
    # Invoked by the background interpreter so we can talk back to it
    global exmh
    set exmh(bgInterp) $bgInterp
    Exmh_Debug "Background interp is $bgInterp, pid $exmh(bgPid)"

    # Bundle up some parameters that could be overridden on the
    # command line and so won't get picked up from the Xresources
    set exmh(pid) [pid]		;# TCL 7.* dependent
    foreach varname {exmh(background) exmh(bgPeriod) exmh(pid)} {
	lappend results [list $varname [set $varname]]
    }
    return $results
}
proc Background_Cleanup {} {
    global exmh
    catch {send $exmh(bgInterp) Exmhbg_Done}
    catch {exec kill $exmh(bgPid)}
}
proc Background_DoPeriodic {} {
    global exmh
    Exmh_Debug Background_DoPeriodic $exmh(background)
    case $exmh(background) {
	"counter" { after [expr $exmh(bgPeriod)*1000*60] BackgroundCount }
	"msgchk"  { after [expr $exmh(bgPeriod)*1000*60] BackgroundMsgChk }
	"inc"     { after [expr $exmh(bgPeriod)*1000*60] BackgroundInc }
	"flist"   { after [expr $exmh(bgPeriod)*1000*60] BackgroundFlist }
	"off"     { after [expr $exmh(bgPeriod)*1000*60] Background_DoPeriodic }
    }
}
proc Background_Off {} {
    global exmh
    set exmh(background) {}
}
proc BackgroundFixup { args } {
    global exmh
    Exmh_Debug BackgroundFixup $exmh(lastBackground) $exmh(background)
    if {$exmh(background) != $exmh(lastBackground)} {
	Background_Startup
	set exmh(lastBackground) $exmh(background)
    }
}
proc BackgroundMsgChk {} {
    global exmh env background
    if {$exmh(background) != "msgchk"} {
	return
    }
    set result [Mh_MsgChk]
    if {$result != $background(lastMsgChk)} {
	Exmh_Status $result
	set background(lastMsgChk) $result
    }
    if [catch {after [expr $exmh(bgPeriod)*1000*60] {BackgroundMsgChk}} msg] {
	puts stderr "BackgroundMsgChk catch: $msg"
    }
}
proc Background_Counter {} {
    global exmh
    if {$exmh(background) != "counter"} {
	set exmh(background) "counter"
	BackgroundCount
    }
}
proc BackgroundCount {} {
    global exmh env
    if {$exmh(background) != "counter"} {
	return
    }
    if ![catch {Mh_MsgCount $exmh(spool)} newmsgs] {
	BackgroundNewMsgs [string trim $newmsgs]
    }
    if [catch {after [expr $exmh(bgPeriod)*1000*60] {BackgroundCount}} msg] {
	puts stderr "BackgroundCount catch: $msg"
    }
}

proc BackgroundNewMsgs { N } {
    global exmh
    if ![info exists exmh(numUnInced)] {
	set exmh(numUnInced) 0
    }
    if {$N > 0} {
	if {$N == 1} {
	    set msg "msg"
	} else {
	    set msg "msgs"
	}
	Exmh_Status "You have $N spooled $msg" blue
    } else {
	if {$exmh(numUnInced) > 0} {
	    Exmh_Status ""
	}
    }
    set exmh(numUnInced) $N
}
proc Background_Inc {} {
    global exmh
    if {$exmh(background) != "inc"} {
	set exmh(background) "inc"
	BackgroundInc
    }
}
proc BackgroundInc {} {
    global exmh
    if {$exmh(background) != "inc"} {
	return
    }
    Inc
    catch {after [expr $exmh(bgPeriod)*1000*60] BackgroundInc}
}
proc Background_Flist {} {
    global exmh
    if {$exmh(background) != "flist"} {
	set exmh(background) "flist"
	BackgroundFlist
    }
}
proc BackgroundFlist {} {
    global exmh
    if {$exmh(background) != "flist"} {
	return
    }
    Flist_FindUnseen		;# Update folder highlights
    BgRPC Inc_PresortFinish	;# Update scan listing
    catch {after [expr $exmh(bgPeriod)*1000*60] BackgroundFlist}
}

# Invoke something in the background interpreter
proc BgAction { args } {
    global exmh
    if [info exists exmh(bgInterp)] {
	if {! [catch {send $exmh(bgInterp) [list after 1 $args]} err]} {
	    return
	}
	Exmh_Debug BgAction $err
    }
    eval $args
}

# Invoke a routine in the UI interpreter, if it exists, else ourselves.
# The idea is that the command might or might not be invoked
# from the background interpreter
proc BgRPC { args } {
    global exmh
    set check [info exists exmh(pid)]
    if [info exists exmh(interp)] {
	# Send command to main, front-end interpreter
	set fail {}
	if {$check && [BgLostPid $exmh(pid) exmh]} {
	    # Front-end died or may have restarted - bail out
	    set fail "process $exmh(pid)"
	} else {
	    if [catch {send $exmh(interp) $args} err] {
		case $err {
		    {remote\ interpreter\ did\ not\ respond} {
			if {$check && [BgLostPid $exmh(pid) exmh]} {
			    set fail "process $exmh(pid)"
			}
		    }
		    {no\ registered\ interpeter*} {
			set fail "interp $exmh(interp)"
		    }
		}
	    }
	}
	if {"$fail" != ""} {
	    unset exmh(interp)
	    catch {puts stderr "exmh-bg: lost UI $fail"}
	    exit
	}
    } else {
	# Eval in main, front-end interpreter
	eval $args
    }
}
proc BgLostPid { pid {name notused} } {
    if [catch {exec ps $pid} err] {
	if [string match {[Uu]sage:*} $err] {
	    return [catch {exec ps -p $pid}]
	} else {
	    return 1
	}
    } else {
	foreach line [split $err \n] {
	    if {[string compare [lindex $line 0] $pid] == 0} {
		return 0
	    }
	}
	return 1
    }
}

proc Background_Preferences {} {
    # Tell the background interpreter to update its per-user settings
    global exmh
    if [info exists exmh(bgInterp)] {
	catch {send $exmh(bgInterp) [list PreferencesReset]}
    }
}

proc Background_Rmm { rmmProc folder msgs } {
    global exmh
    global background
    if [info exists exmh(bgInterp)] {
	if [catch {
	    send $exmh(bgInterp) [list after 1 \
				[list BackgroundRmm $rmmProc $folder $msgs]]
	    BackgroundPending "remove"
	} err] {
	    Exmh_Debug Background_Rmm $err
	    catch {$rmmProc $folder $msgs}
	}
    } else {
	$rmmProc $folder $msgs
    }
}
proc BackgroundRmm { rmmProc folder msgs } {
    global exmh
    if [catch {$rmmProc $folder $msgs} err] {
	Exmh_Status "$rmmProc: $err"
    }
    if [catch {
	send $exmh(interp) [list BackgroundComplete "remove"]
    } err] {
	# The UI is probably scanning a new folder
	Exmh_Status $err
    }
}
proc Background_Refile { refileProc srcfolder msgs folder ok scan new} {
    global exmh
    if [info exists exmh(bgInterp)] {
	if [catch {
	    send $exmh(bgInterp) [list after 1 \
		[list BackgroundRefile $refileProc $srcfolder $msgs $folder $ok $scan $new]]

	    BackgroundPending "refile $folder"
	} err] {
	    Exmh_Debug Background_Refile $err
	    catch {$refileProc $srcfolder $msgs $folder}
	}
    } else {
	$refileProc $srcfolder $msgs $folder
	if {$ok} {
	    Scan_Move $folder $scan $new
	}
    }
}
proc BackgroundRefile { refileProc srcfolder msgs folder ok scan new } {
    global exmh
    if [catch {$refileProc $srcfolder $msgs $folder} err] {
	Exmh_Status "$refileProc: $err"
    } else {
#	Exmh_Status "Background $refileProc folder done"
    }
    if [catch {
	send $exmh(interp) [list BackgroundComplete "refile $folder"]
    } err ] {
	Exmh_Status $err
    }
    if {$ok} {
	Scan_Move $folder $scan $new
    }
}

proc BackgroundPending { action } {
    global bgaction
    set bgaction($action) 1
    Exmh_Debug BackgroundPending $action
}
proc BackgroundComplete { action } {
    global bgaction
    catch {unset bgaction($action)}
    Exmh_Debug BackgroundComplete $action
    if {[Background_Outstanding] == {}} {
	Exmh_Status "background actions complete"
    }
}
proc Background_Outstanding {} {
    global bgaction background
    if [catch {array names bgaction} actions] {
	set background(complete) 1
	return {}
    } else {
	if {$actions == {}} {
	    set background(complete) 1
	}
	return $actions
    }
}
proc Background_Wait {} {
    global background
    set background(complete) 0
    set pending [Background_Outstanding]
    if {$pending != {}} {
	Exmh_Status "waiting... $pending"
	tkwait variable background(complete)
    }
}
