# 
# folder.tcl
#
# Folder operations, minus scan & inc.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Folder_Init {} {
    global exmh argc argv
    set exmh(target) {}		;# Name of target, for refile
    if {$argc > 0 && \
	[file isdirectory [Mh_Path [lindex $argv 0] {}]]} then {
	#scan named folder
	set exmh(folder) $argv
    } else {
	if [catch {exec folder -fast < /dev/null} f] {
	    set exmh(folder) {}
	} else {
	    set exmh(folder) $f
	}
    }
}

proc Folder_Summary { folder } {
    global mhProfile env
    set cwd [pwd]
    if [catch {cd $mhProfile(path)/$folder}] {
	cd $cwd
	return "${folder}+ does not exist"
    }
    set low 100000
    set high 0
    set num 0
    if {[catch {glob *} files] == 0} {
	foreach f $files {
	    if {[regexp {^[0-9]+$} $f]} {
		if {$f < $low} {
		    set low $f
		}
		if {$f > $high} {
		    set high $f
		}
		incr num
	    }
	}
    }
    cd $cwd
    if {$num <= 0} {
	return "${folder}+ has no messages"
    } else {
	return "${folder}+ $num msgs ($low-$high)"
    }
}

proc Folder_Change {f} {
    Exmh_Debug Folder_Change $f [time [list busy FolderChange $f]]
}
proc FolderChange {f} {
    global exmh mhProfile
    if {[Ftoc_Changes "Change folder"] > 0} {
	# Need to reselect previous button here
	return
    }
    # Trim off leading mail path
    if [regsub ^$mhProfile(path)/ $f {} newf] {
	set f $newf
    }
    if ![file isdirectory $mhProfile(path)/$f] {
	Exmh_Status "Folder $f doesn't exist" purple
	return
    }
    set oldFolder $exmh(folder)
    Exmh_Status "Changing to $f ..."
    if {$f != $exmh(folder)} {
	Exmh_Debug Exmh_CheckPoint [time Exmh_CheckPoint]
	global mhProfile
	Mh_Folder $f	;# Set MH folder state
    } else {
	Msg_CheckPoint	;# Force update of current message
    }
    global folderHook
    if [info exists folderHook(leave,$oldFolder)] {
	$folderHook(leave,$oldFolder) $oldFolder leave
    }
    Label_Folder $f
    Fdisp_HighlightCur $f
    set exmh(folder) $f
    Scan_Folder $f
    Msg_ShowCurrent

    # Take any required folder-specific action (e.g., for drafts folder)
    global folderHook
    if [info exists folderHook(enter,$f)] {
	$folderHook(enter,$f) $f enter
    }
}

proc Folder_Target {f} {
    global exmh mhProfile

    if ![file isdirectory $mhProfile(path)/$f] {
	Exmh_Status "$mhProfile(path)/$f doesn't exist"
	return
    }
    if {$exmh(folder) == $f} {
	Exmh_Status "Target must be different than current" red
	return
    }
    Fdisp_HighlightTarget $f
    set exmh(target) $f
    Exmh_Status "$f is target for moves"
}
proc Folder_TargetMove { f {moveProc Ftoc_MoveMark} } {
    Folder_Target $f
    Msg_Move $moveProc
}


proc Folder_Sort {} {
    global exmh

    if {[Ftoc_Changes "Sort"] == 0} then {
	Background_Wait
	Exmh_Status "Sorting folder..." blue
	Msg_CheckPoint
	Mh_Sort $exmh(folder)
	Msg_ClearCurrent
	Scan_Folder $exmh(folder)
    }
}

proc Folder_Pack {} {
    global exmh

    if {[Ftoc_Changes "Pack"] == 0} then {
	Background_Wait
	Exmh_Status "Packing folder..." blue
	Msg_CheckPoint
	Mh_Pack $exmh(folder)
	Msg_ClearCurrent
	Scan_Folder $exmh(folder)
    }
}
proc Folder_Commit { {rmmCommit Mh_Rmm} {moveCommit Mh_Refile} } {
    busy FolderCommit $rmmCommit $moveCommit
    return 0
}
proc FolderCommit { rmmCommit moveCommit } {
    global exmh exwin

    Ftoc_RangeUnHighlight
    Ftoc_Commit $rmmCommit $moveCommit
    Msg_ShowCurrent
    Background_Wait	;# Let folder ops complete
    Exmh_CheckPoint	;# before writing out cache
    Label_Folder $exmh(folder)
    # The following isn't strictly necessary, but it fixes up any
    # display bugs caused by races between exmh-bg FindUnseen and the UI
#    Flist_Update $exmh(folder)
}

