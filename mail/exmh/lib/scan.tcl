# 
# scan.tcl
#
# Folder scanning, with optimizations.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

#### Display folder contents

proc Scan_Folder {F} {
    Exmh_Debug Scan_Folder $F
    Exmh_Debug Scan_Folder [time [list ScanFolder $F]]
}
proc ScanFolder {F} {
    global mhProfile flist ftoc
    set cacheFile $mhProfile(path)/$F/.xmhcache

    set ftoc(displayValid) 0
    set ftoc(displayDirty) 0
    # Scan the folder, being clever in two different ways, if possible.
    # 1) if we have flist, then we know how many new messages there
    # are, so we just scan those and merge that info into our cached
    # scan output
    # 2) if there are no new messages, just read the scan cache
    # 3) if there have been folder operations that invalidated the cache,
    # then do a full scan.

    set U [Flist_NumUnseen $F]
    if {$U > 0} {
	set overlap 5
	set numNewMsgs [expr $U+$overlap]
	if {$numNewMsgs > $overlap} {
	    set scanio [open "|scan +$F last:$numNewMsgs -width 100"]
	    Exmh_Status "scanning new msgs in $F ..." blue
	    if [ScanMerge $F $scanio $cacheFile] {
		set ftoc(displayValid) 1
		set ftoc(displayDirty) 1
		Label_Folder $F
		Exmh_Status $F
		return
	    }
	}
    }
    # Inc_PresortFinish will call Scan_Folder with pending changes
    # on the assumption that ScanMerge will always work.
    # In case it doesn't, we bail out here.  Other calls to Scan_Folder
    # make their own check of Ftoc_Changes
    if [Ftoc_Changes "Scan Overwrite" noauto] {
	Exmh_Status ""
	return
    }

    if { ! [Scan_CacheValid $F]} {
	Exmh_Status "rescanning $F ..." red
	Scan_IO $F [open "|scan +$F -width 100"]
	set ftoc(displayDirty) 1
    } else {
	Exmh_Status "scan cache hit for $F ..." blue
	Scan_IO $F [open $cacheFile]
	set ftoc(displayDirty) 0
    }
    Exmh_Status $F
    Label_Folder $F
    set ftoc(displayValid) 1
}
proc Scan_FolderForce {} {
    global exmh mhProfile
    set F $exmh(folder)
    set cacheFile $mhProfile(path)/$F/.xmhcache
    Exmh_Debug Scan_FolderForce
    Msg_CheckPoint		;# save current sequence information
    if {$F == ""} {
	Exmh_Status "No current folder" red
    } else {
	if {! [Ftoc_Changes Rescan]} {
	    Label_Folder $F
	    Exmh_Status "rescanning $F ..."
	    Scan_IO $F [open "|scan +$F -width 100"]
	}
    }
}
proc Scan_Iterate { incout lineVar body } {
    upvar $lineVar line
    foreach line [split $incout \n] {
	if [regexp ^Incorporating $line] {
	    continue
	}
	if {[string length $line] > 0} {
	    uplevel $body
	}
    }
}

proc Scan_Inc {folder incOutput} {
    # Append output of an Inc to the scan display
    ScanAddLineInit
    Scan_Iterate $incOutput l {
	ScanAddLine $l
    }
    ScanAddLineCleanup $folder
    Ftoc_ShowUnseen $folder
    global exwin ftoc
    Ftoc_Update [expr [lindex [split [$exwin(ftext) index end] .] 0]-1] $folder
    set ftoc(displayDirty) 1
}
proc Scan_IO {folder scanIO } {
    Exmh_Debug Scan_IO [time [list ScanIO $folder $scanIO]]
}
proc ScanIO {folder scanIO } {
    global exmh exwin

    ScanAddLineReset $folder
    if [catch {
	ScanAddLines [read $scanIO]
	close $scanIO
    } err] {
	Exmh_Status $err red
    }
    ScanAddLineCleanup $folder
    Ftoc_ShowUnseen $folder
    Msg_Reset [expr [lindex [split [$exwin(ftext) index end] .] 0]-1] $folder
}
proc ScanMerge {folder scanIO cacheFile} {
    global ftoc		;# In bed with ftoc display here
    set ftoc(mergeok) 0

    if [catch {
	if {$ftoc(folder) == $folder} {
	    ScanMergeCache $folder $scanIO
	} else {
	    ScanMergeCache $folder $scanIO $cacheFile
	}
    } err] {
	Exmh_Error "ScanMerge \n$err"
    }
    return $ftoc(mergeok)
}
proc ScanMergeCache { folder scanIO {cacheFile {}} } {
    global ftoc
    if [catch {
	set scan [split [read $scanIO] \n]
	close $scanIO
    } err] {
	Exmh_Debug ScanMergeCache: $err
	return 0
    }
    if {$cacheFile != {}} {
	if [catch {open $cacheFile} cacheIO] {
	    Exmh_Debug ScanMergeCache: $cacheIO
	    return 0
	}
	# Read in cache and the scan output of the last several messages
	set rawCache [read $cacheIO]
	set cache [split $rawCache \n]
	close $cacheIO
	set displayCache 0
    } else {
	# Use the display as the cache, not the file
	set displayCache 1
    }

    # Match up the scan output with the last lines in the cache
    set sync 0
    set scanIndex 0
    set scanLine [lindex $scan $scanIndex]
    set scanMsgNum [Ftoc_MsgNumberRaw $scanLine]
    if {$displayCache} {
	set nlines $ftoc(numMsgs)
    } else {
	# Split adds extra blank element, hence the -2 here
	set nlines [expr [llength $cache]-2]
    }
    for {set L $nlines} {$L >= 0} {incr L -1} {
	if {$displayCache} {
	    set cacheMsgNum [Ftoc_MsgNumber $L]
	} else {
	    set line [lindex $cache $L]
	    set cacheMsgNum [Ftoc_MsgNumberRaw $line]
	}
	if {$cacheMsgNum == $scanMsgNum} {
	    incr L
	    incr scanIndex
	    set sync 1
	    break
	}
    }
    if {! $sync} {
	return 0
    }
    for {} {$L <= $nlines} {incr L ; incr scanIndex} {
	set scanLine [lindex $scan $scanIndex]
	set scanMsgNum [Ftoc_MsgNumberRaw $scanLine]
	if {$displayCache} {
	    set cacheMsgNum [Ftoc_MsgNumber $L]
	} else {
	    set line [lindex $cache $L]
	    set cacheMsgNum [Ftoc_MsgNumberRaw $line]
	}
	if {$cacheMsgNum != $scanMsgNum} {
	    return 0
	}
    }
    # Fill in the display if things seem ok
    if {$displayCache} {
	ScanAddLineInit
    } else {
	ScanAddLineReset $folder
	ScanAddLines $rawCache
    }
    set scanLength [llength $scan]
    for {} {$scanIndex < $scanLength} {incr scanIndex} {
	set scanLine [lindex $scan $scanIndex]
	if {$scanLine == {}} {
	    continue
	}
	ScanAddLine $scanLine
    }
    ScanAddLineCleanup $folder
    Ftoc_ShowUnseen $folder
    global exwin
    if {$displayCache} {
	Ftoc_Update [expr [lindex [split [$exwin(ftext) index end] .] 0]-1] \
		    $folder
    } else {
	Msg_Reset [expr [lindex [split [$exwin(ftext) index end] .] 0]-1] \
		  $folder
    }
    set ftoc(mergeok) 1
    return 1
}

proc ScanAddLineInit {} {
    global exmh exwin
    $exwin(ftext) configure -state normal
}
proc ScanAddLineReset { folder } {
    global exwin ftoc
    if {$ftoc(folder) == $folder} {
	# Rescanning a folder, so save mark state
#	Ftoc_Save $folder
    }
    ScanAddLineInit
    $exwin(ftext) delete 0.0 end
}
proc ScanAddLine { line } {
    global exwin
    $exwin(ftext) insert end "$line\n"
}
proc ScanAddLines { text } {
    global exwin
    $exwin(ftext) insert end $text
}
proc ScanAddLineCleanup { folder } {
    global exwin flist ftoc
    if {$ftoc(folder) == $folder} {
	# Restore mark state
#	Ftoc_Restore $folder
    }
    set ftoc(folder) $folder
    $exwin(ftext) configure -state disabled
    WidgetTextYview $exwin(ftext) end
}
proc Scan_ProjectSelection { ids } {
    global ftoc exwin
    set ftoc(displayValid) 0	;# Don't cache this display
    set lines {}
    set num 0
    foreach id $ids {
	set L [Ftoc_FindMsg $id]
	if {$L != {}} {
	    lappend lines [$exwin(ftext) get $L.0 $L.end]
	    incr num
	}
    }
    ScanAddLineReset $ftoc(folder)
    foreach line $lines {
	ScanAddLine $line
    }
    ScanAddLineCleanup $ftoc(folder)
    Msg_ClearCurrent
    Msg_Reset $num
}
proc Scan_CacheValid {F} {
    # Maintain a cache of folder listings
    global mhProfile exmh
    set cacheFile $mhProfile(path)/$F/.xmhcache
    if {![file exists $cacheFile] || ![file size $cacheFile]} {
	return 0
    }
    if {[file mtime $mhProfile(path)/$F] >
	[file mtime $cacheFile]} {
	return 0
    }
    return 1
}
proc Scan_CacheUpdate {} {
    global exmh mhProfile exwin ftoc
    set folder $exmh(folder)
    if {$folder == {}} {
	return
    }
    if {!$ftoc(displayValid) || !$ftoc(displayDirty)} {
	return
    }
    set cacheFile $mhProfile(path)/$folder/.xmhcache
    if [catch {
	set cacheIO [open $cacheFile w]
	set curLine [Ftoc_ClearCurrent]			;# Clear +
	set display [$exwin(ftext) get 1.0 end]
	Ftoc_Change [Ftoc_MsgNumber $curLine] $curLine	;# Restore it
	puts $cacheIO $display nonewline
	close $cacheIO
	set ftoc(displayDirty) 0
	Exmh_Debug Scan_CacheUpdate OK $folder
    } err] {
	Exmh_Debug Scan_CacheUpdate error $err
    }
}

# Move scan lines to the scan cache for another folder
proc Scan_Move { folder scanlinesR new } {
    global mhProfile
    set cacheFile $mhProfile(path)/$folder/.xmhcache
    if ![file writable $cacheFile] {
	Exmh_Debug Scan_Move $folder scan cache not writable
	return
    }
    # Reverse engineer the scan format
    if ![regexp {( *)([0-9]+)} [lindex $scanlinesR 0] prefix foo2 number] {
	Exmh_Debug Scan_Move cannot handle scan format
	return
    }
    Exmh_Debug Scan_Move $folder
    set len [string length $prefix]
    set fmt [format "%%%dd%%s" $len]
    set cacheIO [open $cacheFile a]
    for {set i [expr [llength $scanlinesR]-1]} {$i >= 0} {incr i -1} {
	set line [lindex $scanlinesR $i]
	if [regsub {( *[0-9]+)(\+)} $line {\1 } newline] {
	    puts $cacheIO [format $fmt $new [string range $newline $len end]] \
		nonewline
	} else {
	    puts $cacheIO [format $fmt $new [string range $line $len end]] \
		nonewline
	}
	incr new
    }
    close $cacheIO
}
proc Scan_AllFolders { {force 0} } {
    global flist mhProfile
    if [catch {open /tmp/scancmds w} out] {
	Exmh_Status "Scan_AllFolders $out"
	return
    }
    puts $out "wm withdraw ."
    set myname [winfo name .]
    foreach f $flist(allfolders) {
	if {$force || ! [Scan_CacheValid $f]} {
	    puts $out "send $myname \{Exmh_Status \"scan +$f\"\}"
	    puts $out "catch \{exec scan +$f > $mhProfile(path)/$f/.xmhcache\}"
	}
    }
    puts $out "send $myname \{Exmh_Status \"scans completed\"\}"
    puts $out "exec rm /tmp/scancmds"
    puts $out exit
    close $out
    Exmh_Status "wish -f /tmp/scancmds &" blue
    exec wish -f /tmp/scancmds &
}
