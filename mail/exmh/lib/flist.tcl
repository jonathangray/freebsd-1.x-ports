#
# flist.tcl
#
# Manage the folder list.  This includes information about which folders
# have nested folders under them, and which folders have unseen messages.
# The display of this information is done by the fdisp.tcl module
#
# Some of the routines here are set up to run in a background interpreter.
# When you see calls to BgRPC it is invoking the routine in the
# forground UI interpreter, whether or not the current routine
# is already running there.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.


proc Flist_Init {} {
    global flist
    FlistInitVars		;# Reset unseen msgs state
    set flist(context) {}
    set flist(contextMtime) 0
    set flist(cacheFileMtime) 0
    Flist_FindAllFolders
}
proc FlistInitVars {} {
    global flist
    set flist(unseen) {}	;# Sequence of folders to visit
    set flist(newMsgs) 0
    set flist(newFolders) 0
    #   flist(new,$folder)	;# not always defined
    #   flist(newseq,$folder)	;# not always defined
}

### Routines to find all folders, figure out which have nested folders, etc.

proc Flist_Refresh {} {
    global flist
    FlistFindAllInner
    Fdisp_Redisplay
    Flist_FindUnseen
}

proc Flist_FindAllFolders {} {
    global flist mhProfile flistSubCache flistParents

    set flist(cacheFile) $mhProfile(path)/.folders
    if ![file readable $flist(cacheFile)]  {
	FlistFindAllInner
    } else {
	if {![info exists flist(allfolders)]||
	    [file mtime $flist(cacheFile)] > $flist(cacheFileMtime)} {
	    set in [open $flist(cacheFile)]
	    set flist(allfolders) [FlistSort [split [read $in] \n]]
	    close $in
	    set flist(cacheFileMtime) [file mtime $flist(cacheFile)]
	    FlistSubFoldersInit
	}
    }

}
proc FlistFindAllInner {} {
    global flist flistSubCache flistParents mhProfile
    Widget_Toplevel .scanning "Scanning..."
    Widget_Message .scanning msg  -text "
Scanning for nested folders.
(folders -fast -recurse)
This can be very slow if you have
not patched the MH folders command.
A patch comes with exmh.

The results are cached in
$mhProfile(path)/.folders
so you won't have to wait like
this until you press the Folders
button to update the folder set.

Please wait...
"
    Exmh_Status "Scanning for nested folders ..." purple
    update
    set flist(allfolders) [FlistSort [split [exec folders -fast -recurse] \n]]
    FlistSubFoldersInit
    FlistCacheFolderList
    destroy .scanning
}
proc Flist_AddFolder { folder } {
    global flist
    if {[lsearch $flist(allfolders) $folder] >= 0} {
	return
    }
    lappend flist(allfolders) $folder
    set flist(allfolders) [FlistSort $flist(allfolders)]
    FlistSubFoldersInit
    FlistCacheFolderList
    Fdisp_Redisplay
}
proc Flist_DelFolder { folder } {
    global flist
    set ix [lsearch $flist(allfolders) $folder]
    if {$ix < 0} {
	return
    }
    set flist(allfolders) [FlistSort [lreplace $flist(allfolders) $ix $ix]]
    FlistSubFoldersInit
    FlistCacheFolderList
    Fdisp_Redisplay
}
proc FlistCacheFolderList {} {
    global flist
    if [catch {open $flist(cacheFile) w} out] {
	Exmh_Status "Cannot cache folder list: $out" red
    } else {
	foreach f $flist(allfolders) {
	    puts $out $f
	}
	close $out
	set flist(cacheFileMtime) [file mtime $flist(cacheFile)]
    }
}
proc FlistSubFoldersInit {} {
    global flist subFolders flistSubCache flistParents

    catch {unset subFolders}	;# Map from name to list of children
    catch {unset flistSubCache}
    catch {unset flistParents}
    foreach f $flist(allfolders) {
	append subFolders([file dirname $f]) "$f "
    }
}
proc Flist_SubFolders {{folder .}} {
    global subFolders

    return [info exists subFolders($folder)]
}
proc Flist_FolderSet { {subfolder .} } {
    #  Find all folders at a given level in the folder hierarchy
    global flist flistSubCache
    if [info exists flistSubCache($subfolder)] {
	return $flistSubCache($subfolder)
    }
    foreach f $flist(allfolders) {
	set parent [file dirname $f]
	if {$subfolder == $parent || $subfolder == $f} {
	    lappend result $f
	}
    }
    if ![info exists result] {
	return {}
    } else {
	set flistSubCache($subfolder) $result
	return $result
    }
}

proc FlistSeq { folder sequence } {
    global mhProfile
    # Explode a sequence into a list of message numbers
    set seq {}
    set rseq {}
    foreach range [split [string trim $sequence]] {
	set parts [split [string trim $range] -]
	if {[llength $parts] == 1} {
	    lappend seq $parts
	    set rseq [concat $parts $rseq]
	} else {
	    for {set m [lindex $parts 0]} {$m <= [lindex $parts 1]} {incr m} {
		lappend seq $m
		set rseq [concat $m $rseq]
	    }
	}
    }
    # Hack to weed out unseen sequence numbers for messages that don't exist
    foreach m $rseq {
	if ![file exists $mhProfile(path)/$folder/$m] {
	    Exmh_Debug $mhProfile(path)/$folder/$m not found
	    set ix [lsearch $seq $m]
	    set seq [lreplace $seq $ix $ix]
	} else {
	    # Real hack
	    break
	}
    }
    return $seq
}
proc Flist_UnseenMsgs { folder } {
    global flist
    set unseen [Mh_Unseen $folder]
    set seen [Msg_Seen]
    foreach id $seen {
	set ix [lsearch $unseen $id]
	if {$ix >= 0} {
	    set unseen [lreplace $unseen $ix $ix]
	}
    }
    if {[llength $unseen] == 0} {
	catch {
	    FlistUnseenFolder $folder
	}
    } else {
	if ![info exists flist(new,$folder)] {
	    Fdisp_HighlightUnseen $folder
	}
	set flist(newseq,$folder) $unseen
	set flist(new,$folder) [llength $unseen]
    }
    return $unseen
}
proc Flist_NumUnseen { folder } {
    global flist
    if [info exists flist(new,$folder)] {
	return $flist(new,$folder)
    } else {
	return 0
    }
}
proc Flist_UnseenFolders {} {
    global flist
    return $flist(unseen)
}
proc FlistReset {} {
    global flist
    FlistInitVars
    
    Exmh_Debug FlistReset
}
proc Flist_AddUnseen {folder seq} {
    global flist exmh
    set num [llength $seq]
    if {$num <= 0} {
	return
    }
    Exmh_Debug Flist_AddUnseen $folder $seq
    if {$folder == $exmh(folder)} {
	Flist_UpdateUnseen $folder $seq
	return
    }
    incr flist(newMsgs) $num
    set flist(new,$folder) $num
    set flist(newseq,$folder) $seq
    incr flist(newFolders)
    lappend flist(unseen) $folder
    Fdisp_HighlightUnseen $folder
}
proc Flist_UpdateUnseen {folder seq} {
    global flist exmh
    Exmh_Debug Flist_UpdateUnseen $folder $seq seen=[Msg_Seen]
    # Check overlap with already seen msgs
    foreach id [Msg_Seen] {
	set ix [lsearch $seq $id]
	if {$ix >= 0} {
	    set seq [lreplace $seq $ix $ix]
	}
    }
    set num [llength $seq]
    if {$num <= 0} {
	return
    }
    incr flist(newMsgs) $num
    set flist(new,$folder) $num
    set flist(newseq,$folder) $seq
    if {[lsearch $flist(unseen) $folder] < 0} {
	incr flist(newFolders)
	lappend flist(unseen) $folder
    }
    Fdisp_HighlightUnseen $folder
}
proc Flist_Done {} {
    global flist exmh
    if {$flist(newMsgs) > 0} {
	if {$flist(newMsgs) == 1} {set msg "msg"} else {set msg "msgs"}
	if {$flist(newFolders) == 1} {set f "folder"} else {set f "folders"}
	Exmh_Status "$flist(newMsgs) unread $msg in $flist(newFolders) $f" blue
	if ![info exists flist(lastNewMsgs)] {
	    set flist(lastNewMsgs) 0
	}
	set delta [expr {$flist(newMsgs) - $flist(lastNewMsgs)}]
	Exmh_Debug "Flist_Done deltaNew $delta"
	if {$delta > 0} {
	    Flag_NewMail
	    Sound_Feedback $delta
	}
    } else {
	Flag_NoUnseen
	Exmh_Status "No unread msgs" blue
    }
    set flist(lastNewMsgs) $flist(newMsgs)
}


# Flist enumerates folders that have unseen messages.
proc Flist_FindUnseen {} {
    Exmh_Debug Flist_FindUnseen begin
    Exmh_Debug Flist_FindUnseen end [time FlistFindUnseen]
}

proc FlistFindUnseen {} {
    global mhProfile flist curFolder
    BgRPC FlistReset
    FlistGetContext
    foreach f $flist(allfolders) {
	set hit 0
	foreach line $flist(context) {
	    set line [split $line]
	    set key [lindex $line 0]
	    if {$key == "atr-$mhProfile(unseen-sequence)-$mhProfile(path)/$f:"} {
		set seq [lindex [split $line :] 1]
		BgRPC Flist_AddUnseen $f [FlistSeq $f $seq]
		set hit 1
		break
	    }
	}
	if {! $hit} {
	    if {[catch {open $mhProfile(path)/$f/$mhProfile(mh-sequences)} in] == 0} {
		set se \n[read $in]
		if [regexp \n$mhProfile(unseen-sequence):\[^\n\]*\n $se line] {
		    set seq [lindex [split $line :\n] 2]
		    BgRPC Flist_AddUnseen $f [FlistSeq $f $seq]
		}
		close $in
	    }
	}
    }
    BgRPC Flist_Done
}
proc FlistGetContext {} {
    global flist mhProfile
    if {$flist(contextMtime) < [file mtime $mhProfile(path)/$mhProfile(context)]} {
	if {[catch {open $mhProfile(path)/$mhProfile(context)} in] == 0} {
	    set flist(context) [split [read $in] \n]
	    set flist(contextMtime) [file mtime $mhProfile(path)/$mhProfile(context)]
	    close $in
	    Exmh_Debug "Reloaded $mhProfile(path)/$mhProfile(context) at $flist(contextMtime)"
	}
    }
}
proc Flist_MsgSeen { msgid } {
    global flist exmh
    if [info exists flist(newseq,$exmh(folder))] {
	set ix [lsearch $flist(newseq,$exmh(folder)) $msgid]
	if {$ix >= 0} {
	    set flist(newseq,$exmh(folder)) \
		[lreplace $flist(newseq,$exmh(folder)) $ix $ix]
	    incr flist(new,$exmh(folder)) -1
	    incr flist(newMsgs) -1
	    set flist(lastNewMsgs) $flist(newMsgs)
	    if {$flist(new,$exmh(folder)) == 0} {
		FlistUnseenFolder $exmh(folder)
	    }
	}
    }
}
proc FlistUnseenFolder { folder } {
    global flist
    unset flist(new,$folder)
    unset flist(newseq,$folder)
    Fdisp_UnHighlightUnseen $folder
    set ix [lsearch $flist(unseen) $folder]
    if {$ix >= 0} {
	set flist(unseen) [lreplace $flist(unseen) $ix $ix]
	incr flist(newFolders) -1
	if {[llength $flist(unseen)] == 0} {
	    Flag_NoUnseen
	}
    }
}
proc Flist_Update { folder {delete no} } {
    global flist
    if {$folder == {}} {
	return
    }
    Exmh_Debug Flist_Update begin $folder
    Exmh_Debug Flist_Update end [time [list FlistUpdate $folder $delete]]
}
proc FlistUpdate { folder {delete no} } {
    global mhProfile flist
    set dir $mhProfile(path)/$folder
    set newseq {}
    set hit 0
    FlistGetContext
    foreach line $flist(context) {
	set line [split $line]
	set key [lindex $line 0]
	if {$key == "atr-$mhProfile(unseen-sequence)-$mhProfile(path)/$folder:"} {
	    set seq [lindex [split $line :] 1]
	    set newseq [FlistSeq $folder $seq]
	    set hit 1
	    break
	}
    }
    if {! $hit} {
	if {[catch {open $dir/$mhProfile(mh-sequences)} in] == 0} {
	    while {[gets $in line] >= 0} {
		if [regexp unseen: $line] {
		    set seq [lindex [split $line :] 1]
		    set newseq [FlistSeq $folder $seq]
		    break
		}
	    }
	    close $in
	}
    }
    if [llength $newseq] {
	Flist_UpdateUnseen $folder $newseq
    } else {
	Fdisp_UnHighlightUnseen $folder
	set ix [lsearch $flist(unseen) $folder]
	if {$ix >= 0} {
	    set flist(unseen) [lreplace $flist(unseen) $ix $ix]
	}
    }
}

proc FlistSort { dirlist } {
    # Order the folder list according to a pattern template.
    # Patterns early in the list have higher priority.

    global mhProfile
    set patterns $mhProfile(folder-order)

    set max [llength $patterns]
    set dirlist [lsort $dirlist]
    foreach f $dirlist {
	set patLength($f) 0
    }
    foreach f $dirlist {
	set hit 0
	for {set pri 0} {$pri < $max} {incr pri} {
	    set pat [lindex $patterns $pri]
	    set patLen [string length $pat]
	    if {$patLen > $patLength($f)} {
		if [string match $pat $f] {
#    puts stderr "$f matchs $pat at pri $pri"
		    set priority($f) $pri
		    set patLength($f) $patLen
		    set hit 1
		}
	    }
	}
	if {! $hit} {
	    set priority($f) $max
	}
    }
    foreach f $dirlist {
	set hide 0
	if {$f == {}} {
	    set hide 1
	}
	foreach pat $mhProfile(folder-ignore) {
	    if [string match $pat $f] {
		set hide 1
		break
	    }
	}
	if {! $hide} {
	    lappend pset($priority($f)) $f
	}
    }
    set result ""
    for {set pri 0} {$pri <= $max} {incr pri} {
	if [info exists pset($pri)] {
	    append result $pset($pri) " "
	}
    }
    return $result
}
