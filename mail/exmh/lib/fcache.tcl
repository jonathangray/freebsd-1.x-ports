#
# fcache.tcl
#
# Folder cache display - a smaller folder display of frequently visted folders.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Fcache_Init {} {
    global fcache ;# for backwards compatibility with preferences
    global mhProfile
    set fcache(folders) {}
    Preferences_Add "Folder Cache" \
"Exmh can maintain a cache of buttons for recently used folders. Set the cache size to 0 (zero) to disable this feature.  The cache appears as a second display below the main display of folder labels.
The cache is useful if you have lots of folders or a heavily nested folder structure.  If you only have a few lines of folder labels, the cache probably just wastes space." {
	{fcache(size) fcacheSize 8 {Num cached folders}
"Exmh can maintain a cache of buttons for recently used folders.
Set the cache size to 0 (zero) to disable this feature.  The
cache appears as a second display below the main display of
folder labels.  The cache is useful if you have lots of folders
or a heavily nested folder structure.  If you only have a few
lines of folder labels, the cache probably just wastes space." }
    }
    trace variable fcache(size) w FcacheFixupSize

    # Init the cache and handle various error cases.

    if [catch {source $mhProfile(path)/.exmhfcache} msg] {
	set fcache(folders) {}
	set fcache(LRU) {}
    }
    if ![info exists fcache(enabled)] {
	set fcache(enabled) 1
    }
    set fcache(lastEnabled) $fcache(enabled)

    if ![info exists fcache(LRU)] {
	set fcache(LRU) $fcache(folders)
    }
    if {[llength $fcache(folders)] != [llength $fcache(LRU)]} {
	set fcache(LRU) $fcache(folders)
    }
    FcacheFixupSize nodisplay
}
proc Fcache_CreateWindow {} {
    global fdisp fcache
    # Create the canvas for cache display
    set fdisp(cache) [canvas $fdisp(parent).cache -bd 2 -relief raised]

    set h [expr {($fdisp(itemHeight) + $fdisp(ygap)) + $fdisp(ygap)}]
    $fdisp(cache) configure -height $h

    pack append $fdisp(parent) $fdisp(cache) {bottom expand fill}
    bind $fdisp(cache) <Configure> {Fcache_Display 1}
}
proc FcacheFixupSize { args } {
    global exwin fcache
    set fcache(enabled) [expr {$fcache(size) > 0}]
    if !$fcache(enabled) {
	if $fcache(lastEnabled) {
	    global fdisp
	    catch {
		destroy $fdisp(cache)
		unset fdisp(cache)
	    }
	}
    } else {
	if !$fcache(lastEnabled) {
	    Fcache_CreateWindow
	}
	if {[llength $fcache(folders)] > $fcache(size)}  {
	    FcacheLRU
	    if {$args != "nodisplay"} {
		after 1 {Fcache_Display 1}
	    }
	}
    }
    set fcache(lastEnabled) $fcache(enabled)
}
proc Fcache_CheckPoint {} {
    global exmh fcache mhProfile
    if [catch {open $mhProfile(path)/.exmhfcache w} out] {
	return
    }
    puts $out [list set fcache(folders) $fcache(folders)]
    puts $out [list set fcache(LRU) $fcache(LRU)]
    close $out
}

#### Cache of recently used folder labels

proc Fcache_Folder { folder } {
    # Add a folder to the set of cached ones
    global fcache exmh fdisp
    if {$folder == {} || !$fcache(enabled)} {
	return
    }
    if {$folder == $exmh(folder)} {
	set fdisp(cur,cache) $folder
    }
    if {$folder == $exmh(target)} {
	set fdisp(tar,cache) $folder
    }
    set ix [lsearch $fcache(LRU) $folder]
    if {$ix < 0} {
	Exmh_Debug Fcache_Folder $folder
	if {$fcache(folders) == {}} {
	    set fcache(folders) $folder
	    set fcache(LRU) $folder
	} else {
	    lappend fcache(folders) $folder
	    lappend fcache(LRU) $folder
	    if {[llength $fcache(folders)] > $fcache(size)} {
		FcacheLRU
	    }
	}
	Fcache_Display
    } else {
	set fcache(LRU) [lreplace $fcache(LRU) $ix $ix]
	lappend fcache(LRU) $folder
    }
}
proc FcacheLRU {} {
    global fcache
    set len [llength $fcache(folders)]
    set extra [expr {$len - $fcache(size) - 1}]
    set nukes [lrange $fcache(LRU) 0 $extra]
    set fcache(LRU) [lreplace $fcache(LRU) 0 $extra]
    foreach nuke $nukes {
	set i [lsearch $fcache(folders) $nuke]
	set fcache(folders) [lreplace $fcache(folders) $i $i]
    }
}

proc Fcache_Display { {force 0} } {
    # Layout the cache of folder buttons
    global fcache
    if {$fcache(enabled)} {
	if {($fcache(folders) != {})} {
	    Fdisp_Layout cache $fcache(folders) {} $force
	    Fdisp_HighlightCanvas cache
	}
    }
}
proc Fcache_FolderDiscard { folder } {
    # Remove a folder to the set of cached ones
    global fcache exmh fdisp
    if {$folder == {} || !$fcache(enabled)} {
	return
    }
    if {$folder == $fdisp(cur,cache)} {
	set fdisp(cur,cache) {}
    }
    if {$folder == $fdisp(tar,cache)} {
	set fdisp(tar,cache) {}
    }
    set ix [lsearch $fcache(LRU) $folder]
    if {$ix >= 0} {
	Exmh_Debug Fcache_FolderDisard $folder
	set fcache(LRU) [lreplace $fcache(LRU) $ix $ix]
	set ix [lsearch $fcache(folders) $folder]
	set fcache(folders) [lreplace $fcache(folders) $ix $ix]
	Fcache_Display
    }
}

