#
# fileselect.tcl --
# simple file selector.
#
# Mario Jorge Silva			          msilva@cs.Berkeley.EDU
# University of California Berkeley                 Ph:    +1(510)642-8248
# Computer Science Division, 571 Evans Hall         Fax:   +1(510)642-5775
# Berkeley CA 94720                                 
# 
# Layout:
#
#  file:                  +----+
#  ____________________   | OK |
#                         +----+
#
#  +------------------+    Cancel
#  | ..               |S
#  | file1            |c
#  | file2            |r
#  |                  |b
#  | filen            |a
#  |                  |r
#  +------------------+
#  currrent-directory
#
# Copyright 1993 Regents of the University of California
# Permission to use, copy, modify, and distribute this
# software and its documentation for any purpose and without
# fee is hereby granted, provided that this copyright
# notice appears in all copies.  The University of California
# makes no representations about the suitability of this
# software for any purpose.  It is provided "as is" without
# express or implied warranty.
#


# names starting with "fileselect" are reserved by this module
# no other names used.
# Hack - FSBox is defined instead of fileselect for backwards compatibility

# use the "option" command for further configuration

option add *Listbox*font \
    "-*-helvetica-medium-r-normal-*-12-*-*-*-p-*-iso8859-1" startupFile
option add *Entry*font \
    "-*-helvetica-medium-r-normal-*-12-*-*-*-p-*-iso8859-1" startupFile
option add *Label*font \
    "-*-helvetica-medium-r-normal-*-12-*-*-*-p-*-iso8859-1" startupFile


# this is the proc that creates the file selector box
# purpose - comment string
# defaultName - initial value for name
# cmd - command to eval upon OK
# errorHandler - command to eval upon Cancel
# If neither cmd or errorHandler are specified, the return value
# of the FSBox procedure is the selected file name.

proc FSBox {{purpose "Select file:"} {defaultName ""} {cmd ""} {errorHandler ""}} {
    global fileselect
    set w .fileSelect
    if [Exwin_Toplevel $w "Select File"] {
	# path independent names for the widgets
    
	set fileselect(list) $w.file.sframe.list
	set fileselect(scroll) $w.file.sframe.scroll
	set fileselect(direntry) $w.file.f1.direntry
	set fileselect(entry) $w.file.f2.entry
	set fileselect(ok) $w.but.ok
	set fileselect(cancel) $w.but.cancel
	set fileselect(msg) $w.label
	set fileselect(text) $purpose
    
	set fileselect(result) ""	;# value to return if no callback procedures
    
	# widgets
	Widget_Label $w label {top fill pady 10 padx 20 expand} -anchor w -width 24
	Widget_Frame $w file Dialog {left filly} -bd 10 -background white

	Widget_Frame $w.file f1
	Widget_Label $w.file.f1 label {left} -text "Dir"
	Widget_Entry $w.file.f1 direntry {right fill expand} \
	    -relief sunken -width 30
    
	Widget_Frame $w.file sframe
	scrollbar $w.file.sframe.yscroll -relief sunken \
	     -command [list $w.file.sframe.list yview]
	listbox $w.file.sframe.list -relief sunken \
	    -yscroll [list $w.file.sframe.yscroll set]
	pack append $w.file.sframe \
	    $w.file.sframe.yscroll {right filly} \
	    $w.file.sframe.list {left expand fill} 

	Widget_Frame $w.file f2
	Widget_Label $w.file.f2 label {left} -text Name
	Widget_Entry $w.file.f2 entry {right fill expand} -relief sunken
    
	# buttons
	$w.but.quit configure -text Cancel \
	    -command [list fileselect.cancel.cmd $w]

	Widget_AddBut $w.but ok OK \
	    [list fileselect.ok.cmd $w $cmd $errorHandler] {left padx 1}

	Widget_AddBut $w.but list List \
	    [list fileselect.list.cmd $w] {left padx 1}    
    }
    $fileselect(msg) configure -text $purpose
    $fileselect(entry) delete 0 end
    $fileselect(entry) insert 0 [file tail $defaultName]

    if {[info exists fileselect(lastDir)] && ![string length $defaultName]} {
	set dir $fileselect(lastDir)
    } else {
	set dir [file dirname $defaultName]
    }
    set fileselect(pwd) [pwd]
    cd $dir
    $fileselect(direntry) delete 0 end
    $fileselect(direntry) insert 0 [pwd]

    $fileselect(list) delete 0 end
    $fileselect(list) insert 0 "Directory Listing"

    # Set up bindings for the browser.
    foreach ww [list $w $fileselect(entry)] {
	bind $ww <Return> [list $fileselect(ok) invoke]
	bind $ww <Control-c> [list $fileselect(cancel) invoke]
    }
    bind $fileselect(direntry) <Return> [list fileselect.list.cmd %W]

    tk_listboxSingleSelect $fileselect(list)


    bind $fileselect(list) <Button-1> {
        # puts stderr "button 1 release"
        %W select from [%W nearest %y]
	$fileselect(entry) delete 0 end
	$fileselect(entry) insert 0 [%W get [%W nearest %y]]
    }

    bind $fileselect(list) <Key> {
        %W select from [%W nearest %y]
        $fileselect(entry) delete 0 end
	$fileselect(entry) insert 0 [%W get [%W nearest %y]]
    }

    bind $fileselect(list) <Double-ButtonPress-1> {
        # puts stderr "double button 1"
        %W select from [%W nearest %y]
	$fileselect(entry) delete 0 end
	$fileselect(entry) insert 0 [%W get [%W nearest %y]]
	$fileselect(ok) invoke
    }

    bind $fileselect(list) <Return> {
        %W select from [%W nearest %y]
	$fileselect(entry) delete 0 end
	$fileselect(entry) insert 0 [%W get [%W nearest %y]]
	$fileselect(ok) invoke
    }

    # set kbd focus to entry widget

    focus $fileselect(entry)

    # Wait for button hits if no callbacks are defined

    if {"$cmd" == "" && "$errorHandler" == ""} {
	# wait for the box to be destroyed
	update idletask
	grab $w
	tkwait variable fileselect(result)
	grab release $w

	set path $fileselect(result)
	set fileselect(lastDir) [pwd]
	cd $fileselect(pwd)
	return [string trimright [string trim $path] /]
    }
    cd $fileselect(pwd)
    return ""
}


# auxiliary button procedures

proc fileselect.yck { {tag {}} } {
    global fileselect
    $fileselect(msg) configure -text "Yck! $tag"
}
proc fileselect.ok {} {
    global fileselect
    $fileselect(msg) configure -text $fileselect(text)
}

proc fileselect.cancel.cmd {w} {
    global fileselect
    set fileselect(result) {}
    Exwin_Dismiss $w
}

proc fileselect.list.cmd {w} {
    global fileselect
    set seldir [$fileselect(direntry) get]
    if {[catch {glob $seldir} dir]} {
	fileselect.yck "glob failed"
	return
    }
    if {[llength $dir] > 1} {
	set dir [file dirname $seldir]
	set pat [file tail $seldir]
    } else {
	set pat *
    }
    fileselect.ok
    update idletasks
    if [file isdirectory $dir] {
	fileselect.getfiles $dir $pat
	focus $fileselect(entry)
    } else {
	fileselect.yck "not a dir"
    }
}

proc fileselect.ok.cmd {w cmd errorHandler} {
    global fileselect
    set selname [$fileselect(entry) get]
    set seldir [$fileselect(direntry) get]

    if [string match /* $selname] {
	set selected $selname
    } else {
	if [string match ~* $selname] {
	    set selected $selname
	} else {
	    set selected $seldir/$selname
	}
    }

    # some nasty file names may cause "file isdirectory" to return an error
    if [catch {file isdirectory $selected} isdir] {
	fileselect.yck "isdirectory failed"
	return
    }
    if [catch {glob $selected} globlist] {
	if ![file isdirectory [file dirname $selected]] {
	    fileselect.yck "bad pathname"
	    return
	}
	set globlist $selected
    }
    fileselect.ok
    update idletasks

    if {[llength $globlist] > 1} {
	set dir [file dirname $selected]
	set pat [file tail $selected]
	fileselect.getfiles $dir $pat
	return
    } else {
	set selected $globlist
    }
    if [file isdirectory $selected] {
	fileselect.getfiles $selected
	$fileselect(entry) delete 0 end
	return
    }

    if {$cmd != {}} {
	$cmd $selected
    } else {
	set fileselect(result) $selected
    }
    Exwin_Dismiss $w
}

proc fileselect.getfiles { dir {pat *} } {
    global fileselect
    $fileselect(msg) configure -text Listing...
    update idletasks
    $fileselect(direntry) delete 0 end
    cd $dir
    $fileselect(direntry) insert 0 [pwd]
    set files [lsort [glob -nocomplain $pat]]

    # build a reordered list of the files: directories are displayed first
    # and marked with a trailing "/"
    fileselect.putfiles $files [expr {($pat == "*") ? 1 : 0}]
    fileselect.ok
}

proc fileselect.putfiles {files {dotdot 0} } {
    global fileselect

    $fileselect(list) delete 0 end
    if {$dotdot} {
	$fileselect(list) insert end "../"
    }
    foreach i $files {
        if {[file isdirectory $i]} {
	    $fileselect(list) insert end $i/
	} else {
	    $fileselect(list) insert end $i
	}
    }
}

