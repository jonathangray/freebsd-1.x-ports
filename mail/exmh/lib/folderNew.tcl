# folderNew.tcl
#
# New folder dialog
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc FolderDialogInner {path label cancelProc okProc message} {
    global mhProfile exwin exmh

    if [Exwin_Toplevel $path $label FolderDialog nomenu] {
	set t $path
	wm transient $t
    
	Widget_Message $t msg -aspect 400 -text $message
	set f [Widget_Frame $t name Rim]
	$f configure -bd 10
	set f [Widget_Frame $f rim Rim]
	$f configure -bd 2 -relief raised
	Widget_Label $f label {left} -text "Folder Name:"
	set e [Widget_Entry $f name {right fill} -bg white -relief raised]
	bind $e <Return> [list $okProc $path $e]
	focus $e
    
	set f [Widget_Frame $t but Rim]
	$f configure -bd 10
	Widget_AddBut $f cancel "Cancel" [list $cancelProc $path] left
	Widget_AddBut $f ok "OK" [list $okProc $path $e] right
    }
}

proc Folder_New {} {
    global mhProfile exmh

    FolderDialogInner .newf "Create Folder" Exwin_Dismiss FolderNewCommit \
"Creating a new folder results in a new directory under $mhProfile(path).
Nested folders are allowed.
You need NOT put a + after the folder name.
@ is shorthand for the current folder, so
@foo => $exmh(folder)/foo"
}

proc FolderNewCommit { top entry } {
    global mhProfile exmh
    set name [$entry get]
    if [regexp { } $name] {
	Exmh_Status "NO SPACES in folder names" warn
	return
    }
    if [regexp {@(.*)} $name all newname] {
	set name $exmh(folder)
	if {[string length $newname] > 0} {
	    set name $name/$newname
	}
    }
    set components [split $name /]
    set path $mhProfile(path)
    foreach comp $components {
	append path /$comp
	if [file isdirectory $path] {
	    continue
	}
	if [file exists $path] {
	    Exmh_Status "Non-directory $path already exists" red
	    Exwin_Dismiss $top
	    return
	}
	if [catch {exec mkdir $path} msg] {
	    Exmh_Status "mkdir $path: $msg"
	    Exwin_Dismiss $top
	    return
	}
	if [info exists mhProfile(folder-protect)] {
	    exec chmod $mhProfile(folder-protect) $path
	}
    }
    Exmh_Status "Created folder $name" blue
    Flist_AddFolder $name
    Exwin_Dismiss $top
    return
}

proc Folder_Delete {} {
    global mhProfile exmh

    FolderDialogInner .delf "Delete Folder" Exwin_Dismiss FolderDelCommit \
"Only folders with no messages can be deleted.
You need NOT put a + after the folder name.
@ is shorthand for the current folder, so
@foo => $exmh(folder)/foo"
}

proc FolderDelCommit { top entry } {
    global mhProfile exmh
    set name [$entry get]
    if [regexp {@(.*)} $name all newname] {
	set name $exmh(folder)
	if {[string length $newname] > 0} {
	    set name $name/$newname
	}
    }
    set path $mhProfile(path)
    if [catch {Mh_Path $name new} nextid] {
	Exmh_Status "Cleaning up folder $name" blue
	Flist_DelFolder $name
	Fcache_FolderDiscard $name
	Exwin_Dismiss $top
	return
    }
    if {[file tail $nextid] == 1} {
	catch {exec rm -f $path/$name/.xmhcache}
	catch {
	    foreach nfsjunk [glob $path/$name/.nfs*] {
		exec rm -f $nfsjunk
	    }
	}
	if [catch {exec rmf +$name -nointeractive} err] {
	    Exmh_Status $err
	} else {
	    Exmh_Status "Deleted folder $name" blue
	    Flist_DelFolder $name
	    Fcache_FolderDiscard $name
	}
    } else {
	Exmh_Status "Still messages in +$name"
    }
    Exwin_Dismiss $top
    return
}

