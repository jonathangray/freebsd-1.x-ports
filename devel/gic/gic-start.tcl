#
#  This file is part of GIC - the Graphical Interface to CVS
#
#  (c) Copyright 1992 Department of Computer Science, University of
#      Calgary, Calgary, Alberta, Canada.  All rights reserved.
#
#  Permission to use, copy, modify, and distribute this software and its
#  documentation for any purpose and without fee is hereby granted, provided
#  that the above copyright notice appears in all copies.  The University
#  of Calgary makes no representations about the suitability of this
#  software for any purpose.  It is provided "as is" without express or
#  implied warranty.
#


proc GetModulesList {} {
    global env
    return [lsort [exec awk {{if (substr($0, 0, 1) != "#") printf("%s ", $1);}} $env(CVSROOT)/CVSROOT/modules]]
}


proc UpdateModuleList {} {
    .mod.list delete 0 end
    foreach Mod [GetModulesList] {
	.mod.list insert end $Mod
    }
}


proc StartGic {} {
    if {[string compare [.mod.list curselection] ""] == 0} return

    global tk_library
    if {[file exists $tk_library/gic.tcl] == 0} {
	ModalInfoBox "Unable to find gic.tcl in the tk_library directory.  Please copy gic.tcl to $tk_library."
	return
    }

    set Sel [.mod.list get [.mod.list curselection]]
    if {[file exists $Sel] == 0} {
	toplevel .top
	wm title .top Info
	pack append .top [message .top.mess -text "Checking out $Sel." -aspect 400] {top fill}
	update
	exec cvs co $Sel
    }
    cd $Sel
    set env(PWD) [pwd]
    exec wish -f $tk_library/gic.tcl &
    destroy .
}


# Checks out the modules module, edits the modules file, and checks it back in.
proc EditModules {} {
    exec cvs co modules

    global EditorCommand
    if {[file exists modules/modules] == 0} {puts stdout "File doesn't exist."}
    set env(PWD) [pwd]
    eval [concat exec $EditorCommand modules/modules]
    global CVSResults
    set CVSResults [exec cvs commit modules/modules]
    exec echo y | cvs rel -d modules
    UpdateModuleList
}


wm title . "GIC Starter"

pack append [frame .mod] \
    [label     .mod.label -text "Available Modules"] {top fill} \
    [scrollbar .mod.scroll -relief sunken  \
               -command ".mod.list yview"] {right fill} \
    [listbox   .mod.list -yscroll ".mod.scroll set" \
               -relief sunken -setgrid 1] {left expand fill}
button .ok -text "Open Module" -command "StartGic"
button .quit -text "Quit" -command "destroy ."
button .editmodules -text "Edit modules file" -command EditModules
catch {tk_listboxSingleSelect .mod.list}
bind .mod.list <Double-1> {StartGic}
UpdateModuleList

pack append . .mod {top expand fill} .quit right .ok right .editmodules {right expand fill}

# Set EDITOR if it doesn't exist.
if {[info exists env(EDITOR)] == 0} {
    set EditorCommand "xterm -e vi"
} else {
    set EditorCommand $env(EDITOR)
}
