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

# Written by David Marwood


# ----------------------------------------------------------------
# Functions in the "File" menu: remove, commit, add, edit, show log.
# ----------------------------------------------------------------

# Checks which file is selected in .files.list and removes it then updates that status.
proc RemoveFile {} {
    # Get the selection into $Sel.
    set Sel [GetSel .files.list]
    if {$Sel == -1} {
	ModalInfoBox "Please select a file before pressing Remove."
	return
    }

    # Confirm removal
    global ExpertMode
    set File [.files.list get $Sel]
    if {$ExpertMode == "No"} {
	set Button [ModalButtonBox "You are about remove \"$File\" and schedule it for removal from the repository.  Are you sure you want to do this?" "OK Cancel"]
	if {$Button == 1} return;
    }

    # Remove from disk
    if {[file exists $File]} {exec rm $File}

    # Remove from CVS
    global CVSResults
    set CVSResults "[exec cvs remove $File]"

    .lastaction configure -text "$File has been locally removed."
    update

    busy UpdateStatus
}


# Commits the file selected in .files.list.  If no selection, calls CommitAll.  Updates the status.
proc CommitFile {} {
    # Get selection into $Sel.
    set Sel [GetSel .files.list]
    if {$Sel == -1} {
	CommitAll
	return
    }

    set File [.files.list get $Sel]

    # Confirm commit.
    global ExpertMode
    if {$ExpertMode == "No"} {
	if {[ModalButtonBox "Any changes you have made to $File will be merged with the central repository.  Are you sure you want to do this?" "Yes Cancel"] == 1} return
	update
    }

    # If necessary, show diff.  
    global ShowDiffOnCommit
    if {$ShowDiffOnCommit == "Yes"} {
	ShowDiff
	# The sleep ensures the diff process gets a chance to start.
	sleep 1
	update
    }

    # Do "cvs commit"
    global CVSResults
    set CVSResults [busy [list exec cvs commit $File]]

    .lastaction configure -text "The changes you have made to $File have been placed in the repository."
    update

    busy UpdateStatus
}


# 
# Adds a new file to the repository.  Prompts with a file selection
# box.  If the file doesn't exist, it is created.  Strips and discards
# to directory returned by the file selection box, so the user had
# better not change directories in the FSBox.
# 
proc AddFile {} {
    # Create FSBox.  Set NoPathFile to the file name selected.
    set File [FSBox "Select file to add:"]
    if {$File == ""} return
    set NoPathFile [split $File "/"]
    set NoPathFile [lindex $NoPathFile [expr [llength $NoPathFile]-1]]
    if {[string compare $NoPathFile ""] == 0} return

    # Ensure file isn't already in module.
    if {[lsearch [GetFilesInModule] $NoPathFile] >= 0} {
	ModalInfoBox "$NoPathFile is already in the module and cannot be added."
	return
    }

    # Create file if necessary.
    if {[file exists $NoPathFile] == 0} {close [open $NoPathFile w]}

    # Do "cvs add"
    global CVSResults
    set CVSResults [exec cvs add $NoPathFile]

    .lastaction configure -text "$NoPathFile has been locally added."
    update

    busy UpdateStatus
}


# Brings up the default editor for the selected file.
proc EditFile {} {
    # Set $File to the name of the selected file.
    set Sel [GetSel .files.list]
    if {$Sel == -1} {
	ModalInfoBox "Please select a file before selecting Edit."
	return
    }
    set File [.files.list get $Sel]

    # Exec the editor.
    global EditorCommand
    eval [concat exec $EditorCommand $File &]

    busy UpdateStatus
}


proc ShowLog {} {
    # Set $File to the name of the selected file.
    set Sel [GetSel .files.list]
    if {$Sel == -1} {
	ModalInfoBox "Please select a file before selecting Show Log."
	return
    }
    set File [.files.list get $Sel]

    global tk_library
    catch {exec wish -f $tk_library/cvs_log.tcl $File &}
    return
}


proc ShowDiff {} {
    # Set $File to the name of the selected file.
    set Sel [GetSel .files.list]
    if {$Sel == -1} {
	ModalInfoBox "Please select a file before selecting Show Diff."
	return
    }
    set File [.files.list get $Sel]

    global tk_library
    catch {exec wish -f $tk_library/cvs_diff.tcl $File &}
    return 
}


# ----------------------------------------------------------------
# Functions in the Modules menu: commit all, update, release, edit modules file
# ----------------------------------------------------------------

# Commits all files to to repository.  Updates status.
proc CommitAll {} {
    # Confirm commit.
    global ExpertMode
    if {$ExpertMode == "No" } {
	if {[ModalButtonBox "Changes you have made to any of the files in the module are about to be recorded in the repository.  Are you sure you want to do this?" "Yes Cancel"] == 1} return
    }

    # Call "cvs commit"
    global CVSResults
    set CVSResults [busy {exec cvs commit}]

    .lastaction configure -text "The changes you have made have been recorded in the repository.  If any files were added or removed, then the modules file should be updated to reflect the changes before continuing."
    update

    busy UpdateStatus
}


# Does a "cvs update" and updates the status.
proc UpdateFiles {} {
    global CVSResult
    set CVSResults [exec cvs update]
    busy UpdateStatus
}


# Releases the module and deletes the checked out copy.
proc ReleaseFiles {} {
    global ExpertMode
    global Module
    if {$ExpertMode == "No"} {
	if {[ModalButtonBox "You are about to remove the checked out copy of module $Module.  Local modifications, additions and removals that have not been commited will be lost.  Are you sure you want to do this?" "OK Cancel"] == 1} return
	update
    }

    global ModuleParentDir
    cd $ModuleParentDir
    exec echo y | cvs release -d $Module
    Quit
}


proc Quit {} {
    foreach Process [winfo interps] {
	if {[regexp "cvs_diff.tcl.*" $Process] || [regexp "cvs_log.tcl.*" $Process]} {
	    catch {send $Process Quit}
	}
    }
    exit 0
}


# Checks out the modules module, edits the modules file, and checks it back in.
proc EditModules {} {
    exec cvs co modules

    if {[file exists modules/modules] == 0} {puts stdout "File doesn't exist."}

    global EditorCommand
    set env(PWD) [pwd]
    eval [concat exec $EditorCommand modules/modules]
    global CVSResults
    set CVSResults [exec cvs commit modules/modules]
    exec echo y | cvs rel -d modules
}


# ----------------------------------------------------------------
# Functions in the help menu: about, overview
# ----------------------------------------------------------------

# "help about" menu option.  Displays program name.
proc HelpAbout {} {
    if {[winfo exists .about] == 1} { return "" }
    toplevel .about
    label .about.icon -fg hotpink
    message .about.title -text "GIC\nThe Graphical Interface to CVS" -font -Adobe-times-bold-r-normal--*-240* -justify center -aspect 10000
    message .about.version -text "Version 1.1b3\nAug 25, 1993" -width 200 -justify center
    message .about.author -text "By David Marwood\n<marwood@cpsc.ucalgary.ca>" -width 300 -justify center
    button .about.ok -text "Close" -command "destroy .about"
    pack append .about .about.icon left .about.title top .about.version top .about.author top .about.ok bottom
}


# Pops up window giving overview of CVS.
proc HelpOverview {} {
    if {[winfo exists .overview] == 1} {return}
    toplevel .overview
    message .overview.mess -text "GIC is a graphical interface to the Concurrent Version System (CVS).  It allows a user of CVS to perform the common CVS functions without having to use the sometimes cumbersome CVS command line options." -aspect 300
    button .overview.close -text "Close" -command "destroy .overview"
    pack append .overview .overview.mess {top fill} .overview.close {top fill expand}
}


# ----------------------------------------------------------------
# Leftover buttons that weren't placed in menus: update status, clear output
# ----------------------------------------------------------------

# Exec's "cvs status" to see the status of each file and refreshes .files.list and .status.list.
proc UpdateStatus {} {
    if {[GetSel .files.list] == -1} {set SelText ""
    } else {set SelText [.files.list get [GetSel .files.list]]}

    .files.list delete 0 end
    .status.list delete 0 end
    set Stats [exec cvs -q status -l | awk {$3 ~ /Status:/ {printf("%s %s %s %s %s\n", $2, $4, $5, $6, $7);}}]
    set Stats [split $Stats "\n"]

    set FID [open "CVS/Repository" "r"]
    if {$FID <= 0} {
	ModalInfoBox "Unable to open CVS/Repository file.  Exitting."
	exit 1
    }
    set DirPrefix [gets $FID]
    close $FID

    foreach Stat $Stats {
	if {[llength $Stat] >= 2} {
	    .files.list insert end [lindex $Stat 0]
	    if {$SelText != "" && [lindex $Stat 0] == $SelText} {
		.files.list select from end
	    }
	    .status.list insert end [lreplace $Stat 0 0]
	}
    }

    global MaxFilesListHeight

#    global Module
#    set Users [exec cvs history -o -a | awk "\{if (\$6 == \"=$Module=\") printf(\"%s \", \$4);\}"]
#    .users configure -text "Users: $Users"
}


proc UpdateDirs {} {
    global ModuleParentDir
    global Module

    .dirs.list delete 0 end
    foreach Dir [lsort [glob -nocomplain * .*]] {
	if {[file exists $Dir/CVS/Repository] != 0 && 
	    ("$ModuleParentDir/$Module" != [pwd] || $Dir != "..") &&
	    $Dir != "."} {
	    .dirs.list insert end $Dir
	}
    }
    global MaxFilesListHeight
}


# change CWD and update files, cwd label and dirs list
proc changeCWD {} {
    DoChangeCWD [.dirs.list get [GetSel .dirs.list]]
    busy UpdateDirs
    busy UpdateStatus
}


proc DoChangeCWD cwd {
    cd $cwd
    global ModuleParentDir
    set RelDir ""
    regsub $ModuleParentDir/ [pwd] "" RelDir
    .cwd configure -text $RelDir
}


# ----------------------------------------------------------------
# Utility functions used throughout the program.
# ----------------------------------------------------------------

# 
# Returns the index of the first selection of the listbox named by
# $Obj.  Returns -1 if no selection.
# 
# eg. GetSel .files.list returns the first selected file in
# .files.list.
# 
proc GetSel {Obj} {
    set Sels [eval $Obj curselection]
    if {[string compare $Sels ""] == 0} {
	return -1
    }
    return [lindex $Sels 0]
}


# 
# Returns a list of files in the current module according to the
# CVS/Entries file.
# 
proc GetFilesInModule {} {
    global env
    set Ans [exec awk {{FS="/"; print $2;}} CVS/Entries]
    return [split $Ans "\n"]
}


proc min {x y} {
    if {$x > $y} {return $y} else {return $x}
}

#
# Procedure to pop up an error message.  Thanks goes to Michael
# Pilawa for this procedure.  Works great and insulates the user from
# those huge annoying error dumps.
# 
proc tkerror {message} {
    catch {destroy .d}
    toplevel .d -class Dialog
    set wintitle [concat "ERROR in" [winfo name .]]
    wm iconname .d $wintitle
    wm title    .d $wintitle
    frame .d.errframe -relief raised -border 1
    pack append .d.errframe \
	[message .d.errframe.mssg    -text $message \
        -anchor center      -justify center -width 400 \
        -background yellow2 -foreground red -relief raised ]  {top fill} \
	[button  .d.errframe.ok      -text "Press this button to continue." \
        -background yellow2 \
        -command { destroy .d } ] { top fill }
    pack append  .d  .d.errframe  {top expand fill frame center}
#    configcolor  .d.errframe.ok
    focus .d
    bind .d <Return>  { destroy .d }
}


# 
# Sets all cursors to busy, executes command, and restores cursors.
# 
proc busy {cmds} {
    global errorInfo

    set busy {.app}
    set list [winfo children .]
    while {$list != ""} {
	set next {}
	foreach w $list {
	    set cursor [lindex [$w config -cursor] 4]
	    if {[winfo toplevel $w] == $w || $cursor != ""} {
		lappend busy [list $w $cursor]
	    } else {
		lappend busy [list $w {}]
	    }
	    set next [concat $next [winfo children $w]]
	}
	set list $next
    }

    foreach w $busy {
	catch {[lindex $w 0] config -cursor watch}
    }

    update idletasks

    set error [catch {uplevel eval $cmds} result]
    set ei $errorInfo

    foreach w $busy {
	catch {[lindex $w 0] config -cursor [lindex $w 1]}
    }

    if $error {
	error $result $ei
    } else {
	return $result
    }
}


proc nop {} {}


# ----------------------------------------------------------------
# Screen management functions
# ----------------------------------------------------------------

# 
# Managing the files and status listbox and scroller.  Keep the list
# boxes in .files.list and .status.list syncronized when scrolling.
# 
proc scroll_bars {index} {
	.files.list yview $index  
	.status.list yview $index 
}


proc slave_scroll { win_size  win_vis  vis_top  vis_bottom } {
	.status.list yview $vis_top 
	.files.scroll set $win_size  $win_vis  $vis_top $vis_bottom
}

# ----------------------------------------------------------------
# Set up the screen parts
# ----------------------------------------------------------------

# 
# Create the menus .menu
# 

frame .menu -relief raised -borderwidth 1

# The Item menu
menubutton .menu.item -text "Item" -menu .menu.item.m -underline 7
menu .menu.item.m
.menu.item.m add command -label "Add" -command "AddFile"
.menu.item.m add command -label "Remove" -command "RemoveFile"
.menu.item.m add command -label "Edit" -command "EditFile"
.menu.item.m add command -label "Commit" -command "CommitFile"
.menu.item.m add command -label "Show Log" -command "ShowLog"
.menu.item.m add command -label "Show Diff" -command "ShowDiff"

# The Module menu
menubutton .menu.module -text "Module" -menu .menu.module.m -underline 7
menu .menu.module.m
.menu.module.m add command -label "Commit All" -command "CommitAll"
.menu.module.m add command -label "Update" -command "UpdateFiles"
.menu.module.m add command -label "Edit modules file" -command "EditModules"
.menu.module.m add command -label "Release (and quit)" -command "ReleaseFiles"
.menu.module.m add command -label "Quit (no release)" -command "Quit"

# The Help menu
menubutton .menu.help -text "Help" -menu .menu.help.m -underline 7
menu .menu.help.m
.menu.help.m add command -label "About" -command HelpAbout
.menu.help.m add command -label "Overview" -command HelpOverview

pack append .menu .menu.module left .menu.item left .menu.help right
pack append . .menu {top fillx}


# 
# Set up handy info at top.
# 

# .modulename is the module name.  .youare is your name.
label .youare -text "You are: [exec whoami]"
pack append . .youare {top fillx}
# label .parentdir -textvariable ModuleParentDir
# pack append . .parentdir {top fillx}


# 
# Create output panels at bottom
# 

# Create the "Information on Last Action" panel.
label .lastactiontitle -text "Last Action"
message .lastaction -relief sunken -aspect 500

# Create CVSResults .cvsresults and title .cvsresultstitle
set CVSResults ""
label .cvsresultstitle -text "CVS Output"
message .cvsresults -relief sunken -textvariable CVSResults -aspect 1000

pack append . .cvsresults {bottom fillx} .cvsresultstitle {bottom fillx} \
    .lastaction {bottom fillx} .lastactiontitle {bottom fillx} 


# 
# Create the buttons .buttons
# 

frame .buttons
button .buttons.updatestatus -text "Update status now" -command {busy UpdateStatus}
button .buttons.clear -text "Clear output" -command {.lastaction configure -text ""; set CVSResults ""}
pack append .buttons .buttons.updatestatus {right fillx} .buttons.clear {right fillx}
pack append . .buttons {bottom fillx}


# 
# Create subdirectories list and title.
# 

frame .dirs
label .dirstitle -text "CVS Controlled Sub-directories of" -height 3 -anchor s
label .cwd 
scrollbar .dirs.scroll -relief sunken -command ".dirs.list yview"
listbox .dirs.list -relief sunken -yscroll ".dirs.scroll set" -geometry 30x4

pack append .dirs \
      .dirs.list {left fill expand} \
      .dirs.scroll {left filly}
tk_listboxSingleSelect .dirs.list
bind .dirs.list <Double-1> "changeCWD"
pack append . .dirs {bottom fill expand} .cwd {bottom fillx} .dirstitle {bottom fillx}


# 
# Create files list, .files and .status
# 

# Create the files frame .files.  Contains a listbox of files
# and a scroller.  Allow only one selection.
frame .files
label .files.title -text "Files"
listbox .files.list -relief sunken -yscroll "slave_scroll" -setgrid 1
scrollbar .files.scroll -relief sunken -command "scroll_bars"
pack append .files .files.title {top fillx} .files.list {left fill expand} .files.scroll {right fill}
catch {tk_listboxSingleSelect .files.list}

# Create the status frame .status.  Prevent any selection/scrolling.
pack append [frame .status] [label .status.titles -text "Status"] {top fillx} [listbox .status.list -relief sunken] {top fill expand}
bind .status.list <Any-ButtonPress-1> {nop}
bind .status.list <ButtonPress-1> {nop}
bind .status.list <B1-Motion> {nop}
bind .status.list <B1-Any-Motion> {nop}

frame .frame_status
pack append .frame_status .files {left fill expand} .status {left fill}
pack append . .frame_status {bottom fill expand}


# ----------------------------------------------------------------
# Ensure all external parts exist - files, environ variables
# ----------------------------------------------------------------

# Make sure this is a CVS directory.
if {[file isdirectory CVS] == 0} {
    update
    ModalInfoBox "GIC may only be run from within a directory containing a CVS subdirectory.  Also, the name of the directory must the same as the name of the module."
    exit
}

# Ensure CVSROOT is set to an existing directory.
if {[info exists env(CVSROOT)] == 0 || 
    [file isdirectory $env(CVSROOT)/CVSROOT] == 0 || 
    [file readable $env(CVSROOT)/CVSROOT] == 0} {
    update
    ModalInfoBox "The CVSROOT environment variable must exist and be set to a readable CVS repository directory.  To create a CVS repository, set the CVSROOT variable to the directory in which to place the repository and run the cvs program \"cvsinit\" from the top of the CVS source tree."
    exit
}

# Make sure CVS/Repository and $CVSROOT (less the possibly trailing
# slash) are the same.
if {[string match [string range $env(CVSROOT) 0 [string length $env(CVSROOT)]]* [exec cat CVS/Repository]] == 0} {
    update
    ModalInfoBox "The CVSROOT environment variable does not match the respository specified in the CVS/Repository file.  Please correct this problem."
    exit
}

# Ensure parselog.nawk exists
if {[file exists $tk_library/parselog.nawk] == 0} {
    update
    ModalInfoBox "GIC has been incorrectly installed.  The file 'parselog.nawk' does not exist in the tk_library directory, $tk_library.  Please copy it there and re-start GIC."
    exit
}

# A dumb hack to work around the inconsistency between $env(PWD) and [pwd].
set env(PWD) [pwd]


# ----------------------------------------------------------------
# Initialize the variables
# ----------------------------------------------------------------

# Init "Module" - the name of the module, based on the pwd.
# Init "ModuleParentDir" - then name of the parent directory of the module
set Parts [split [pwd] "/"]
set ModuleParentDir [join [lrange $Parts 0 [expr [llength $Parts]-2]] "/"]
set Module [lindex $Parts [expr [llength $Parts]-1]]
wm title . "GIC - $Module"

wm protocol . WM_DELETE_WINDOW "Quit"

# Correctly set .modulename.
# .modulename configure -text "Module: $Module" -font 8x13bold

# If ShowDiffOnCommit is not 0, then the diff is displayed when the file is commited.
set ShowDiffOnCommit 1

# Set defaults and read .gicrc.
set ExpertMode "No"

if {[info exists env(EDITOR)] == 0} {
    set EditorCommand "xterm -e vi"
} else {
    set EditorCommand $env(EDITOR)
}

set MaxFilesListHeight 30

catch {source ~/.gicrc}


# ----------------------------------------------------------------
# Initialize the screen contents
# ----------------------------------------------------------------

# Initialize the cwd.
DoChangeCWD .

# Init .files.list and .status.list
busy UpdateStatus
busy UpdateDirs
