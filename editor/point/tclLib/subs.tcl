
proc pr {s} {puts stdout $s}

# The purpose of InsertSelectedString is to prevent too many interpretations
# of strings that are gotten from the selection (and other places)
proc InsertSelectedString {} {
	InsertString [selection get]
}

proc pt_listboxSingleSelect {button w} {
	bind $w <B$button-Motion> {%W select from [%W nearest %y]} 
	bind $w <Shift-$button> {%W select from [%W nearest %y]}
	bind $w <Shift-B$button-Motion> {%W select from [%W nearest %y]}
}

proc GetSelectedFiles {} {
	set listbox [selection own]
	set idlist [$listbox curselection]
	set filelist ""
	foreach id $idlist {
		lappend filelist [$listbox get $id]
	}
	return $filelist
}

proc DeleteSelectedFiles {} {
	set rmlist [GetSelectedFiles]
	if {$rmlist==""} {
		MessageLine "No selected files to delete"
		return
	}
	catch "exec rm -f $rmlist"
	Option set filePattern "*"
}

proc OpenSelectedFiles {} {
	set openlist [GetSelectedFiles]
	if {$openlist==""} {
		MessageLine "No selected files to open"
		return
	}
	foreach name $openlist {
		OpenMultipleFile $name 1
	}
}

proc OpenMultipleFile {name button} {
	global location1 location2 location3
	# check for a file size at the end of the name
	set pat "\{(.*) \(\[0-9\]+k\)\}$"
	if [regexp "$pat" "$name" junk fname] {
		# and remove the file size if it is found
		set name $fname
	}
	if [file isdirectory $name] then {
		MessageLine "$name is a directory"
	} else {
		OpenWindow $name "[set location$button]"
	}
}

proc FindUnusedName {prefix} {
	set n 1
	while 1 {
		set name [format "%s.%d" $prefix $n]
		if ![file exists $name] { break }
		incr n
	}
	return $name
}

proc UsersShell {} {
	global env
	if [info exists env(SHELL)] {
		return $env(SHELL)
	} else {
		return csh
	}
}

proc RunLatexOnSelection {} {
	# capture the selection
	set sel [selection get]
	# create the latex input file
	set latex_name [FindUnusedName latex]
	set fid [open $latex_name w]
	puts $fid "\\input{latex.header}"
	puts $fid $sel
	puts $fid "\\input{latex.trailer}"
	close $fid
	# create the file with a single newline in it
	set window_name [FindUnusedName window]
	set fid [open $window_name w]
	puts $fid \n
	close $fid
	global location1
	set w [OpenWindow $window_name $location1]
	Sel set 0 0 $w char
	set pid [eval ConnectToPty latex $latex_name]
pr "Wait for process $pid to complete"
	WaitForProcess $pid
pr "Process $pid has completed"
	exec xdvi -geometry 900x985+0+0 latex.dvi
}

proc RunProgramInFile {delsel} {
	# get the selection which is the program call
	set prog [selection get]
	if $delsel {
		DeleteToScrap
	} else {
		# move the selection to after the program call
		set sel [Sel get]
		set end [expr 1+[lindex $sel 1]]
		Sel set $end $end
	}
	# insert a newline and select it
	InsertString \n
	# eval is necessary to break apart the command name
	# and the arguments in the string $prog
	eval ConnectToPty $prog
}

proc RunProgramInWindow {prog} {
	global location1
	set name [FindUnusedName csh]
	# create the file with a single newline in it
	set fid [open $name w]
	puts $fid \n
	close $fid
	set w [OpenWindow $name $location1]
	Sel set 0 0 $w char
	eval ConnectToPty $prog
}

proc RemakeTextMenus {} {
	global TextMenuSpec
	foreach w [GetTextWindowList] {
		pack unpack $w.menu
		destroy $w.menu
		MakeMenubar $w.menu $TextMenuSpec
		TextMenuBindings $w.menu
		pack before $w.msg $w.menu {top fill}
	}
}

proc RemakebrowserMenus {} {
	global BrowserMenuSpec
	foreach w [GetBrowserList] {
		pack unpack $w.menu
		destroy $w.menu
		MakeMenubar $w.menu $BrowserMenuSpec
		browserMenuBindings $w.menu
		pack after $w.openList $w.menu {top fill}
	}
}

proc GetTextWindowList {} {
	set window_list [winfo children .]
	set len [llength $window_list]
	for {set n 0} {$n<$len} {incr n} {
		set this_window [lindex $window_list $n]
		if [string match .tw* $this_window] {
			lappend text_window_list $this_window
		}
	}
	return $text_window_list
}

proc GetBrowserList {} {
	set window_list [winfo children .]
	set len [llength $window_list]
	for {set n 0} {$n<$len} {incr n} {
		set this_window [lindex $window_list $n]
		if [string match .bw* $this_window] {
			lappend browser_list $this_window
		}
	}
	return $browser_list
}

proc CheckCloseBrowser {} {
	if [llength [GetBrowserList]]<2 {
		puts stdout "Cannot close the last browser"
	} else {
		CloseBrowser
	}
}

proc InsertFile {} {
	set name [selection get]
	set fid [open $name r]
	InsertString [read $fid]
	close $fid
}

proc SearchForSel {regex direction} {
	set sel [selection get]
	if {$regex == 1} then {
		RegexSearch $sel $direction
	} else {
		Search $sel $direction
	}
}

# Miscellaneous subroutines

proc FixGeometry {g} {
	global screen_width screen_height
	# set default values for width and height
	set w 500
	set h 450
	set ret [regexp {([0-9]+)x([0-9]+)([+-])([0-9]+)([+-])([0-9]+)} $g \
		junk w h sx x sy y]
	if [info exists y] {
		if ![string compare $sx "-"] {
			set x [expr $screen_width-$w-$x]
		}
		if ![string compare $sy "-"] {
			set y [expr $screen_height-$h-$y]
		}
		return [format "%dx%d+%d+%d" $w $h $x $y]
	} else {
		set ret [regexp {([0-9]+)x([0-9]+)} $g junk w h]
		return [format "%dx%d" $w $h]
	}
}

proc OpenFileOrCD {button} {
	global location1 location2 location3
	global browser1 browser2 browser3
	set name [selection get]
	# check for a file size at the end of the name
	set pat "\{(.*) \(\[0-9\]+k\)\}$"
	if [regexp "$pat" "$name" junk fname] {
		# and remove the file size if it is found
		set name $fname
	}
	if [file isdirectory $name] then {
		if {$button==1} {
			CD $name
		}
		if {$button==2} {
			Browser $browser2
			CD $name
		}
		if {$button==3} {
			Browser $browser3
			CD $name
		}
	} else {
		OpenWindow $name "[set location$button]"
	}
}

proc RemoteOpen {name {loc 1} {doNotAsk {}}} {
	global location1 location2 location3
	set tkName [OpenWindow $name "[set location$loc]" $doNotAsk]
	return $tkName
}
	
proc bind.entry args {
    foreach w $args {
	bind $w <2> {%W insert cursor [selection get]}
    }
}

#proc bs win {
#    set x [expr {[$win index cursor] - 1}]
#    if {$x != -1} {$win delete $x}
#}

proc tkerror {msg} {
    global errorInfo
    global debugMode
    # only print messages in debugging mode
    if !$debugMode { return }
    # get rid of spurious error message
    if [string match "*.browserMenu.*" $msg] { return }
    if [string match "bad listbox index*" $msg] { return }
    puts stderr "Background error (usually harmless):\n$errorInfo"
}

