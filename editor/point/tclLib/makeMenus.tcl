#
#
# Make the menus and menu bars
#
#
proc MakeMenubar {name menuString} {
	frame $name -relief raised
	set menubuttonlist ""
	foreach item $menuString {
		set kind [lindex $item 0]
		set buttonName "[lindex $item 1]"
		case $kind in {
		"button" {
			MakeButton $name.$buttonName \
				"[lindex $item 2]" "[lindex $item 3]"
			}
		"menu" {
			lappend menubuttonlist $name.$buttonName
			MakeMenu $name.$buttonName \
				"[lindex $item 2]" "[lindex $item 3]" \
				"[lindex $item 4]"
			}
		}
		pack append $name $name.$buttonName {left fill}
	}
	eval tk_menuBar $name $menubuttonlist
}

proc ButtonHelp {name} {
	global ButtonHelpText
	MessageLine $ButtonHelpText($name)
}


proc MakeButton {name text cmd} {
	global ButtonFont
	button $name -text "$text" -relief raised
	bind $name <Any-1> "
		tk_butDown %W
	"
	bind $name <Any-2> "
		tk_butDown %W
	"
	bind $name <Any-3> "
		tk_butDown %W
	"
	bind $name <Any-ButtonRelease-1> "pt_butUp %W \{[lindex $cmd 0]\}"
	bind $name <Any-ButtonRelease-2> "pt_butUp %W \{[lindex $cmd 1]\}"
	bind $name <Any-ButtonRelease-3> "pt_butUp %W \{[lindex $cmd 2]\}"
	global ButtonHelpText
	set ButtonHelpText($name) "[lindex $cmd 3]"
	bind $name <Any-Enter> "
		focus $name
		ButtonHelp $name
		[bind Button <Any-Enter>]
	"
	bind $name <Any-Leave> "
		MessageLine {}
		[bind Button <Any-Leave>]
	"
}


proc pt_butUp {w cmd} {
    tk_butUp $w
    global tk_priv
    if {$w == $tk_priv(window)} {
        $w flash
	catch "eval $cmd"
    }
}

proc MakeMenuItems {name menuString} {
	set n 0
	foreach item $menuString {
		set kind [lindex $item 0]
		case $kind in {
		"cascade"   {
			$name add cascade -label [lindex $item 1] \
				-menu $name.$n
			menu $name.$n
			if {[llength [lindex $item 2]]==1} then {
				set m [lindex $item 2]
				global $m
				MakeMenuItems $name.$n [set $m]
			} else {
				MakeMenuItems $name.$n [lindex $item 2]
			}
		}
		"separator" { $name add separator }
		"command"   {MakeCommand $name \
			[lindex $item 1] [lindex $item 2] }
		"check"     {MakeCheck $name $n \
			[lindex $item 1]  [lindex $item 2] [lindex $item 3] }
		"radio"     {MakeRadio $name $n \
			[lindex $item 1]  [lindex $item 2] \
			[lindex $item 3]  [lindex $item 4] }
		}
		set n [expr $n+1]
	}
}

proc MakeCommand {name text cmd} {
	$name add command -label "$text" -command "$cmd"
}

proc MakeCheck {name index text variable cmd} {
	global $variable
	set $variable 0
	$name add checkbutton -label "$text" -variable $variable \
							-command "$cmd"
	set xx [Option get $variable]
	if $xx {catch {$name invoke $index}}
}

proc MakeRadio {name index text variable value cmd} {
	global $variable
	$name add radio -label "$text" -variable $variable -value $value \
		-command $cmd
	if ![string compare "[Option get $variable]" "$value"] {
		catch {$name invoke $index}
	}
}

proc MakeMenu {name text menuString helptext} {
	global menuBarButton ButtonFont
	menubutton $name -text $text -relief raised -menu $name.m
	global ButtonHelpText
	set ButtonHelpText($name) "$helptext"
	bind $name <Any-Enter> "
		focus $name
		ButtonHelp $name
		[bind Menubutton <Any-Enter>]
	"
	bind $name <Any-Leave> "
		MessageLine {}
		[bind Menubutton <Any-Leave>]
	"
	menu $name.m
	global $menuString
	MakeMenuItems $name.m [set $menuString]
}

