# seditBind.tcl
#
# Support routines to define a set of consistent editing bindings for
# Text and Entry widgets
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Sedit_BindInit {} {
    global sedit

    set sedit(key,selpaste) <Control-y>
    set sedit(key,seldelete) {<Control-w> <Key-Delete>}
    set sedit(key,backspace) {<Control-h> <Key-BackSpace>}
    set sedit(key,openline) <Control-o>
    set sedit(key,deleol) <Control-k>
    set sedit(key,delbol) <Control-x><Key-Delete>
    set sedit(key,delwordforw) <Meta-d>
    set sedit(key,delwordback) <Meta-h>
    set sedit(key,delchar) <Control-d>
    set sedit(key,transpose) <Control-t>
    set sedit(key,transword) <Meta-t>

    set sedit(key,bof)	<Meta-less>
    set sedit(key,eof)	<Meta-greater>
    set sedit(key,linestart) <Control-a>
    set sedit(key,lineend) <Control-e>
    set sedit(key,up1line) {<Control-p> <Key-Up>}
    set sedit(key,down1line) {<Control-n> <Key-Down>}
    set sedit(key,backword) <Meta-b>
    set sedit(key,forwword) <Meta-f>
    set sedit(key,backchar) {<Control-b> <Key-Left>}
    set sedit(key,forwchar) {<Control-f> <Key-Right>}
    set sedit(key,up1page) <Meta-v>
    set sedit(key,down1page) <Control-v>

    set sedit(dotfile) ~/.exmhsedit

    set sedit(typeKillsSel) 1
    set sedit(scrollButton) Middle

    SeditReadPref
    Sedit_ClassBindings
}
proc SeditReadPref {} {
    global sedit
    if [file exists $sedit(dotfile)] {
	if [catch {uplevel #0 source [glob $sedit(dotfile)]} msg] {
	    Exmh_Status "Error in $sedit(dotfile): $msg"
	    return
	} 
    }
}
proc SeditBind { class key body } {
    global sedit
    if [catch {
	foreach seq $sedit(key,$key) {
	    if {$seq == {}} {
		continue
	    }
	    bind $class $seq $body
	    # Double-bind Meta-key and Escape-key
	    if [regexp {<Meta-(.*)>} $seq match letter] {
		bind $class <Escape><$letter> $body
	    }
	    # Make leading keystroke harmless
	    if [regexp {(<.+>)<.+>} $seq match prefix] {
		bind $class $prefix { }
	    }
	}
    } err] {
	if ![info exists sedit(key,$key)] {
	    puts stderr "Bind $class $key: $err"
	} else {
	    puts stderr "Bind $class $key $sedit(key,$key): $err"
	}
    }
}
proc Sedit_TagBindings { w tag } {
    $w tag bind $tag <Button-1>		{Text_SetInsert %W @%x,%y}
    $w tag bind $tag <Double-Button-1>	{Text_WordSelect %W @%x,%y}
    $w tag bind $tag <Triple-Button-1>	{Text_LineSelect %W @%x,%y}
    $w tag bind $tag <B1-Motion>		{WidgetTextSelMotion %W %x %y}
    $w tag bind $tag <ButtonRelease-1>	{WidgetTextSelDone %W}
    $w tag bind $tag <Shift-B1-Motion>	{WidgetTextSelMotion %W %x %y}
    $w tag bind $tag <Shift-ButtonRelease-1>	{WidgetTextSelDone %W}
}
proc Sedit_ClassBindings { } {
    global sedit

    # Scroll bindings

    bind Text <Button-1>	{Text_SetInsert %W @%x,%y}
    bind Text <Double-Button-1>	{Text_WordSelect %W @%x,%y}
    bind Text <Triple-Button-1>	{Text_LineSelect %W @%x,%y}
    bind Text <B1-Motion>	{WidgetTextSelMotion %W %x %y}
    bind Text <ButtonRelease-1>	{WidgetTextSelDone %W}
    bind Text <Shift-B1-Motion>	{WidgetTextSelMotion %W %x %y}
    bind Text <Shift-ButtonRelease-1>	{WidgetTextSelDone %W}

    # Clear default scroll bindings
    foreach seq {<Button-2> <B2-Motion> <ButtonRelease-2>} {
	bind Text $seq {}
    }

    global exwin
    case $sedit(scrollButton) {
	Right {set b 3}
	Middle {set b 2}
	ShiftMiddle {set b shift2}
	None {set b {}}
    }
    if {"$b" == "2" || "$b" == "3"} {
	bind Text <Button-$b> 		{WidgetTextMark %W %y}
	bind Text <B$b-Motion> 		\
	    {WidgetTextDragto %W %y $exwin(scrollSpeed)}
	bind Text <Shift-Button-$b> 	{WidgetTextMark %W %y}
	bind Text <Shift-B$b-Motion> 	\
	    {WidgetTextDragto %W %y [expr $exwin(scrollAccel)*$exwin(scrollSpeed)]}
    }
    if {"$b" == "shift2"} {
	set b 2
	bind Text <Shift-Button-$b> 	{WidgetTextMark %W %y}
	bind Text <Shift-B$b-Motion> 	\
	    {WidgetTextDragto %W %y $exwin(scrollSpeed)}
    }

    # Modification bindings

    bind Text <Return> {
	Text_Insert %W insert \n; %W yview -pickplace insert
	SeditDirty %W
    }
    bind Text <Tab> {
	if [%W compare insert < header] {
	    Text_MoveInsert %W insert+1line
	    Text_MoveInsert %W "insert lineend"
	} else {
	    Text_Insert %W insert %A; %W yview -pickplace insert
	    SeditDirty %W
	}
    }
    bind Text <Control-i> [bind Text <Tab>]

    bind Text <Escape> { } ;# no-op
    bind Entry <Escape> { } ;# no-op

    SeditBind Text selpaste {
	Text_Yank %W
	SeditDirty %W
    }
    SeditBind Entry selpaste {
	if [catch {
	    %W insert insert [selection get]
	}] {
	    if [catch {%W insert insert [cutbuffer get]}] {
#		catch {%W insert insert $sedit(killbuf)}
	    }
	}
    }

    SeditBind Text seldelete {
	Text_KillSelection %W
	SeditDirty %W
    }
    SeditBind Entry seldelete {
	catch {%W delete sel.first sel.last}
    }

    SeditBind Text backspace {
	Text_Backspace %W; %W yview -pickplace insert
	SeditDirty %W
    }
    SeditBind Entry backspace {
	tk_entryBackspace %W
    }

    SeditBind Text openline {
	Text_Insert %W insert \n
	Text_MoveInsert %W insert-1c
	SeditDirty %W
    }
    SeditBind Entry openline { info library }

    SeditBind Text deleol {
	Text_KillRight %W
	%W yview -pickplace insert
	SeditDirty %W
    }
    SeditBind Entry deleol {
	%W delete insert end
    }

    SeditBind Text delbol {
	Text_KillLeft %W
	%W yview -pickplace insert
	SeditDirty %W
    }
    SeditBind Entry delbol {
	%W delete insert end
    }

    SeditBind Text delwordforw {
	Text_DelWordRight %W
	SeditDirty %W
    }
    SeditBind Entry delwordforw { }

    SeditBind Text delwordback {
	Text_DelWordLeft %W
	SeditDirty %W
    }
    SeditBind Entry delwordback { }

    SeditBind Text delchar {
	Text_DelRight %W; %W yview -pickplace insert
	SeditDirty %W
    }
    SeditBind Entry delchar {
	%W delete insert
    }

    SeditBind Text transpose {
	Text_TransposeChars %W
	SeditDirty %W
    }
    SeditBind Entry transpose {
    }

    SeditBind Text transword {
	Text_TransposeWords %W
	SeditDirty %W
    }
    SeditBind Entry transword {
    }

    # Motion bindings
    SeditBind Text bof {
	Text_MoveInsert %W 1.0
    }
    SeditBind Entry bof { }

    SeditBind Text eof {
	Text_MoveInsert %W end
    }
    SeditBind Entry eof { }

    SeditBind Text linestart {
	Text_MoveToBOL %W
    }
    SeditBind Entry linestart {
	%W icursor 0
    }

    SeditBind Text lineend {
	Text_MoveInsert %W "insert lineend"
    }
    SeditBind Entry lineend {
	%W icursor end
    }

    set sedit(lastpos,Text) {}
    SeditBind Text up1line {
	Text_MoveInsert %W insert-1line
    }
    SeditBind Entry up1line { }

    SeditBind Text down1line {
	Text_MoveInsert %W insert+1line
    }
    SeditBind Entry down1line { }

    SeditBind Text backword {
	Text_MoveInsert %W [Text_PrevWord %W insert]
    }
    SeditBind Entry backword {
	set string [%W get]
	set curs [expr [%W index insert]-1]
	if {$curs < 0} return
	for {set x $curs} {$x > 0} {incr x -1} {
	    if {([string first [string index $string $x] " \t"] < 0)
		    && ([string first [string index $string [expr $x-1]] " \t"]
		    >= 0)} {
		break
	    }
	}
	%W icursor $x
    }

    SeditBind Text forwword {
	Text_MoveInsert %W [Text_NextWord %W insert]
    }
    SeditBind Entry forwword {
	set string [%W get]
	set curs [expr [%W index insert]+1]
	set len [string length $string]
	if {$curs < 0} return
	for {set x $curs} {$x < $len} {incr x} {
	    if {([string first [string index $string $x] " \t"] < 0)
		    && ([string first [string index $string [expr $x+1]] " \t"]
		    >= 0)} {
		break
	    }
	}
	%W icursor $x	
    }

    SeditBind Text backchar {
	Text_MoveInsert %W insert-1c
    }
    SeditBind Entry backchar {
	set x [%W index insert]
	if {$x > 0} {
	    incr x -1
	    %W icursor $x
	}
    }

    SeditBind Text forwchar {
	Text_MoveInsert %W insert+1c
    }
    SeditBind Entry forwchar {
	set x [%W index insert]
	incr x
	%W icursor $x
    }

    SeditBind Text up1page {
	Widget_TextPageUp %W
    }
    SeditBind Entry up1page { } ;# no-op

    SeditBind Text down1page {
	Widget_TextPageDown %W
    }
    SeditBind Entry down1page { } ;# no-op

    bind Text <Any-Key> {
	if {"%A" != ""} {
	    if $sedit(typeKillsSel) {
		Text_KillSelection %W
	    }
	    Text_Insert %W insert %A
	    %W yview -pickplace insert
	    SeditDirty %W
	}
    }
}
proc SeditMarkClean { t } {
    global sedit
    set sedit($t,dirty) 0
}
proc SeditDirty { t } {
    global sedit
    set sedit($t,dirty) 1
}
proc SeditIsDirty { t } {
    global sedit
    return $sedit($t,dirty)
}

proc Sedit_Pref {} {
    global sedit
    if [Exwin_Toplevel .seditpref "Simple Edit Preferences" Pref] {
	Widget_AddBut .seditpref.but save "Save" {SeditPrefSave}
	Widget_Label .seditpref.but label {left fill} \
	    -text "Text and Entry class bindings"

	Widget_Message .seditpref msg -aspect 1500 -text \
"Use this dialog to change the keybindings for edit functions.
1. Any sequence like <Meta-x> also results in a binding for <Escape><x>
2. If you bind <Button-2> to selpaste, change the Scroll Button setting, too.
3. Case is significant in the keywords Control, Meta, Shift, Key, etc.
4. Multiple bindings:  to bind multiple (different) keystrokes to a
function, separate the sequences with spaces."

	set f2 [Widget_Frame .seditpref tog]
	$f2 configure -bd 10
	Widget_CheckBut $f2 type "Type Kills SEL" sedit(typeKillsSel) {left padx 1}
	Widget_Label $f2 label {left padx 10} -text "Scroll Button"
	Widget_RadioBut $f2 but2 "Middle" sedit(scrollButton)
	Widget_RadioBut $f2 but3 "Right" sedit(scrollButton)
	Widget_RadioBut $f2 shift2 "ShiftMiddle" sedit(scrollButton)
	Widget_RadioBut $f2 none "None" sedit(scrollButton)

	set f [Widget_Frame .seditpref p Dialog]
	$f configure -bd 10
	set lr [Widget_SplitFrame $f Left Right]
	set left [lindex $lr 0]
	set right [lindex $lr 1]
	set width 0
	foreach item [array names sedit] {
	    if [regexp key $item] {
		set name [lindex [split $item ,] 1]
		set w [string length $name]
		if {$w > $width} { set width $w }
	    }
	}
	set size 0
	# Hack - remove after 1.3
	foreach x {2 3} {
	    if [info exists sedit(key,backspace$x)] {
		lappend sedit(key,backspace) $sedit(key,backspace$x)
		unset sedit(key,backspace$x)
	    }
	}
	if [info exists sedit(key,delword)] {
	    lappend sedit(key,delwordforw) $sedit(key,delword)
	    unset sedit(key,delword)
	}
	foreach item [lsort [array names sedit]] {
	    if [regexp key $item] {
		set name [lindex [split $item ,] 1]
		incr size
		set keystroke $sedit($item)
		set frame [lindex $lr [expr {$size % 2}]]
		SeditPrefItem $frame $width $name $keystroke
	    }
	}
    }
}
proc SeditPrefItem { frame width name keystroke } {
    global sedit
    Widget_Frame $frame $name Pref
    Widget_Label $frame.$name label {left} -text $name -width $width -anchor w
    Widget_Entry $frame.$name entry {right expand fill} -background white -relief sunken -bd 2 -font fixed
    set sedit(entry,$name) $frame.$name.entry
    $frame.$name.entry insert 0 $keystroke
}
proc SeditPrefSave { } {
    global sedit
    # Save it
    set out [open $sedit(dotfile) w]
    foreach item [array names sedit] {
	if [regexp key $item match] {
	    set name [lindex [split $item ,] 1]
	    set entry $sedit(entry,$name)
	    set keystrokes [$entry get]
	    puts $out [list set sedit($match,$name) $keystrokes]
	}
    }
    puts $out "set sedit(typeKillsSel) $sedit(typeKillsSel)"
    puts $out "set sedit(scrollButton) $sedit(scrollButton)"
    close $out
    Exwin_Dismiss .seditpref
    # Apply it to current session
    SeditReadPref
    Sedit_ClassBindings
}


