# sedit
#
# A simple editor for composing mail messages.
# See also the Text and Entry bindings in seditBind.tcl
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc SeditHelp {} {
    if [Exwin_Toplevel .sedithelp "Sedit help" Help] {
	set t .sedithelp
	Widget_Message $t msg -text \
"
Sedit is a simple editor built into exmh.
The editor works on draft messages that are kept in your
MH draft folder.  When you do Compose, Reply, or Forward,
a new draft is created for you.

Here is what the buttons do:

Abort - remove the draft message from the drafts folder.
    The editor window is removed.
Bind - displays a dialog box that lets you edit keybindings.
    See the TK bind man page for keystroke syntax, if you need to.
Help - displays this window.
Insert @ - Insert a copy of the current message (replies only)
Send - post the message.
Sign - insert your ~/.signature file at the end of the message.
Sign... - A menu of ~/.signature* files if you have more than one.
Whom - displays the list of recipients for the message.

More... - Bring up a menu with the following options:
Keep on Send - if this button is highlighted, then Send and Save will not
    remove the editor window.  Otherwise, the window disappears afterwards.
Format mail - this breaks long lines (> 79 characters) at word boundaries.
    Note that the auto-wrapping done by the TK text widget does not insert
    newline characters.  This will be done by the editor when you save
    the message and the Format mail option is selected.
Attempt mhn - run the message body through mhn before sending.  If you know
    the # directives of mhn, you can use this quick hack to compose MIME
    messages.  A GUI alternative will appear someday.
Insert File... - Insert a file into the message.
Save - write out the editor buffer to disk.  This is useful if you want
    to re-edit the draft message later.
Spell... - run UNIX spell over the message body.
Find SEL - search the message body for the selected string.
Dismiss - remove the editor window.  You will be prompted if the message
    has not been saved or sent.  If you abort the message from the dialog,
    it gets deleted from the drafts folder.
"
    }
}

proc Sedit_Start { draft } {
    global sedit
    if ![info exists sedit(init)] {
	Sedit_Init
    }
    set id [file tail $draft]
    if {[Exwin_Toplevel .sedit$id $draft Sedit] == 0} {
	# Reuse existing window
	set t $sedit($id,text)
	set sedit($t,format) $sedit(formatDefault)
	SeditMsg $t $draft
	$t delete 1.0 end
    } else {
	set b .sedit${id}.but
	set f [Widget_Frame .sedit$id f Frame {top expand fill}]
	set t [Widget_Text $f $sedit(height) -cursor xterm -setgrid true]
	set sedit($t,status) [Widget_Entry .sedit${id} status {top fill} -relief raised]
	$b.quit configure -command [list SeditQuit $draft $t]

	Widget_AddBut $b send "Send" [list SeditSend $draft $t] {right padx 10}
	if [catch {glob ~/.signature*} sigFiles1] {
	    set sigFiles1 [glob ~]/.signature
	}
	set sigFiles {}
	foreach sig $sigFiles1 {
	    if {! [string match *~ $sig]} {
		lappend sigFiles $sig
	    }
	}
	if {[llength $sigFiles] <= 1} {
	    Widget_AddBut $b sign "Sign" [list SeditSign $draft $t]	
	} else {
	    set menub [Widget_AddMenuB $b sign "Sign..." {right padx 1}]
	    foreach file $sigFiles {
		Widget_AddMenuItem $menub [file tail $file] [list SeditSign $draft $t $file]
	    }
	}
	Widget_AddBut $b whom "Whom" [list SeditWhom $draft $f $t]
	Widget_AddBut $b pref "Bind" [list Sedit_Pref]
	Widget_AddBut $b help "Help" [list SeditHelp]

	Widget_AddBut $b repl "Insert @" {}
	set menub [Widget_AddMenuB $b text "Text..." {right padx 1}]
	Widget_AddMenuItem $menub "Plain" \
	    [list SeditMimeEnriched $draft $t x-plain]
	Widget_AddMenuItem $menub "Fixed" \
	    [list SeditMimeEnriched $draft $t fixed]
	Widget_AddMenuItem $menub "Bold" \
	    [list SeditMimeEnriched $draft $t bold]
	Widget_AddMenuItem $menub "Italic" \
	    [list SeditMimeEnriched $draft $t italic]
	Widget_AddMenuItem $menub "Underline" \
	    [list SeditMimeEnriched $draft $t underline]
	Widget_AddMenuItem $menub "Smaller" \
	    [list SeditMimeEnriched $draft $t smaller]
	Widget_AddMenuItem $menub "Bigger" \
	    [list SeditMimeEnriched $draft $t bigger]

	set menub [Widget_AddMenuB $b file "More..." {right padx 1}]
	set sedit($t,keep) 0
	set sedit($t,format) $sedit(formatDefault)
	set sedit($t,mhn) $sedit(mhnDefault)
	Widget_CheckMenuItem $menub "Keep on send" { } sedit($t,keep)
	Widget_CheckMenuItem $menub "Format mail" { } sedit($t,format)
	Widget_CheckMenuItem $menub "Attempt mhn" { } sedit($t,mhn)
	Widget_AddMenuItem $menub "Insert File..." [list SeditInsertFileDialog $draft $t]
	Widget_AddMenuItem $menub "Save" [list SeditSave $draft $t]
	Widget_AddMenuItem $menub "Spell..." [list SeditSpell $draft $f $t]
	Widget_AddMenuItem $menub "Find SEL" [list Sedit_Find $draft $t]

	Widget_AddBut $b abort "Abort" [list SeditAbort $draft $t] {left padx 3}

	SeditMsg $t $draft

	# Define a bunch of maps among things
	set sedit($t,toplevel) .sedit$id
	set sedit($id,text) $t
	set sedit($t,id) $id
	lappend sedit(allids) .sedit$id
	set sedit(.sedit$id,draft) $draft
	set sedit(.sedit$id,id) $id
    }
    SeditTextBindings $draft $t		;# set up sendMsg binding
    if [file readable "@"] {
	.sedit$id.but.repl configure -state normal -command \
		[list SeditInsertFile $draft $t "@"]
    } else {
	.sedit$id.but.repl configure -state disabled
    }
    set sedit($t,sent) 0
    set sedit($t,dirty) 0
    SeditMimeReset $t

    if [catch {open $draft r} in] {
	$t insert 1.0 "Cannot open $draft"
    } else {
	$t insert 1.0 [read $in]
	close $in
	SeditPositionCursor $t
    }
    focus $t
}
proc SeditPositionCursor { t } {
    set l 1
    set insert 0
    set header 0
    for {set l 1} {1} {incr l} {
	if {[$t compare $l.0 > end]} {
	    if {! $insert} {
		$t mark set insert end
	    }
	    if {! $header} {
		incr l -1
		$t mark set header $l.0
	    }
	    return
	}
	set line [$t get $l.0 $l.end]
	if [regexp {: *$} $line] {
	    if {! $insert} {
		$t mark set insert $l.end
		set insert 1
	    }
	}
	if {($line == {}) || [regexp {^-*$} $line]} {
	    incr l -1
	    $t mark set header $l.0
	    if {! $insert} {
		incr l 2
		$t mark set insert $l.0
	    }
	    return
	}
    }
}

proc SeditQuit { draft t } {
    global sedit
    if [SeditIsDirty $t] {
	Widget_Toplevel .seditDirty "Dirty file"
	Widget_Message .seditDirty msg  -aspect 1000 -text "
$draft
has not been saved or sent.
Do you want to abort (remove) it,
send it now,
save it for later editting,
or do nothing?"
	Widget_Frame .seditDirty f Dialog
	.seditDirty.f configure -bd 10
	Widget_AddBut .seditDirty.f ok "Abort" [list SeditNuke $draft $t]
	Widget_AddBut .seditDirty.f send "Send" [list SeditSend $draft $t]
	Widget_AddBut .seditDirty.f save "Save" \
		[list SeditSave $draft $t SeditNuke]
	Widget_AddBut .seditDirty.f no "Do nothing" {destroy .seditDirty}
    } else {
	SeditNuke $draft $t
    }
}
proc SeditNuke { draft t } {
    global sedit
    catch {destroy .seditUnsent}
    catch {destroy .seditDirty}
    catch {destroy $sedit($t,toplevel).whom}
    Exwin_Dismiss $sedit($t,toplevel)
}
proc SeditMsg { t text } {
    global sedit
    $sedit($t,status) configure -state normal
    $sedit($t,status) delete 0 end
    $sedit($t,status) insert 0 $text
    $sedit($t,status) configure -state disabled
    update idletasks
}

proc SeditSend { draft t } {
    if [SeditSave $draft $t] {
	global env sedit
	set id [file tail $draft]
	# Decide if this file needs to go through mhn
	if {$sedit($t,mhn) && ![catch {exec grep -l ^# $draft}]} {
	    set env(mhdraft) $draft
	    SeditMsg $t "Running mhn..."
	    exec mhn $draft
	}
	SeditMsg $t "Message sent"
	SeditMarkSent $t
	Edit_Done send $id
	global sedit
	if {! $sedit($t,keep)} {
	    SeditNuke $draft $t
	}
    }
}
proc SeditAbort { draft t } {
    set id [file tail $draft]
    Edit_Done abort $id
    SeditNuke $draft $t
}
proc SeditWhom { draft f t } {
    global tk_version sedit
    set parent [file root $f]
    if {[catch {destroy $parent.whom}] == 0} {
	return
    }
    # Do an unformatted save so Mh_Whom gets the right info
    set format $sedit($t,format)
    set sedit($t,format) 0
    SeditSave $draft $t
    SeditDirty $t
    set sedit($t,format) $format

    set id [file tail $draft]
    catch {Mh_Whom $id} result
    set lines [llength [split $result \n]]
    set f2 [Widget_Frame $parent whom {top fill}]
    set height [expr {$lines > 8 ? 8 : $lines}]
    set t2 [Widget_Text $f2 $height]
    $t2 configure -height $height	;# Widget_Text broken
    $t2 insert 1.0 $result
    $t2 config -state disabled
    if {$tk_version >= 3.3} {
	pack $f2 -before $f -side top
    } else {
	pack before $f $f2 top
    }
}
proc SeditSign { draft t {f ~/.signature} } {
    if [catch {glob $f} sig] {
	return
    }
    if [file readable $sig] {
	set in [open $sig]
	set signature [read $in]
	$t insert end \n
	$t insert end $signature
	close $in
    }
}
proc SeditInsertFile { draft t file } {
    global sedit
    if [file readable $file] {
	set in [open $file]
	if {$file == "@"} {
	    set inheaders 1
	    while {[gets $in line] > -1} {
		if {! $inheaders} {
		    $t insert insert $sedit(pref,replPrefix)$line\n
		} else {
		    if {[string length $line] == 0} {
			set inheaders 0
		    }
		}
	    }
	} else {
	    set body [read $in]
	    $t insert insert $body
	}
	close $in
    } else {
	SeditMsg $t "Cannot read $file"
    }
}

proc SeditInsertFileDialog { draft t } {
    set name [FSBox "Select file name"]
    if {$name != ""} {
	SeditInsertFile $draft $t $name
    }
}

proc SeditSave { draft t {hook {}} } {
    global sedit
    if [catch {
	set out [open $draft w]
	if {$sedit($t,format)} {
	    SeditFormatMail $t $out
	} else {
	    puts $out [$t get 1.0 end]
	}
	close $out
	SeditMsg $t "Message saved"
	if {$hook != {}} {
	    after 1 [list $hook $draft $t]
	}
    } err] {
	Exmh_Error "SeditSave $draft: $err"
	return 0
    }
    SeditMarkClean $t
    return 1
}
proc SeditSaveBody { t outfile } {
    set out [open $outfile w]
    puts $out [$t get [$t index "header + 1 line"] end]
    close $out
}
proc SeditFormatMail { t out } {
    global sedit exmh
    if {$sedit($t,enriched)} {
	SeditEnrichedExpand $t
	set quote 1
    } else {
	set quote 0
    }
    set xmailer 0
    set inheaders 1
    scan [$t index end] "%d"  last
    #
    # XXX - should nuke all this with on-the-fly line chopping.
    # Also, should pipe message through mimencode -q if there
    # is riched text or 8-bit characters.
    #
    for {set L 1} {$L <= $last} {incr L} {
	set line [$t get $L.0 $L.end]
	if {$inheaders} {
	    # Blank or empty line terminates headers
	    # Leading --- terminates headers
	    if {[regexp {^[ 	]*$} $line] || [regexp {^-+} $line]} {
		# No X-Mailer on redistributed messages
		set id $sedit($t,id)
		if {![info exists exmh($id,mhannodist)] && !$xmailer} {
		    puts $out "X-Mailer: exmh $exmh(version)"
		}
		SeditMimeHeaders $t $out
		set inheaders 0
	    }
	    if {[regexp {^X-Mailer:} $line]} {
		set xmailer 1
	    }
	}
	if $inheaders {
	    set limit $sedit(lineLength)
	} else {
	    set limit $sedit(lineLength)
	}
	set climit [expr $limit-1]
	set cutoff 50
	set continuation 0
	while {[string length $line] > $limit} {
	    for {set c [expr $limit-1]} {$c >= $cutoff} {incr c -1} {
		set char [string index $line $c]
		if {$char == " " || $char == "\t"} {
		    break
		}
		if {$char == ">"} {	;# Hack for enriched formatting
		    break
		}
	    }
	    if {$c < $cutoff} {
		if {! $inheaders} {
		    set c [expr $limit-1]
		} else {
		    set c [string length $line]
		}
	    }
	    set newline [string range $line 0 $c]
	    if {! $continuation} {
		puts $out $newline
	    } else {
		puts $out \ $newline
	    }
	    incr c
	    set line [string range $line $c end]
	    if {$inheaders} {
		set continuation 1
		set limit $climit
	    }
	}
	if {$continuation} {
	    puts $out \ $line
	} else {
	    if {$quote && !$inheaders} {
		# enriched requires two newlines for each one.
		puts $out $line\n
	    } else {
		puts $out $line
	    }
	}
    }
}

proc SeditMarkSent { t } {
    global sedit
    set sedit($t,sent) 1
}
proc SeditNotSent { t } {
    global sedit
    return [expr {! $sedit($t,sent)}]
}

proc SeditTextBindings { draft t } {
    global sedit
    SeditBind $t sendMsg [list SeditSend $draft $t]
    SeditBind Entry sendMsg { }
}

proc SeditSpell { draft f t } {
    global tk_version sedit
    set parent [file root $f]
    if {[catch {destroy $parent.spell}] == 0} {
	return
    }
    # Do an unformatted save so spell gets the right info
    catch {
	SeditSaveBody $t /tmp/exmh.spell.$t
	exec spell /tmp/exmh.spell.$t
    } result
    catch {exec rm /tmp/exmh.spell.$t}

    set f2 [Widget_Frame $parent spell {top fill}]

    set lines [llength [split $result \n]]
    set height [expr {$lines > 8 ? 8 : $lines}]
    set t2 [Widget_Text $f2 $height]
    $t2 configure -height $height	;# Widget_Text broken
    $t2 insert 1.0 $result
    $t2 config -state disabled
    if {$tk_version >= 3.3} {
	pack $f2 -before $f -side top
    } else {
	pack before $f $f2 top
    }
}
proc Sedit_Find {draft t} {
    global sedit
    if [catch {selection get} string] {
	SeditMsg $t "Select a string first"
	return
    }
    # hack
    global find
    if ![info exists find(line)] {
	set find(line) {}
    }
    if ![info exists find(lasthit)] {
	set find(lasthit) {}
    }
    set sedit(searchWidget) $t
    set match [Find_Inner $string forw $find(line) [lindex [split [$t index end] .] 0] Sedit_FindMatch nofeedback]
    case $match {
	0 {
	    SeditMsg $t "Next search will wrap."
	}
	-1 {
	    SeditMsg $t "$string not found"
	}
	default {
	    SeditMsg $t $draft
	    $t mark set insert sel.first
	    focus $t
	}
    }
}
proc Sedit_FindMatch { L string } {
    global sedit
    return [FindTextMatch $sedit(searchWidget) $L $string]
}
proc Sedit_CheckPoint {} {
    global sedit
    foreach top $sedit(allids) {
	if [info exists sedit($top,id)] {
	    set draft $sedit($top,draft)
	    set id $sedit($top,id)
	    set t $sedit($id,text)
	    if [SeditIsDirty $t] {
		Exmh_Status "Saving draft $id"
		SeditSave $draft $t
	    }
	}
    }
}
