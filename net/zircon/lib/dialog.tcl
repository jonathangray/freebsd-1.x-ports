proc doButtons {w param lv pars} {
    if {[llength $pars] > 0} {
	set arg [lindex $pars 0]
	frame $w.bot.0 -relief raised -border 1
	pack $w.bot.0 -side left -expand 1 -fill x -padx 5 -padx 5
	if {[set cmd [lindex $arg 1]] != {}} { append cmd " $param" }
	if ![string match {} $lv] {
	   bind $lv <Return> "$cmd ; killWindow $w ; notIdle {}"
	   bind $lv <Tab> "focus $w.top.v0.value ; notIdle {}"
	}
	button $w.bot.0.button -text [lindex $arg 0] \
		-command "$cmd ; killWindow $w"
	pack $w.bot.0.button -expand 1 -fill x -padx 5 -pady 5
	bind $w <Return> "$cmd ; killWindow %W ; notIdle {}"
	set i 1
	foreach arg [lrange $pars 1 end] {
	    if ![string match {} [set cmd [lindex $arg 1]]] {
		append cmd " $param"
	    }
	    button $w.bot.$i -text [lindex $arg 0] \
	      -command "$cmd ; killWindow $w"
	    pack $w.bot.$i -side left -expand 1 -fill x -padx 5 -pady 5
	    incr i
	}
    }
    bind $w <Any-Enter> {focus %W ; notIdle {}}
}

proc mkDialog {kind w title msgText entries args} {
    if ![string match {} $kind] {
	global noConfirm
	if {[lsearch $noConfirm $kind] >= 0} {
	    set param {}
	    foreach entry $entries {
		if {[set init [lindex $entry 1]] != {}} {
		    append param " {$init}"
		}
	    }
	    if {[llength $args] > 0} {
		set arg [lindex $args 0]
		set cmd [lindex $arg 1]
		if {$cmd != {}} { append cmd " $param" }
	    }
	    eval $cmd
	    return
	}
	global toInfo
	if {[lsearch $toInfo $kind] >= 0} {
	    info0 addText {} "*** $msgText"
	    return
	}
    }
    set just [expr {[lsearch \
      {DCC WHO STATS INFO LINKS WHOIS} $kind] >= 0 ? "left" : "center"}]
    killWindow $w
    toplevel $w -class Zircon
    wm title $w "$title"
    frame $w.top -relief raised
    frame $w.bot -relief raised
    pack $w.top -fill both -expand 1
    pack $w.bot -fill x
    if {$just == "left"} {
	scrollbar $w.top.vscroller -command "$w.top.message yview"
	text $w.top.message -yscrollcommand "$w.top.vscroller set"
	$w.top.message insert insert $msgText
	set ln [lindex [split [$w.top.message index end] .] 0]
	$w.top.message conf -state disabled -height $ln
	pack $w.top.message -side left -expand 1 -fill both
	pack $w.top.vscroller -side left -fill y
    } {
	message $w.top.message -justify $just -text "$msgText" -aspect 500
	pack $w.top.message -expand 1 -fill both
    }

    set param {}
    set vb 0
    set lv {}
    foreach entry $entries {
	frame $w.top.v${vb}
	set name $w.top.v${vb}.value
	label $w.top.v${vb}.label -text [lindex $entry 0]
	emacsEntry $name
	if {[set init [lindex $entry 1]] != {}} { $name insert end $init }
	append param " \[${name} get\]"
	pack $w.top.v${vb}.label -side left -padx 5 -pady 5
	pack $w.top.v${vb}.value -side left -expand 1 -fill x \
	  -padx 10 -pady 10
	pack $w.top.v${vb} -fill x -padx 5 -pady 5
	set lv $w.top.v${vb}.value
	incr vb
	bind $lv <Return> "notIdle {} ; focus $w.top.v${vb} "
	bind $lv <Tab> "notIdle {} ; focus $w.top.v${vb} "
    }
    doButtons $w $param $lv $args
    focus $w
    tkwait visibility $w
    catch "grab $w"
}

proc mkEntryBox {w title msgText entries args} {
    killWindow $w
    toplevel $w -class Zircon
    wm title $w "$title"

    frame $w.top -relief raised
    frame $w.bot -relief raised
    pack $w.top -fill both -expand 1
    pack $w.bot -fill x
    message $w.top.message -text "$msgText" -aspect 500
    pack $w.top.message -expand 1 -fill both

    set param {}
    set vb 0
    set lv {}
    foreach entry $entries {
	frame $w.top.v${vb}
	set name $w.top.v${vb}.value
	label $w.top.v${vb}.label -text [lindex $entry 0]
	emacsEntry $name
	if {[set init [lindex $entry 1]] != {}} { $name insert end $init }
	append param " \[${name} get\]"
	pack $w.top.v${vb}.label -side left -padx 5 -pady 5
	pack $w.top.v${vb}.value -side left -expand 1 -fill x \
	  -padx 10 -pady 10
	pack $w.top.v${vb} -fill x -padx 5 -pady 5
	set lv $w.top.v${vb}.value
	incr vb
	bind $lv <Return> "notIdle {} ; focus $w.top.v${vb} "
	bind $lv <Tab> "notIdle {} ; focus $w.top.v${vb} "
    }
    doButtons $w $param $lv $args
    focus $w
}
#
proc mkInfoBox {kind w title msgText args} {
    if ![string match {} $kind] {
	global noConfirm toInfo
	if {[lsearch $noConfirm $kind] >= 0} {
	    if {[llength $args] > 0} {
		eval [lindex [lindex $args 0] 1]
	    }
	    return [info0 text]
	}
	if {[lsearch $toInfo $kind] >= 0} {
	    info0 addText {} $msgText
	    return [info0 text]
	}
    }
    set just [expr {[lsearch \
      {DCC WHO STATS INFO LINKS WHOIS} $kind] >= 0 ? {left} : {center}}]
    killWindow $w
    toplevel $w -class Zircon
    wm title $w $title
    wm minsize $w 10 2
    frame $w.top -relief raised
    bind $w.top <Destroy> { }
    frame $w.bot -relief raised
    pack $w.bot -fill x -expand 1 -side bottom
    pack $w.top -fill both -expand 1 -side top
    if {$just == "left"} {
	scrollbar $w.top.vscroller -command "$w.top.message yview"
	text $w.top.message -width 80 -height 10 \
	  -yscrollcommand "$w.top.vscroller set"
	if ![string match {} $msgText] {
	    $w.top.message insert insert $msgText
	    set ln [lindex [split [$w.top.message index end] .] 0]
	    set ln [expr $ln > 24 ? 24 : $ln]
	    $w.top.message conf -state disabled -height $ln
	}
	pack $w.top.vscroller -side right -fill y
	pack $w.top.message -side left -expand 1 -fill both
    } {
	message $w.top.message -justify $just -text $msgText
	pack $w.top.message -expand 1 -fill both
    }
    doButtons $w {} {} $args
    return $w.top.message
}
#
proc setFile {w y cmd} {
    set x [$w nearest $y]
    set fn [$w get $x]
    if [file isdirectory $fn] {
	cd $fn
	$w delete 1 end
	if ![catch {set fls [glob *]}] {
	    foreach fl [lsort $fls] {
		if [file isdirectory $fl] { append fl / }
		$w insert end $fl
	    }
	}
	foreach fn [lsort [glob *]] { $w insert end $fn	}
    } {
	eval $cmd $fn
	killWindow [winfo toplevel $w]
	notIdle {}
    }
}

proc mkFileBox {w title msgText init args} {
    killWindow $w
    toplevel $w -class Zircon
    wm title $w "$title"

    frame $w.top -relief raised
    frame $w.mid -borderwidth 0
    frame $w.bot -relief raised
    pack $w.top $w.mid -fill both -expand 1
    pack $w.bot -fill x
    if ![string match {} $msgText] {
	message $w.top.message -text "$msgText" -aspect 500
	pack $w.top.message -expand 1 -fill both
    }
    makeLB $w.mid.flist -setgrid 1
    pack $w.mid.flist -expand 1 -fill both
    labelEntry 0 $w.mid.fn {-text Filename} $init {}
    pack $w.mid.fn -expand 1 -fill x
    $w.mid.flist.l insert end ../
    if ![catch {set fls [glob *]}] {
	foreach fl [lsort $fls] {
	    if [file isdirectory $fl] { append fl / }
	    $w.mid.flist.l insert end $fl
	}
    }
    bind $w.mid.flist.l <1> "
	set x \[%W nearest %y\]
	set f \[%W get \$x\]
	entrySet $w.mid.fn.entry \
	 \[expr {\[file isdirectory \$f\] ? {} : \$f }\]
	%W select from \$x
    "
    set cmd [lindex [lindex $args 0] 1]
    bind $w.mid.flist.l <Double-1> "setFile %W %y {$cmd}"
    if ![string match {} $args] {
	set arg [lindex $args 0]
	frame $w.bot.0 -relief raised -border 1
	pack $w.bot.0 -side left -expand 1 -fill x -padx 5 -pady 5
	set cmd [lindex $arg 1]
	if ![string match {} $cmd] { append cmd " \[$w.mid.fn.entry get\]" }
	button $w.bot.0.button -text [lindex $arg 0] \
		-command "$cmd ; killWindow $w ; notIdle {}"
	pack $w.bot.0.button -expand 1 -fill x -padx 5 -pady 5
	bind $w <Return> "$cmd ; killWindow %W ; notIdle {}"
	focus $w
	set i 1
	foreach arg [lrange $args 1 end] {
	    set cmd [lindex $arg 1]
	    if ![string match {} $cmd] {
		append cmd "\[$w.mid.fn.entry get\]"
	    }
	    button $w.bot.$i -text [lindex $arg 0] \
	      -command "$cmd ; killWindow $w"
	    pack $w.bot.$i -side left -expand 1 -fill x
	    incr i
	}
    }
    bind $w <Any-Enter> {focus %W ; notIdle {}}
}
