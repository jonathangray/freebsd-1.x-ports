#
# Procedure to put up a dialog box. The code is based on that
# provided in one of the tk demos, but has been altered to allow
# dialog boxes with entry fields in them. It also takes a window
# title.
#
proc doButtons {w param lv pars} {
    if {[llength $pars] > 0} {
	set arg [lindex $pars 0]
	frame $w.bot.0 -relief raised -border 1
	zpack $w.bot 0 {left expand fillx}
	if {[set cmd [lindex $arg 1]] != {}} { append cmd " $param" }
	if {$lv != {}} {
	   bind $lv <Return> "$cmd ; killWindow $w ; notIdle {}"
	   bind $lv <Tab> "focus $w.top.v0.value ; notIdle {}"
	}
	button $w.bot.0.button -text [lindex $arg 0] \
		-command "$cmd ; killWindow $w"
	zpack $w.bot.0 button {expand fillx padx 12 pady 12}
	bind $w <Return> "$cmd ; killWindow %W ; notIdle {}"
	set i 1
	foreach arg [lrange $pars 1 end] {
	    if {[set cmd [lindex $arg 1]] != {}} { append cmd " $param" }
	    button $w.bot.$i -text [lindex $arg 0] \
	      -command "$cmd ; killWindow $w"
	    zpack $w.bot $i {left expand fillx}
	    incr i
	}
    }
    bind $w <Any-Enter> {focus %W ; notIdle {}}
}

proc mkDialog {kind w title msgText entries args} {
    if {$kind != {}} {
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
	    addText {} @info "*** $msgText"
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
    zpack $w top {fill expand}
    zpack $w bot {fillx}
    if {$just == "left"} {
	scrollbar $w.top.vscroller -command "$w.top.message yview"
	text $w.top.message -yscrollcommand "$w.top.vscroller set"
	$w.top.message insert insert $msgText
	set ln [lindex [split [$w.top.message index end] .] 0]
	$w.top.message conf -state disabled -height $ln
	zpack $w.top message {left expand fill}
	zpack $w.top vscroller {left filly}
    } {
	message $w.top.message -justify $just -text "$msgText" -aspect 500
	zpack $w.top message {expand fill}
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
	zpack $w.top.v${vb} label {left padx 5 pady 5}
	zpack $w.top.v${vb} value {left expand fillx padx 5 pady 5}
	zpack $w.top v${vb} {fillx padx 5 pady 5}
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
    zpack $w top {fill expand}
    zpack $w bot {fillx}
    message $w.top.message -text "$msgText" -aspect 500
    zpack $w.top message {expand fill}

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
	zpack $w.top.v${vb} label {left padx 5 pady 5}
	zpack $w.top.v${vb} value {left expand fillx padx 5 pady 5}
	zpack $w.top v${vb} {fillx padx 5 pady 5}
	set lv $w.top.v${vb}.value
	incr vb
	bind $lv <Return> "notIdle {} ; focus $w.top.v${vb} "
	bind $lv <Tab> "notIdle {} ; focus $w.top.v${vb} "
    }
    doButtons $w $param $lv $args
    focus $w
}

proc mkInfoBox {kind w title msgText args} {
    if {$kind != {}} {
	global noConfirm
	if {[lsearch $noConfirm $kind] >= 0} {
	    if {[llength $args] > 0} {
		eval [lindex [lindex $args 0] 1]
	    }
	    return
	}
	global toInfo
	if {[lsearch $toInfo $kind] >= 0} {
	    addText {} @info $msgText
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
    zpack $w top {fill expand}
    zpack $w bot {fillx}
    if {$just == "left"} {
	scrollbar $w.top.vscroller -command "$w.top.message yview"
	text $w.top.message -yscrollcommand "$w.top.vscroller set"
	$w.top.message insert insert $msgText
	set ln [lindex [split [$w.top.message index end] .] 0]
	$w.top.message conf -state disabled -height $ln
	zpack $w.top message {left expand fill}
	zpack $w.top vscroller {left filly}
    } {
	message $w.top.message -justify $just -text "$msgText"
	zpack $w.top message {expand fill}
    }
    doButtons $w {} {} $args
}

proc setFile {w y cmd} {
    set x [$w nearest $y]
    set fn [$w get $x]
    if [file isdirectory $fn] {
	cd $fn
	$w delete 1 end
	foreach fn [lsort [glob *]] {
	    $w insert end $fn
	}
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
    zpack $w {top mid} {fill expand}
    zpack $w bot {fillx}
    if {$msgText != {}} {
	message $w.top.message -text "$msgText" -aspect 500
	zpack $w.top message {expand fill}
    }
    frame $w.mid.flist -borderwidth 0
    scrollbar $w.mid.flist.vscroller -command "$w.mid.flist.list yview"
    listbox $w.mid.flist.list \
      -xscrollcommand "$w.mid.hsFrm.hscroller set" \
      -yscrollcommand "$w.mid.flist.vscroller set" -setgrid 1

    zpack $w.mid.flist list {left expand fill}
    zpack $w.mid.flist vscroller {left filly} 

    frame $w.mid.hsFrm
    scrollbar $w.mid.hsFrm.hscroller -command "$w.mid.flist.list xview" \
      -orient horizontal

    frame $w.mid.hsFrm.pf0
    zpack $w.mid.hsFrm hscroller {left expand fillx}
    zpack $w.mid.hsFrm pf0 {left padx 20} 
    zpack $w.mid flist {expand fill}
    zpack $w.mid hsFrm {fillx}
    labelEntry 0 $w.mid.fn {-text Filename} $init {}
    zpack $w.mid fn {expand fillx}
    $w.mid.flist.list insert end ..
    if ![catch {set fls [glob *]}] {
	foreach fl [lsort $fls] {
	    $w.mid.flist.list insert end $fl
	}
    }
    bind $w.mid.flist.list <1> "
	set x \[%W nearest %y\]
	set f \[%W get \$x\]
	entrySet $w.mid.fn.entry \
	 \[expr {\[file isdirectory \$f\] ? {} : \$f }\]
	%W select from \$x
    "
    set cmd [lindex [lindex $args 0] 1]
    bind $w.mid.flist.list <Double-1> "setFile %W %y {$cmd}"
    if {$args != {}} {
	set arg [lindex $args 0]
	frame $w.bot.0 -relief raised -border 1
	zpack $w.bot 0 {left expand fillx}
	set cmd [lindex $arg 1]
	if {$cmd != {}} { append cmd " \[$w.mid.fn.entry get\]" }
	button $w.bot.0.button -text [lindex $arg 0] \
		-command "$cmd ; killWindow $w ; notIdle {}"
	zpack $w.bot.0 button {expand fillx padx 12 pady 12}
	bind $w <Return> "$cmd ; killWindow %W ; notIdle {}"
	focus $w

	set i 1
	foreach arg [lrange $args 1 end] {
	    set cmd [lindex $arg 1]
	    if {$cmd != {}} { append cmd " \[$w.mid.fn.entry get\]" }
	    button $w.bot.$i -text [lindex $arg 0] \
	      -command "$cmd ; killWindow $w"
	    zpack $w.bot $i {left expand fillx}
	    incr i
	}
    }
    bind $w <Any-Enter> {focus %W ; notIdle {}}
}
