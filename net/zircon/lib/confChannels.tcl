#
proc editChan {pos win chan} {
    global selID newCname newClname
    set lchan [string tolower ${chan}]
    if {[string match {} $chan] || $lchan == [$chid $selID]} {
	return
    }
    $win delete $pos
    $win insert $pos ${chan}
    $win select from $pos
    set newCname($selID) ${chan}
    set newClname($selID) ${lchan}
}
#
proc changeChan {dbl win y} {
    global selID defChan
    saveChan
    if {[set cnm [$win get [set p [$win nearest $y]]]] == {*NEW*}} {
	if $dbl {
	    set selID nil
	    setCCB {}
	    confAddChan $win
	} {
	    incr p -1
	    $win select from $p
	    set selID $defChan
	    setCCB *DEFAULT*
	}
    } {
	set selID [Channel :: find $cnm]
	setCCB $cnm
	$win select from $p
	if {$dbl && $selID != $defChan} {
	    mkEntryBox .@can {Edit Channel} {Edit the channel name:} \
	      "{Channel {$cnm}}" "OK {editChan $p $win }" \
	      "Delete {confDelChan $win $y}" {Cancel {}}
	    tkwait window .@can
	}
    }
}
#
proc setCCB {chan args} {
    global confData confB newChistory newCkey newCicon newClogfile \
      newCmsg newCkey newCclosetime
    set w .@confChannels.chan
    if [string match {} ${chan}] {
	foreach wc [winfo children $w.options] {
	    if {$wc != "$w.options.msg"} { $wc conf -state disabled }
	}
	foreach i {history closetime icon1 icon2 logfile key} {
	   $w.values.$i.entry conf -state disabled
	}
	return
    }
    foreach wc [winfo children $w.options] {
	if {$wc != "$w.options.msg"} { $wc conf -state normal }
    }
    foreach wc [winfo children $w.values] {
	$wc.entry conf -state normal
    }
    set chan [Channel :: find ${chan}]
    foreach v $confData(channel) {
	set tn [lindex $v 1]
	global newC${tn}
	$w.options.$tn configure -variable newC${tn}($chan)
    }
    foreach b $confData(msg) {
	set b [string toupper $b]
	set lb [string tolower $b]
	set confB($lb) [expr {[lsearch $newCmsg($chan) $b] < 0}]
    }
    entrySet $w.values.history.entry $newChistory($chan)
    entrySet $w.values.closetime.entry $newCclosetime($chan)
    set v $newCicon($chan)
    entrySet $w.values.icon1.entry [lindex $v 0]
    entrySet $w.values.icon2.entry [lindex $v 1]
    entrySet $w.values.logfile.entry $newClogfile($chan)
    entrySet $w.values.key.entry $newCkey($chan)
}
#
proc CancelCAC {win args} {
    global selID defChan
    set selID $defChan
    setCCB *DEFAULT*
    $win select from [expr [$win size] - 2]
}
#
proc doCAC {win chan} {
    if ![string match {} ${chan}] {
	global newCname
	set chid [Channel :: make ${chan}]
	if ![info exists newCname($chid)] {
	    set x [expr {[$win size] - 2}]
	    $win insert $x ${chan}
	    $chid pack new
	}
	global selID
	set selID $chid
	$win select from $x
	setCCB ${chan}
    }
}
#
proc confAddChan {win} {
    mkEntryBox .@cac {New Channel} {Enter the channel name:} \
      {{Channel {}}} "OK {doCAC $win}" "Cancel {CancelCAC $win}"
    tkwait window .@cac
}
#
proc confDelChan {win y args} {
    if {[set dx [$win curselection]] == {}} { set dx [$win nearest y] }
    if {$dx < [expr {[$win size] - 2}]} {
	$win delete $dx
	global selID
	foreach prop {open close jump join draw quiet menu msg \
	  history closetime key icon logfile name lname} {
	    global newC${prop}
	    catch {unset newC${prop}($selID)}
	}
	$selID configure -keep 0
	if ![$selID active] { $selID delete }
	set cnm [string tolower [$win get $dx]]
	$win select from $dx
	set selID [Channel :: find $cnm]
	setCCB $cnm
    }
}
#
proc confChannels {} {
    set win .@confChannels
    if [winfo exists $win] { popup $win ; return }
    confInit Channels
    toplevel $win -class Zircon
    wm title $win {Channel Configuration}
    wm iconname $win Channels
    set winc [frame $win.chan -relief raised]
    set wincn [frame $winc.nels]
    label $wincn.label -text Channels
    makeLB $wincn.list -setgrid 1
    tk_listboxSingleSelect $wincn.list.l
    global confData selID defChan
    foreach c [Channel :: list] {
	if {$c != $defChan && [$c isa Channel]} {
	    $wincn.list.l insert end [$c name]
	}
    }
    set selID $defChan
    $wincn.list.l insert end *DEFAULT*
    $wincn.list.l insert end *NEW*
    $wincn.list.l select from [expr {[$wincn.list.l size] - 2}]
    bind $wincn.list.l <Delete> { confDelChan %W %y }
    bind $wincn.list.l <BackSpace> { confDelChan %W %y }
    bind $wincn.list.l <Control-h> { confDelChan %W %y }
    bind $wincn.list.l <Button-1> { changeChan 0 %W %y }
    bind $wincn.list.l <Double-Button-1> { changeChan 1 %W %y }
    pack $wincn.label
    pack $wincn.list -expand 1 -fill both
    frame $winc.options
    foreach opt $confData(channel) {
	set tn [lindex $opt 1]
	global newC${tn}
	checkbutton $winc.options.$tn -text [lindex $opt 0] \
	  -variable newC${tn}(*default*)
	pack $winc.options.$tn -anchor w
    }
    label $winc.options.msg -text Messages
    pack $winc.options.msg -anchor w
    foreach opt $confData(msg) {
	set val [string tolower $opt]
	checkbutton $winc.options.msg$val -text $opt \
	  -variable confB($val) -command "doConfButton $val"
	pack $winc.options.msg$val -anchor w
    }
    frame $winc.values
    labelEntry 0 $winc.values.history {-text History -width 12} {} {}
    bind $winc.values.history.entry <Return> {}
    bind $winc.values.history.entry <Any-KeyPress> {
	case %A {
	[0-9+-] { %W insert insert %A }
	}
    }
    labelEntry 0 $winc.values.closetime {-text {Close Time} -width 12} {} {}
    bind $winc.values.closetime.entry <Return> {}
    bind $winc.values.closetime.entry <Any-KeyPress> {
	case %A {
	[0-9+-] { %W insert insert %A }
	}
    }
    labelEntry 0 $winc.values.icon1 {-text Icon -width 12} {} {}
    labelEntry 0 $winc.values.icon2 {-text {Active Icon} -width 12} {} {}
    labelEntry 0 $winc.values.logfile {-text {Log File} -width 12} {} {}
    labelEntry 0 $winc.values.key {-text Key -width 12} {} {}
    pack $winc.values.history $winc.values.closetime $winc.values.icon1 \
      $winc.values.icon2 $winc.values.logfile $winc.values.key -expand 1 -fill x
    pack $winc.nels $winc.options $winc.values -side left -expand 1 -fill both
    setCCB *DEFAULT*
    bind $wincn <Enter> "focus $wincn.list.l"
    confMkBtn $win Channels
    pack $win.chan -expand 1 -fill both
    pack $win.btn -fill x
}
#
proc saveChan {} {
    global newCicon selID
    if [string match {nil} $selID] { return }
    set w .@confChannels.chan.values
    foreach e {closetime history logfile key} {
	set v [$w.$e.entry get]
	global newC${e}
	if {[set newC${e}($selID)] != $v} {
	    set newC${e}($selID) $v
	}
    }
    set v1 [$w.icon1.entry get]
    set v2 [$w.icon2.entry get]
    if {[set v1 [list $v1 $v2]] == {{} {}}} { set v1 {} }
    if {$v1 != $newCicon($selID)} { set newCicon($selID) $v1 }
}
#
proc doConfButton {indx} {
    global selID defChan
    if ![string match {nil} $selID] {
	global confB newCmsg
	set vdx [lsearch $newCmsg($selID) [set v [string toupper $indx]]]
	if !$confB($indx) {
	    if {$vdx < 0} { lappend newCmsg($selID) $v }
	} {
	    if {$vdx >= 0} { listdel newCmsg($selID) $vdx }
	}
    }
}
#
proc copybackChan {} {
    global newCname confChange closeTime defChan
    saveChan
    foreach ch [array names newCname] { $ch unpack new }
    set closeTime [expr [$defChan closetime] * 1000]
    set confChange 1
}
