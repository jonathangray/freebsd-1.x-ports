#
# Adjust the configuration and rc file.
#
proc confInit {which} {
    global cVars
    global new
    foreach v $cVars($which) { global $v ; set new(${v}) [set $v] }
}

proc saveChan {chan} {
    if {${chan} == {} } { return }
    global new
    if {${chan} == "*DEFAULT*"} {
	set new(history) [.@confChannels.chan.values.history.entry get]
	set new(closeTime) \
	  [expr {[.@confChannels.chan.values.close.entry get] * 1000}]
    } {
	if {[set dx [listmatch $new(channelInfo) ${chan}]] < 0} { return }
	set chin [lindex $new(channelInfo) $dx]
	set sv 0
	set v1 [.@confChannels.chan.values.history.entry get]
	if {$v1 != [lindex $chin 4]} { listupdate chin 4 $v1 ; set sv 1}
	set v1 [.@confChannels.chan.values.close.entry get]
	if {$v1 != [lindex $chin 5]} { listupdate chin 5 $v1 ; set sv 1}
	set v1 [.@confChannels.chan.values.icon1.entry get]
	set v2 [.@confChannels.chan.values.icon2.entry get]
	if {[set v1 [list $v1 $v2]] == {{} {}}} { set v1 {} }
 	if {$v1 != [lindex $chin 6]} { listupdate chin 6 $v1 ; set sv 1}
	set v1 [.@confChannels.chan.values.log.entry get]
 	if {$v1 != [lindex $chin 8]} { listupdate chin 8 $v1 ; set sv 1}
	if $sv { listupdate new(channelInfo) $dx $chin }
    }
}

proc confCopyBack {which} {
    global cVars
    global confChange
    global confCSel
    global new
    case $which {
    Channels { saveChan $confCSel }
    Info {
	    set new(helpService) [.@confInfo.misc1.help.entry get]
	    set new(signoff) [.@confInfo.misc1.soff.entry get]
	}
    Channels {
	    set new(closeTime) [expr {$new(closeTime) * 1000}]
	}
    }
    foreach v $cVars($which) {
	global $v
	if {[set $v] != $new($v)} {
	    set $v $new(${v})
	    set confChange 1
	}
	unset new($v)
    }
    if $confChange {
	case $which {
	People { setupUsers }
	IRC {
		global nicks
		global ircnames
		global servers
		global defaultPort
		set name .oFrm.nSFrm
		$name.nickname.label.menu delete 0 last
		foreach nn $nicks {
		    $name.nickname.label.menu add command -label "$nn" \
		      -command "changeNickname {$nn}"
		}
		$name.ircname.label.menu delete 0 last
		foreach nn $ircnames {
		    $name.ircname.label.menu add command -label "$nn" \
		      -command "changeIRCName {$nn}"
		}
		$name.server.label.menu delete 0 last
		foreach nn $servers {
		    if {[set p [lindex $nn 1]] == ""} { set p $defaultPort}
		    set sn [lindex $nn 0]
		    $name.server.label.menu add command \
		      -label $sn -command "changeServer $sn $p"
		}
	    }
	Info {
		global Open ; global popInfo ; set Open(@info) $popInfo
		global Close ; global closeInfo ; set Close(@info) $closeInfo
	    }
	Channels {
		global channelInfo
		global CInfo
		catch "unset CInfo"
		while \
		  {[.oFrm.bf2.channels.menu entryconfigure last] != {}} {
		    .oFrm.bf2.channels.menu delete last
		}
		foreach chn $channelInfo {
		    set cn [lindex $chn 0]
		    set CInfo(${cn}) [lrange $chn 1 end]
		    if {[lsearch [getAuto ${cn}] menu] >= 0} {
			.oFrm.bf2.channels.menu add command -label ${cn} \
			  -command "channelJoin ${cn}"
		    }
		    if [winfo exists .${cn}] { setChanVars ${cn} }
		}
	    }
	    global history ; global History ; set History(@info) $history
	    global CloseTime ; set CloseTime(@info) $closeTime
	    global CloseCount ; set CloseCount(@info) $closeTime
	}
    }
}

proc confApply which {confCopyBack $which ; confInit $which}

proc confRevert which { killWindow .@conf${which} ; conf${which} }

proc forceSave {} {
    global confChange
    set confChange 1
    confSave all
}

proc confSave which {
    global confChange
    if {$which != "all"} { confCopyBack $which}
    if $confChange {
	set rc [glob ~]/.zirconrc
	if [file exist $rc] { exec mv ${rc} ${rc}.bak }
	set desc [open $rc w]
	global cVars
	global confData
	foreach x [array names cVars] {
	    foreach v $cVars($x) {
		global $v
		case $v {
		closeTime {
			puts $desc "set closeTime [expr {$closeTime / 1000}]"
		    }
		ignores {	
			foreach p $ignores {
			    puts $desc "ignore {[lindex $p 0]} [lindex $p 1]"
			}
		    }
		default {
			if {[lsearch $confData(single) $v] >= 0} {
			    puts $desc "set $v {[set $v]}"
			} {
		            puts $desc "set $v {"
		            foreach x [set $v] { puts $desc "	{$x}" }
		            puts $desc "}"
			}
		    }
		}
	    }
	}
	global DEBUG ; if $DEBUG { puts $desc "set DEBUG 1" }
	global OnCode
	foreach x [array names OnCode] {
	    foreach on $OnCode($x) { puts $desc "on $x $on" }
	}
	global bindings
	if {$bindings != {}} {
	    foreach on $bindings { puts $desc "zbind {} $on\n" }
	}
	global trust
	foreach x [array names trust] {
	    puts $desc "set trust($x) {$trust($x)}"
	}
	close $desc
    }
    killWindow .@conf${which}
    set confChange 0
}

proc confOK which {
    global cVars
    global new
    foreach v $cVars($which) { catch "unset new(${v})" }
    killWindow .@conf${which}
}

proc doCAN {pos name lst win dflt val} {
    if {$val == {}} return
    global new
    set x [lsearch $new($name) "$val"]
    set lvl [expr {$name == "ircnames" ? [list $val] : $val}]
    set edit [expr {$pos != ([$win size] -1)}]
    if {$x >= 0}  {
	if {$edit && $x != $pos} { return }
	if $dflt {
	    $win delete $x
	    $win insert 0 [expr {$lst ? [lindex $val 0] : $val}]
	    listmove new($name) $x 0 $lvl
	}
    } {
	if $dflt {
	    if $edit { listdel new($name) $pos ; $win delete $pos }
	    set new($name) [linsert $new($name) 0 $lvl]
	    $win insert 0 [expr {$lst ? [lindex $val 0] : $val}]
	} {
	    if $edit {
		listupdate new($name) $pos $lvl
		$win delete $pos
		$win insert $pos [expr {$lst ? [lindex $val 0] : $val}]
	    } {
		lappend new($name) $lvl
		$win insert [expr {[$win size] - 1}] \
		  [expr {$lst ? [lindex $val 0] : $val}]
	    }
	}
    }
}

proc confAddNickname {win y} {
    set val [$win get [set pos [$win nearest $y]]]
    if {$val == "*NEW*"} {
	mkEntryBox .@can "New Nick" "Enter the new nickname:" \
	  {{Nickname {}}} "OK {doCAN $pos nicks 0 $win 0}" \
	  "Default {doCAN $pos nicks 0 $win 1}" {Cancel {}}
    } {
	$win select from $pos
	mkEntryBox .@can "Edit Nick" "Edit the nickname:" \
	  "{Nickname {$val}}" "OK {doCAN $pos nicks 0 $win 0}" \
	  "Default {doCAN $pos nicks 0 $win 1}" "Delete {confDel nicks $win}"\
	  {Cancel {}}
    }
}

proc updateFriend {posn val ntfy} {
    global new
    set p [lindex $new(userInfo) $posn]
    set q [lindex $p 1]
    set ix [lsearch $q notify]
    if $ntfy {
	if {$ix < 0} { lappend q notify }
    } {
	if {$ix >= 0} { listdel q $ix }
    }
    listupdate new(userInfo) $posn [list $val $q]
}

proc confAddIRCName {win y} {
    set val [$win get [set pos [$win nearest $y]]]
    if {$val == "*NEW*"} {
	mkEntryBox .@cain {New IRCName} {Enter a new IRC name:} \
	  {{IRCName {}}} "OK {doCAN $pos ircnames 0 $win 0}" \
	  "Default {doCAN $pos ircnames 0 $win 1}" {Cancel {}}
    } {
	$win select from $pos
	mkEntryBox .@cain {Edit IRCName} {Edit the IRC name:} \
	  [list [list IRCName $val]] "OK {doCAN $pos ircnames 0 $win 0}" \
	  "Default {doCAN $pos ircnames 0 $win 1}" \
	  "Delete {confDel ircnames $win}" {Cancel {}}
    }
}

proc doCAS {pos win dflt hst prt onk opw} {
    if {$hst == {}} return
    global new
    global defaultPort
    set val $hst
    if {$opw != {}} {
	lappend val $prt [list $onk $opw]
    } {
	if {$prt != $defaultPort} { lappend val $prt }
    }
    doCAN $pos servers 1 $win $dflt $val
}

proc confDel {var win args} {
    if {[set t [$win curselection]] != {}} {
	global new
	set size [expr {[$win size] - 1}]
	if {[set l [lindex $t 0]] < $size} {
	    set cl [expr {[llength $t] - 1}]
	    while {[set m [lindex $t $cl]] == $size} { incr cl -1 }
	    set new($var) [lreplace $new($var) $l $m]
	    $win delete $l $m
	}
    }
}

proc confAddServer {win y} {
    set val [$win get [set pos [$win nearest $y]]]
    if {$val == "*NEW*"} {
	global defaultPort
	mkEntryBox .@cas {New Server} {Enter the new server details:} \
	  "{Hostname {}} {Port {$defaultPort}} {{Op Nick} {}} \
	  {{Op passwd} {}}" \
	  "OK {doCAS $pos $win 0}" "Default {doCAS $pos $win 1}" {Cancel {}}
    } {
	$win select from $pos
	global new
	set v [lindex $new(servers) $pos]
	set os [lindex $v 2]
	mkEntryBox .@cas {Edit Server} {Edit the server details:} \
	  [list [list Hostname $val] [list Port [lindex $v 1]] \
	  [list {Op Nick} [lindex $os 0]] [list {Op passwd} [lindex $os 1]]] \
	  "OK {doCAS $pos $win 0}" "Default {doCAS $pos $win 1}" \
	  "Delete {confDel servers $win}" {Cancel {}}
    }
}

proc confEnt {win var title lst} {
    global $var
    set name [string tolower $title]
    set winn $win.$name
    set winnl $winn.list
    frame $winn -relief raised
    label $winn.label -text "${title}s"
    frame $winnl -relief raised
    scrollbar $winnl.vscroller -command "$winnl.values yview"

    listbox $winnl.values -xscrollcommand "$winn.hsf.hscroller set" \
      -yscrollcommand "$winnl.vscroller set" -setgrid 1 -relief flat
    foreach v [set $var] {
	$winnl.values insert end [expr {$lst ? [lindex $v 0] : $v}]
    }
    $winnl.values insert end *NEW*
    bind $winnl.values <Double-Button-1> "confAdd${title} %W %y"
    bind $winnl.values <Delete> " confDel ${var} %W"
    bind $winnl.values <BackSpace> " confDel ${var} %W"
    bind $winnl.values <Control-h> " confDel ${var} %W"
    zpack $winnl values {left expand fill}
    zpack $winnl vscroller {left filly}

    frame $winn.hsf
    scrollbar $winn.hsf.hscroller -orient horizontal \
      -command "$winnl.values xview"

    frame $winn.hsf.pf0

    zpack $winn.hsf hscroller {left expand fillx}
    zpack $winn.hsf pf0 {left padx 20} 
    zpack $winn label {top}
    zpack $winn list {expand fill}
    zpack $winn hsf {fillx}
    pack append $win $winn {left expand fill}
    bind $winn <Enter> "focus $winnl.values"
}
proc confMkBtn {win type} {
    frame $win.btn
    foreach bt {Revert Apply Save OK} {
	set lbt [string tolower $bt]
	button $win.btn.$lbt -command "conf$bt $type" -text $bt
	zpack $win.btn $lbt {left expand fillx}
    }
}

proc confIRC {} {
    set win .@confIRC
    if [winfo exists $win] { wm deiconify $win ; raise $win ; return }
    toplevel $win -class Zircon
    wm title $win "IRC Configuration"
    confInit IRC
    frame $win.data
    confEnt $win.data nicks Nickname 0
    confEnt $win.data ircnames IRCName 0
    confEnt $win.data servers Server 1
    confMkBtn $win IRC
    zpack $win data {expand fill}
    zpack $win btn {fillx}
}

proc doCInfo {indx val} {
    global confI${indx}
    global new
    if {[set confI${indx}]} {
	lappend new(toInfo) $val
    } {
	listdel new(toInfo) [lsearch $new(toInfo) $val]
    }
}

proc doCNConf {indx val} {
    global confNC${indx}
    global new
    if {[set confNC${indx}]} {
	lappend new(noConfirm) $val
    } {
	listdel new(noConfirm) [lsearch $new(noConfirm) $val]
    }
}

proc doConfButton indx {
    global confCSel
    if {$confCSel != {}} {
	global confB
	global zircon
	global new
	set l [llength $zircon(autos)]
	if {$confCSel == "*DEFAULT*"} {
	    if {$indx < $l} {
		set var [lindex {{} popUp popDown {} noDraw noJump quiet} $indx]
		set new($var) $confB($indx)
	    } {
		global confData
		set v \
		  [string toupper [lindex $confData(msg) [expr {$indx - $l}]]]
		set vdx [lsearch $new(noMessage) $v]
		if !$confB($indx) {
		    if {$vdx < 0} { lappend new(noMessage) $v }
		} {
		    if {$vdx >= 0} { listdel new(noMessage) $vdx }
		}
	    }
	    return
	}
	set dx [listmatch $new(channelInfo) ${confCSel}]
	set chin [lindex $new(channelInfo) $dx]
	if {$indx >= $l} {
	    global confData
	    set v [string toupper [lindex $confData(msg) [expr {$indx - $l}]]]
	    set msgs [lindex $chin 2]
	    set vdx [lsearch $msgs $v]
	    set nvdx [lsearch $msgs "!$v"]
	    if !$confB($indx) {
		if {$vdx < 0} {
		    if {$nvdx >= 0} {
			listupdate msgs $nvdx $v
		    } {
			lappend msgs $v
		    }
		}
	    } {
		if {$nvdx < 0} {
		    if {$vdx >= 0} {
			listupdate msgs $vdx "!$v"
		    } {
			lappend msgs "!$v"
		    }
		}
	    }
	    listupdate chin 2 $msgs
	} {
	    set autos [lindex $chin 1]
	    set v [lindex $zircon(autos) $indx]
	    set vdx [lsearch $autos $v]
	    if $confB($indx) {
		if {$vdx < 0} { lappend autos $v }
	    } {
		if {$vdx >= 0} { listdel autos $vdx }
	    }
	    listupdate chin 1 $autos
	}
	listupdate new(channelInfo) $dx $chin
    }
}

proc doCAF {pos win ntfy val} {
    if {$val == {}} return
    global new
    set edit [expr {$pos != ([$win size] - 1)}]
    if {[set x [listmatch $new(userInfo) $val]] >= 0}  {
	if {$edit && $x != $pos} { return }
	updateFriend $x $val $ntfy
    } {
	if $edit {
	    updateFriend $pos $val $ntfy
	    $win delete $pos
	    $win insert $pos $val
	} {
	    lappend new(userInfo) [list $val [expr {$ntfy ? {notify} : {}}]]
	    $win insert [expr {[$win size] - 1}] $val
	}
    }
}

proc confAddFriend {win y} {
    set val [$win get [set pos [$win nearest $y]]]
    if {$val == "*NEW*"} {
	mkEntryBox .@can {New Friend} {Enter the new friend's nickname:} \
	  {{Nickname {}}} "OK {doCAF $pos $win 0}" \
	  "{Notify On} {doCAF $pos $win 1}" {Cancel {}}
    } {
	$win select from $pos
	global new
	set nf [expr \
	  {[lsearch [lindex [lindex $new(userInfo) $pos] 1] notify] < 0}]
	mkEntryBox .@can {Edit Friend} {Edit the Friend's nickname:} \
	  "{Nickname {$val}}" "OK {doCAF $pos $win [expr {!$nf}]}" \
	  [list "Notify [expr {$nf ? {On} : {Off}}]" "doCAF $pos $win $nf"] \
	  "Delete {confDel userInfo $win}" {Cancel {}}
    }
}

proc doCAI {pos win val} {
    if {$val == {}} return
    global new
    set x [listmatch $new(ignores) $val]
    set edit [expr {$pos != ([$win size] -1)}]
    if $edit {
	if {$x >= 0 && $x != $pos} { return }
	set v [lindex $new(ignores) $pos]
	listupdate new(ignores) $pos [list $val [lindex $v 1]]
	$win delete $pos
	$win insert $pos $val
    } {
	if {$x < 0} {
	    lappend new(ignores) [list $val {}]
	    $win insert [expr {[$win size] - 1}] $val
	}
    }
}

proc doConfIgnore {indx} {
    global confISel
    if {$confISel != {}} {
	global confI
	global zircon
	global new
	set dx [listmatch $new(ignores) ${confISel}]
	set chin [lindex $new(ignores) $dx]
        set val [lindex $chin 1]
	set vdx [lsearch $val $indx]
	if !$confI($indx) {
	    if {$vdx >= 0} { listdel val $vdx }
	} {
	    if {$vdx < 0} { lappend val $indx }
	}
	if {$dx >= 0} {
	    listupdate new(ignores) $dx [list $confISel $val]
	} {
	    lappend new(ignores) [list $confISel $val]
	}
    }
}

proc confAddIgnore {win y} {
    set val [$win get [set pos [$win nearest $y]]]
    if {$val == "*NEW*"} {
	mkEntryBox .@ci {New ignore} {Enter the nickname/username to ignore:} \
	  {{Nickname {}}} "OK {doCAI $pos $win}" {Cancel {}}
    } {
	$win select from $pos
	mkEntryBox .@ci {Edit Nick} {Edit the nickname:} \
	  "{Nickname {$val}}" "OK {doCAI $pos $win}" \
	  "Delete {confDel ignores $win}" {Cancel {}}
    }
}

proc changeIgnore {dbl win y} {
    global confISel
    if {[set confISel [$win get [set p [$win nearest $y]]]] == "*NEW*"} {
	if $dbl {
	    confAddIgnore $win
	} {
	    global zircon
	    foreach b $zircon(ignore) {
		set l [string tolower $b]
		.@confPeople.data.idata.ignore.ign2.$l configure -state disabled
	    }
	    $win select clear  ; set confISel ""
	}
    } {
	$win select from $p
	setIB $confISel
    }
}

proc setIB {nk} {
    global zircon
    global new
    global confI
    set modes [lindex [lindex $new(ignores) [listmatch $new(ignores) $nk]] 1]
    foreach b $zircon(ignore) {
	set lb [string tolower $b]
	set confI($lb) [expr {[lsearch $modes $lb] >= 0}]
	.@confPeople.data.idata.ignore.ign2.$lb configure -state normal
    }
}

proc confPeople {} {
    set win .@confPeople
    if [winfo exists $win] { wm deiconify $win ; raise $win ; return }
    toplevel $win -class Zircon
    wm title $win "People Configuration"
    confInit People
    global confISel ; set confISel ""
    frame $win.data
    frame $win.data.fdata -relief raised
    confEnt $win.data.fdata userInfo Friend 1
    set winn [frame $win.data.idata -relief raised]
    label $winn.label -text "Ignores"
    frame $winn.ignore
    set winn1 [frame $winn.ignore.ign1]
    set winnl [frame $winn1.list -relief raised]
    scrollbar $winnl.vscroller -command "$winnl.values yview"

    listbox $winnl.values -xscrollcommand "$winn1.hsf.hscroller set" \
      -yscrollcommand "$winnl.vscroller set" -setgrid 1 -relief flat
    global ignores
    foreach v $ignores { $winnl.values insert end [lindex $v 0] }
    $winnl.values insert end *NEW*
    bind $winnl.values <ButtonPress-1> "changeIgnore 0 %W %y "
    bind $winnl.values <Double-Button-1> "confAddIgnore %W %y "
    bind $winnl.values <Delete> "confDel ignores %W"
    bind $winnl.values <BackSpace> "confDel ignores %W"
    bind $winnl.values <Control-h> "confDel ignores %W"
    zpack $winnl values {left expand fill}
    zpack $winnl vscroller {left filly}
    bind $winn1 <Enter> "focus $winnl.values"
    frame $winn.ignore.ign1.hsf
    scrollbar $winn.ignore.ign1.hsf.hscroller -orient horizontal \
      -command "$winnl.values xview"

    frame $winn1.hsf.pf0

    zpack $winn1.hsf hscroller {left expand fillx}
    zpack $winn1.hsf pf0 {left padx 20} 
    zpack $winn1 list {expand fill}
    zpack $winn1 hsf {fillx}
    frame $winn.ignore.ign2
    global zircon
    foreach v $zircon(ignore) {
	set lv [string tolower $v]
	checkbutton $winn.ignore.ign2.$lv -text $v -state disabled \
	  -variable confI($lv) -command "doConfIgnore ${lv}"
	zpack  $winn.ignore.ign2 $lv {frame w}
    }
    zpack $winn.ignore {ign1 ign2} {left expand fill}
    zpack $winn {label ignore} {top}
    confMkBtn $win People
    zpack $win.data {fdata idata} {left expand fill}
    zpack $win data {expand fill}
    zpack $win btn {fillx}
}

proc editChan {pos win chan} {
    global confCSel
    set chan [string tolower ${chan}]
    if {${chan} == {} || ${chan} == $confCSel} return
    $win delete $pos
    $win insert $pos ${chan}
    $win select from $pos
    global new
    set x [listmatch $new(channelInfo) $confCSel]
    set chn [lindex $new(channelInfo) $x]
    listupdate chn 0 ${chan}
    listupdate new(channelInfo) $x $chn
    set confCSel ${chan}
    setCCB $confCSel
}

proc changeChan {dbl win y} {
    global confCSel
    if {$confCSel != {}} { saveChan $confCSel }
    if {[set confCSel [$win get [set p [$win nearest $y]]]] == "*NEW*"} {
	if $dbl { confAddChan $win } { $win select clear  ; set confCSel "" }
    } {
	$win select from $p
	if {$dbl && $confCSel != "*DEFAULT*"} {
	    mkEntryBox .@can {Edit Channel} {Edit the channel name:} \
	      "{Channel {$confCSel}}" "OK {editChan $p $win }" \
	      "Delete {confDelChan $win $y}" {Cancel {}}
	}
    }
    setCCB $confCSel
}

proc setCCB chan {
    global zircon
    global new
    global confData
    global confB
    if {${chan} == {}} {
	set i 0
	foreach w [winfo children .@confChannels.chan.options] {
	    if {$w != ".@confChannels.chan.options.msg"} {
		$w conf -state disabled
		set confB($i) 0
		incr i
	    }
	}
	foreach i {history close icon1 icon2 log} {
	   entrySet .@confChannels.chan.values.${i}.entry ""
	   .@confChannels.chan.values.${i}.entry conf -state disabled
	}
	return
    }
    foreach w [winfo children .@confChannels.chan.options] {
	catch "$w conf -state normal"
    }
    foreach i {history close icon1 icon2 log} {
	.@confChannels.chan.values.${i}.entry conf -state normal
    }
    if {$chan == "*DEFAULT*"} {
	foreach i {0 3} {
	    .@confChannels.chan.options.b${i} configure -state disabled
	    set confB($i) 0
	}
	set confB(1) $new(popUp)
	set confB(2) $new(popDown)
	set confB(4) $new(noDraw)
	set confB(5) $new(noJump)
	set confB(6) $new(quiet)
	set i 7
	foreach b $confData(msg) {
	    set b [string toupper $b]
	    set confB($i) [expr {[lsearch $new(noMessage) ${b}] < 0}]
	    incr i
	}
	entrySet .@confChannels.chan.values.history.entry $new(history)
	entrySet .@confChannels.chan.values.close.entry \
	  [expr {$new(closeTime) / 1000}]
	foreach i {icon1 icon2 log} {
	   .@confChannels.chan.values.${i}.entry conf -state disabled
	}
	return
    }
    set chin [lindex $new(channelInfo) [listmatch $new(channelInfo) ${chan}]]
    set modes [lindex $chin 1]
    set i 0
    foreach b $zircon(autos) {
	set confB($i) [expr {[lsearch $modes $b] >= 0}]
	incr i
    }
    set cm [lindex $chin 2]
    foreach b $confData(msg) {
	set b [string toupper $b]
	set confB($i) \
	  [expr {[lsearch $cm "!$b"] >= 0 || \
	  ([lsearch $cm $b] < 0 && [lsearch $new(noMessage) $b] < 0)}]
	incr i
    }
    entrySet .@confChannels.chan.values.history.entry [lindex $chin 4]
    entrySet .@confChannels.chan.values.close.entry [lindex $chin 5]
    set v [lindex $chin 6]
    entrySet .@confChannels.chan.values.icon1.entry [lindex $v 0]
    entrySet .@confChannels.chan.values.icon2.entry [lindex $v 1]
    entrySet .@confChannels.chan.values.log.entry [lindex $chin 8]
}

proc doCAC {win chan} {
    if {${chan} != {}} {
	global new
	set chan [string tolower ${chan}]
	if {[set x [listmatch $new(channelInfo) ${chan}]] < 0} {
	    set x [expr {[$win size] - 2}]
	    $win insert $x ${chan}
	    lappend new(channelInfo) [list ${chan} {} {} {} {} {} {} {} {}]
	}
	global confCSel ; set confCSel ${chan}
	$win select from $x
	setCCB ${chan}
    }
}

proc confAddChan {win} {
    mkEntryBox .@cac {New Channel} {Enter the channel name:} \
      {{Channel {}}} "OK {doCAC $win}" {Cancel {}}
}

proc confDelChan {win y args} {
    if {[set dx [$win curselection]] == {}} { set dx [$win nearest y] }
    if {$dx < [expr {[$win size] - 2}]} {
	$win delete $dx
	global new ; listdel new(channelInfo) $dx
	global confCSel
	set confCSel [$win get $dx]
	$win select from $dx
	setCCB $confCSel
    }
}

proc confChannels {} {
    set win .@confChannels
    if [winfo exists $win] {
	wm deiconify $win
	raise 
	return
    }
    toplevel $win -class Zircon
    wm title $win {Channel Configuration}
    wm iconname $win Channels
    confInit Channels
    set winc [frame $win.chan -relief raised]
    set wincn [frame $winc.nels]
    label $wincn.label -text Channels
    frame $wincn.list
    scrollbar $wincn.list.vscroller -command "$wincn.list.values yview"
    listbox $wincn.list.values -xscrollcommand "$wincn.hsf.hscroller set" \
      -yscrollcommand "$wincn.list.vscroller set" -setgrid 1
    tk_listboxSingleSelect $wincn.list.values
    global channelInfo
    foreach c $channelInfo { $wincn.list.values insert end [lindex $c 0] }
    global confCSel
    $wincn.list.values insert end [set confCSel *DEFAULT*]
    $wincn.list.values insert end *NEW*
    $wincn.list.values select from [expr {[$wincn.list.values size] - 2}]
    bind $wincn.list.values <Delete> { confDelChan %W %y }
    bind $wincn.list.values <BackSpace> { confDelChan %W %y }
    bind $wincn.list.values <Control-h> { confDelChan %W %y }
    bind $wincn.list.values <Button-1> { changeChan 0 %W %y }
    bind $wincn.list.values <Double-Button-1> { changeChan 1 %W %y }
    zpack $wincn.list values {left expand fill}
    zpack $wincn.list vscroller {left expand filly}
    frame $wincn.hsf
    scrollbar $wincn.hsf.hscroller -orient horizontal \
      -command "$wincn.list.values xview"
    frame $wincn.hsf.pf0
    zpack $wincn.hsf hscroller {left expand fillx}
    zpack $wincn.hsf pf0 {left padx 20} 

    zpack $wincn label {top}
    zpack $wincn list {expand fill}
    zpack $wincn hsf {fillx}
    frame $winc.options
    set i 0
    foreach opt  {{Auto Join} {Pop Up} {Pop Down} {On Menu} \
      {No Draw} {No Jump} Quiet} {
	checkbutton $winc.options.b${i} -text $opt \
	  -variable confB($i) -command "doConfButton ${i}"
	zpack  $winc.options b${i} {frame w}
	incr i
    }
    label $winc.options.msg -text Messages
    zpack $winc.options msg {frame w}
    global confData
    foreach opt $confData(msg) {
	checkbutton $winc.options.b${i} -text $opt \
	  -variable confB($i) -command "doConfButton ${i}"
	zpack $winc.options b${i} {frame w}
	incr i
    }
    frame $winc.values
    labelEntry 0 $winc.values.history {-text History -width 12} "" ""
    bind $winc.values.history.entry <Return> {}
    bind $winc.values.history.entry <Any-KeyPress> {
	case %A {
	[0-9+-] { %W insert insert %A }
	}
    }
    labelEntry 0 $winc.values.close {-text "Close Time" -width 12} "" ""
    bind $winc.values.close.entry <Return> {}
    bind $winc.values.close.entry <Any-KeyPress> {
	case %A {
	[0-9+-] { %W insert insert %A }
	}
    }
    labelEntry 0 $winc.values.icon1 {-text Icon -width 12} "" ""
    labelEntry 0 $winc.values.icon2 {-text {Active Icon} -width 12} "" ""
    labelEntry 0 $winc.values.log {-text {Log File} -width 12} "" ""
    zpack $winc.values {history close icon1 icon2 log} {expand fillx}
    zpack $winc {nels options values} {left expand fill}
    setCCB *DEFAULT*
    bind $wincn <Enter> "focus $wincn.list.values"
    confMkBtn $win Channels
    zpack $win chan {expand fill}
    zpack $win btn {fillx}
}

set zircon(idata1) {
    { {Verbose CTCP} verboseCTCP }
    { {Pop Up Info} popInfo }
    { {Pop Down Info} closeInfo }
    { {Flag Pop Up} noPopup }
    { {No Channel List} noRefresh }
    { {Friends On} friendsOn }
    { {Kill Path} killPath }
}

proc confInfo {} {
    set win .@confInfo
    if [winfo exists $win] { wm deiconify $win ; raise $win ; return }
    global zircon
    global confData
    global new
    toplevel $win -class Zircon
    wm title $win "Info Configuration"
    confInit Info
    frame $win.misc0 -relief raised
    set i 0
    foreach d $zircon(idata1) {
	checkbutton $win.misc0.$i -text [lindex $d 0] \
	  -variable "new([lindex $d 1])"
	zpack $win.misc0 $i {left}
	incr i
    }
    frame $win.misc1
    labelEntry 0 $win.misc1.help {-text {Help Service}} $new(helpService) \
      { set new(helpService) [%W get]}
    labelEntry 0 $win.misc1.soff {-text {Sign Off}} $new(signoff) \
      { set new(signoff) [%W get]}
    zpack $win.misc1 {help soff} {left fillx }
    frame $win.misc2 -relief raised
    global confData
    global toInfo
    label $win.misc2.label -text "Send to Info :"
    zpack $win.misc2 label {left}
    set i 0
    foreach ci $confData(info) {
	set uci [string toupper $ci]
	checkbutton $win.misc2.inf${i} -text $ci \
	  -variable confI${i} -command "doCInfo ${i} ${uci}"
	global confI${i}
	set confI${i} [expr {[lsearch $toInfo $uci] >= 0}]
	zpack $win.misc2 inf${i} {left}
	incr i
    }
    frame $win.misc3 -relief raised
    label $win.misc3.label -text "No Confirm :"
    zpack $win.misc3 label {left}
    global confData
    global noConfirm
    set i 0
    foreach ci $confData(nconf) {
	set uci [string toupper $ci]
	checkbutton $win.misc3.inf${i} -text $ci \
	  -variable confNC${i} -command "doCNConf ${i} ${uci}"
	global confNC${i}
	set confNC${i} [expr {[lsearch $noConfirm $uci] >= 0}]
	zpack $win.misc3 inf${i} {left}
	incr i
    }
    frame $win.filter -relief raised

    checkbutton $win.filter.public -variable new(showPublic) -text "Public"
    checkbutton $win.filter.local -variable new(showLocal) -text "Local"
    checkbutton $win.filter.private -variable new(showPrivate) -text "Private"
    checkbutton $win.filter.topic -variable new(topicOnly) -text "With Topic"

    global minMembers ; set tmp $minMembers
    scale $win.filter.members \
      -from 1 -to 25 -label "Minimum Number of Members" \
      -showvalue 1 -orient horizontal \
      -command {global new ; set new(minMembers) }

    $win.filter.members set $tmp
    zpack $win.filter members {left expand fillx}
    zpack $win.filter {public local private topic} {left}

    labelEntry 0 $win.filter2 {-text {Channel Pattern} -width 16} \
      $new(listPattern) {set new(listPattern) [%W get]}
    labelEntry 0 $win.filter3 {-text {Topic Pattern} -width 16} \
      $new(topicPattern) {set new(topicPattern) [%W get]}
    confMkBtn $win Info
    zpack $win {misc0 misc1 misc2 misc3 filter filter filter2 filter3 btn} \
      {fillx}
}
