#
# Adjust configuration and rc file.
#
#
proc confInit {which} {
    global cVars new
    foreach v $cVars($which) {
	global $v
	set new($v) [set $v]
    }
    switch $which {
    People {
	    global newUsr delUsr
	    set newUsr {}
	    set delUsr {}
	    User :: pack new
	}
    Channels { Channel :: pack new }
    IRC {
	    global newSv delSv
	    set newSv {}
	    set delSv {}
	    Server :: pack new
	}
    }
}
#
proc confCopyBack {which} {
    global cVars confChange defChan new
    switch $which {
    Channels { copybackChan }
    IRC {
	    global newSv delSv
	    Server :: unpack new
	    set confChange 1
	    set newSv {}
	    foreach v $delSv { $v delete }
	    set delSv {}
	}
    People {
	    User :: unpack new
	}
    Info {
	    set new(helpService) [.@confInfo.misc1.help.entry get]
	    set new(signoff) [.@confInfo.misc1.soff.entry get]
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
		global nicks ircnames
		set name .oFrm.nSFrm
		$name.nickname.label.menu delete 0 last
		foreach nn [lsort $nicks] {
		    $name.nickname.label.menu add command -label "$nn" \
		      -command "changeNickname {$nn}"
		}
		$name.ircname.label.menu delete 0 last
		foreach nn [lsort $ircnames] {
		    $name.ircname.label.menu add command -label "$nn" \
		      -command "changeIRCName {$nn}"
		}
		$name.server.label.menu delete 0 last
		foreach nn [lsort [Server :: list]] {
		    $name.server.label.menu add command \
		       -label [$nn name] -command "changeServer $nn"
		}
	    }
	Info {
		global popInfo
		info0 configure -open $popInfo
	    }
	Channels {
		while \
		  {[.oFrm.bf2.channels.menu entryconfigure last] != {}} {
		    .oFrm.bf2.channels.menu delete last
		}
		foreach chn [lsort [Channel :: list]] {
		    if [$chn menu] {
			.oFrm.bf2.channels.menu add command \
			-label [$chn name] -command "$chn sendJoin"
		    }
		}
	    }
	    global defChan
	    info0 configure -history [$defChan history] \
	       -closetime [$defChan $closetime]
	}
    }
}
#
proc confApply {which} {
    confCopyBack $which
    confInit $which
}
#
proc confRevert {which} { 
    destroy .@conf$which
    conf$which
}
#
proc confSave {which} {
    global confChange
    if ![string match {all} $which] { confCopyBack $which }
    if $confChange { saverc }
    killWindow .@conf${which}
    set confChange 0
    confOK $which
}
#
proc confOK {which} {
    global cVars new newObj
    foreach v $cVars($which) { catch {unset new($v) } }
    killWindow .@conf${which}
    switch $which {
    IRC {
	    global newSv delSv
	    if [info exists newSV] {
		foreach v $newSv { $v delete }
		catch {unset newSv delSv}
	    }
	    Server :: cleanup new
	}
    Channels { Channel :: cleanup new}
    }
}
#
proc doCAN {pos name lst win dflt val} {
    if [string match {} $val] { return }
    global new
    set x [lsearch $new($name) "$val"]
    set lvl [expr {$name == {ircnames} ? [list $val] : $val}]
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
#
proc confAddNickname {win y} {
    set val [$win get [set pos [$win nearest $y]]]
    if [string match {\*NEW\*} $val] {
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
    tkwait window .@can
}
#
proc confAddIRCName {win y} {
    set val [$win get [set pos [$win nearest $y]]]
    if [string match {\*NEW\*} $val] {
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
    tkwait window .@cain
}
#
proc doCAS {pos win dflt hst prt onk opw} {
    if [string match {} $hst] { return }
    set last [expr [$win size] - 1]
    if [expr $pos != $last] {
	set lnm [string tolower [set nm [$win get $pos]]]
	global newSTO
	set id $newSTO($lnm)
	if {$lnm != [string tolower $hst]} {
	    global newShost newSname
	    set newShost($id) [set newSname($id) $hst]
	    unset newSTO($lnm)
	    set newSTO([string tolower $hst]) $id
	    $win delete $pos
	    $win insert $pos $hst
	    $win select from $pos
	}
	global newSport newSoper newSoperpw
	if ![string match {} $prt] { set newSport($id) $prt }
	set newSoper($id) $onk
	set newSoperpw($id) $opw
    } {
	global newSv
	set id [Server $hst -oper $onk -operpw $opw]
	if ![string match {} $prt] { $id configure -port $prt }
	lappend newSv $id
	$win insert $last $hst
	$win select from $last
	$id pack new
    }
}
#
proc confDel {var win args} {
    set size [expr {[$win size] - 1}]
    foreach l [set t [$win curselection]] {
	if {$l == $size} { break }
	switch $var {
	servers {
	    global newSv delSv newSTO
	    set lnm [string tolower [$win get $l]]
	    set id $newSTO($lnm)
	    if {[set x [lsearch $newSv $id]] >= 0} {
		$id unpack new
		$id delete
	    } {
		lappend delSv $id
		unset newSTO($lnm)
	    }
	}
	users {
	    global newUsr delUsr newUTO
	    set lnm [string tolower [$win get $l]]
	    set id $newUTO($lnm)
	    $id unpack new
	    $id configure -friend 0
	}
	default {
		global new
		set cl [expr {[llength $t] - 1}]
		while {[set m [lindex $t $cl]] == $size} { incr cl -1 }
		set new($var) [lreplace $new($var) $l $m]
	    }
	}
    }
    $win delete [lindex $t 0] [lindex $t [expr [llength $t] - 1]]
}
#
proc confAddServer {win y} {
    set val [$win get [set pos [$win nearest $y]]]
    if {$val == {*NEW*}} {
	mkEntryBox .@cas {New Server} {Enter the new server details:} \
	  "{Hostname {}} {Port 6667} {{Op Nick} {}} \
	  {{Op passwd} {}}" \
	  "OK {doCAS $pos $win 0}" "Default {doCAS $pos $win 1}" {Cancel {}}
    } {
	global newSTO newSport newSoper newSoperpw
	set sv $newSTO([string tolower $val])
	$win select from $pos
	mkEntryBox .@cas {Edit Server} {Edit the server details:} \
	  "{Hostname $val} {Port $newSport($sv)} \
	  {{Op Nick} $newSoper($sv)]} {{Op passwd} $newSoperpw($sv)]}" \
	  "OK {doCAS $pos $win 0}" "Default {doCAS $pos $win 1}" \
	  "Delete {confDel servers $win}" {Cancel {}}
    }
    tkwait window .@cas
}
#
proc confEnt {win var title} {
    global $var
    set name [string tolower $title]
    set winn $win.$name
    frame $winn -relief raised
    label $winn.label -text "${title}s"
    set winnl $winn.list
    makeLB $winnl -setgrid 1 -relief flat
    switch $title {
    Friend {
	    foreach v [User :: friends] {
		$winnl.l insert end [$v name]
	    }
	}
    Server {
	    foreach v [Server :: list] {
		$winnl.l insert end [$v host]
	    }
	}
    default {
	    foreach v [set $var] {	
		$winnl.l insert end $v
	    }
	}
    }
    $winnl.l insert end *NEW*
    bind $winnl.l <Double-Button-1> "confAdd${title} %W %y"
    bind $winnl.l <Delete> " confDel ${var} %W"
    bind $winnl.l <BackSpace> " confDel ${var} %W"
    bind $winnl.l <Control-h> " confDel ${var} %W"
    pack $winn.label
    pack $winn.list -expand 1 -fill both
    pack $winn -side left -expand 1 -fill both
    bind $winn <Enter> "focus $winnl.l"
}
#
proc confIRC {} {
    set win .@confIRC
    if [winfo exists $win] { popup $win ; return }
    toplevel $win -class Zircon
    wm title $win "IRC Configuration"
    confInit IRC
    frame $win.data
    confEnt $win.data nicks Nickname
    confEnt $win.data ircnames IRCName
    confEnt $win.data servers Server
    confMkBtn $win IRC
    pack $win.data -expand 1 -fill both
    pack $win.btn -fill x
}
#
proc doCInfo {indx val} {
    global confI${indx} new
    if [set confI${indx}] {
	lappend new(toInfo) $val
    } {
	listdel new(toInfo) [lsearch $new(toInfo) $val]
    }
}
#
proc doCNConf {indx val} {
    global confNC${indx} new
    if [set confNC${indx}] {
	lappend new(noConfirm) $val
    } {
	listdel new(noConfirm) [lsearch $new(noConfirm) $val]
    }
}
#
proc doCAF {pos win ntfy unm} {
    if [string match {} $unm] { return }
    set last [expr [$win size] - 1]
    if [expr $pos != $last] {
	global newUTO
	set nm [$win get $pos]
	set lnm [string tolower $nm]
	set id $newUTO($lnm)
	if {$lnm != [set lu [string tolower $unm]]} {
	    global newUnick newUlnick newUnotify newUfriend
	    set newUnick($id) $unm
	    set newLnick($id) $lu
	    set newUnotify($id) $ntfy
	    set newUfriend($id) 1
	    unset newUTO($lnm)
	    set newUTO($lu) $id
	    $win delete $pos
	    $win insert $pos $unm
	    $win select from $pos
	}
    } {
	global newUsr
	set id [User :: make $unm]
	$id configure -friend 1 -notify $ntfy
	$id pack new
	lappend newUsr $id
	$win insert $last $unm
	$win select from $last
    }
}
#
proc confAddFriend {win y} {
    set val [$win get [set pos [$win nearest $y]]]
    if [string match {\*NEW\*} $val] {
	mkEntryBox .@can {New Friend} {Enter the new friend's nickname:} \
	  {{Nickname {}}} "OK {doCAF $pos $win 0}" \
	  "{Notify On} {doCAF $pos $win 1}" {Cancel {}}
    } {
	$win select from $pos
	set id [User :: find [$win get $pos]]
	set nf [$id isNotify]
	mkEntryBox .@can {Edit Friend} {Edit the Friend's nickname:} \
	  "{Nickname {$val}}" "OK {doCAF $pos $win [expr {!$nf}]}" \
	  [list "Notify [expr {$nf ? {On} : {Off}}]" "doCAF $pos $win $nf"] \
	  "Delete {confDel users $win}" {Cancel {}}
    }
    tkwait window .@can
}
#
proc doCAI {pos win val} {
    if [string match {} $val] { return }
    global new confISel
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
	    set pos [expr [$win size] - 1]
	    $win insert $pos $val
	    $win select from $pos
	    set confISel $val
	}
    }
}
#
proc doConfIgnore {indx} {
    global confISel
    if ![string match {} $confISel] {
	global confI zircon new
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
#
proc confAddIgnore {win y} {
    set val [$win get [set pos [$win nearest $y]]]
    if {$val == {*NEW*}} {
	mkEntryBox .@ci {New ignore} {Enter the nickname/username to ignore:} \
	  {{Nickname {}}} "OK {doCAI $pos $win}" {Cancel {}}
    } {
	$win select from $pos
	mkEntryBox .@ci {Edit Nick} {Edit the nickname:} \
	  "{Nickname {$val}}" "OK {doCAI $pos $win}" \
	  "Delete {confDel ignores $win}" {Cancel {}}
    }
}
#
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
	    $win select clear  ; set confISel {}
	}
    } {
	$win select from $p
	setIB $confISel
    }
}
#
proc setIB {nk} {
    global zircon new confI
    set modes [lindex [lindex $new(ignores) [listmatch $new(ignores) $nk]] 1]
    foreach b $zircon(ignore) {
	set lb [string tolower $b]
	set confI($lb) [expr {[lsearch $modes $lb] >= 0}]
	.@confPeople.data.idata.ignore.ign2.$lb configure -state normal
    }
}
#
proc confPeople {} {
    set win .@confPeople
    if [winfo exists $win] { popup $win ; return }
    toplevel $win -class Zircon
    wm title $win {People Configuration}
    confInit People
    global confISel ignores zircon
    set confISel {}
    frame $win.data
    frame $win.data.fdata -relief raised
    confEnt $win.data.fdata users Friend
    set winn [frame $win.data.idata -relief raised]
    label $winn.label -text Ignores
    frame $winn.ignore
    set winn1 $winn.ignore.ign1
    makeLB $winn1 -setgrid 1 -relief flat
    foreach v $ignores { $winn1.l insert end [lindex $v 0] }
    $winn1.l insert end *NEW*
    bind $winn1.l <ButtonPress-1> { changeIgnore 0 %W %y }
    bind $winn1.l <Double-Button-1> { confAddIgnore %W %y }
    bind $winn1.l <Delete> { confDel ignores %W }
    bind $winn1.l <BackSpace> { confDel ignores %W }
    bind $winn1.l <Control-h> { confDel ignores %W }
    bind $winn1 <Enter> { focus %W.l }
    frame $winn.ignore.ign2
    foreach v $zircon(ignore) {
	set lv [string tolower $v]
	checkbutton $winn.ignore.ign2.$lv -text $v -state disabled \
	  -variable confI($lv) -command "doConfIgnore ${lv}"
	pack  $winn.ignore.ign2.$lv -anchor w
    }
    pack $winn.ignore.ign1 -side left -expand 1 -fill both
    pack $winn.ignore.ign2 -side left -expand 1 -fill both
    pack $winn.label $winn.ignore
    confMkBtn $win People
    pack $win.data.fdata $win.data.idata -side left -expand 1 -fill both
    pack $win.data -expand 1 -fill both
    frame $win.misc
    checkbutton $win.misc.on -text {Friends On} -variable new(friendsOn)
    checkbutton $win.misc.sf -text {Show Friends} -variable new(showFriends)
    pack $win.misc.on $win.misc.sf -expand 1 -fill x -side left
    pack $win.misc -fill x
    pack $win.btn -fill x
}
#
proc confMkBtn {win type} {
    frame $win.btn
    foreach bt {Revert Apply Save OK} {
	set lbt [string tolower $bt]
	button $win.btn.$lbt -command "conf$bt $type" -text $bt
	pack $win.btn.$lbt -side left -expand 1 -fill x
    }
}
