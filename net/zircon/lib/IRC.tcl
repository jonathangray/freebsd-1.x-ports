#
#   Handle IRC cmds
#
proc ircNOTICE {net prefix param pargs} {
    set nkinfo [mungPrefix $prefix]
    if [ignoreSet [lindex $nkinfo 3] notices] { return }
    if [string match {} [lindex $nkinfo 2]] {
	$net display {} [mungNotice $param]
    } {
	set usr [lindex $nkinfo 0]
	set nk [$usr name]
	set chan [lindex $pargs 0]
	set id [find ${chan}]
	if [regexp "\001(\[^\001\]*)\001" $param sub cp] {
	    switch -glob -- $cp {
	    {ZIRCON Sorry*} { }
	    {PING *} { handlePing $nk $cp }
	    default {
		    mkInfoBox CTCP .@ctcp${nk} {CTCP Reply} \
		      "CTCP Reply from $nk: $cp" {OK {}}
		}
	    }
	} \
	elseif [string match {[#$&]*} ${chan}] {
	    $id addText [$usr lname] "-${nk}- $param"
	} \
	elseif {[string match {nil} [set id [Notice :: find $nk]]] &&
	  [string match {nil} [set id [Message :: find $nk]]]} {
	    global busy
	    if $busy {
		$net display {} "Notice from ${nk} at [exec date] : $param"
	    } {
		set id [Notice :: make ${nk}]
		$id addText {} "[exec date]\n$param"
	    }
	} {
	    $id addText ${nk} $param
	}
    }
}
#
proc ircMODE {net prefix param pargs} {
    set chan [lindex $pargs 0]
    if [string match {nil} [set id [Channel :: find ${chan}]]] {
	if [me ${chan}] {
	    if [string match {} [set md [lindex $pargs 1]]] { set md $param }
	    foreach m [split $md {}] {
		case $m {
	 	- { set cmd unsetUser }
		+ { set cmd setUser }
		* { $cmd $m }
		}
	    }
	}
	return
    }
    $id mode [lrange $pargs 1 end]
    $id optText MODE "*** Mode change \"[string trim \
      [join [lrange $pargs 1 end]]]\" on channel ${chan} by\
[[lindex [mungPrefix $prefix] 0] name]"
}
#
proc ircPRIVMSG {net prefix param pargs} {
    set nkinfo [mungPrefix $prefix]
    set usr [lindex $nkinfo 0]
    set nk [$usr name]
    set lnk [$usr lname]
    set ign [lindex $nkinfo 3]
    set chan [lindex $pargs 0]
    if [regexp "(\[^\001\]*)\001(\[^\001\]*)\001(\[^\001\]*)" $param sub a cp b] {
    	set ctcp [split $cp]
	set value \
	  [handleCTCP $net [lindex $ctcp 0] ${chan} $usr $prefix $ign "$cp"]
	if [string match {} $value] { return }
	set param "${a}${value}${b}"
    }
    set pfx "<${nk}>"
    if [me ${chan}] {
	if [ignoreSet $ign notes] { return }
	if [string match {nil} [set where [Message :: find $nk]]] {
	    global busy
	    if $busy {
		sendIRC NOTICE ${nk} \
"I am busy and am not accepting calls at the moment."
		$net display {} "Message from ${nk} at [exec date] : $param"
	    } {
		set id [Message :: make $nk]
		$id addText $lnk "[exec date]\n$pfx $param"
	    }
	    return
	} {
	    $where show
	}
	set chan $lnk
    } {
	if [ignoreSet $ign public] { return }
	set where [Channel :: find ${chan}]
    }
    if [string match {nil} $where] {
	set where [$net info]
	set pfx "<${nk}/${chan}>"
    }
    $where addText $lnk "$pfx $param"
    foreach p [$where patterns] {
	set pt [lindex $p 0]
	if {[regexp -nocase [lindex $pt 0] $pfx] && \
	  [regexp [lindex $pt 1] $param]} {
	    uplevel #0 [lindex $p 1]
	}
    }
}
#
proc ircJOIN {net prefix param pargs} {
    set nkinfo [mungPrefix $prefix]
    if [lindex $nkinfo 1] {
	[Channel :: make $param] show
    } {
	[Channel :: find $param] doJoin [lindex $nkinfo 0] \
	  [lindex $nkinfo 2] $prefix
    }
    handleOn JOIN [list $param $prefix]
}
#
proc ircNICK {net prefix param pargs} {
    set nkinfo [mungPrefix $prefix]
    set usr [lindex $nkinfo 0]
    if [lindex $nkinfo 1] { entrySet .oFrm.nSFrm.nickname.entry $param }
    if ![string match {nil} [set old [User :: find $param]]] {
	if {$usr != $old} {
	    $usr copy $old
	    $old delete
	}
    }
    foreach id [Channel :: list] {
	if [$id isa Channel] {
	    if [$id isJoined $usr] { $id nickChange $usr $param }
	} \
	elseif {[$id lname] == [$usr lname]} {
	    $id nickChange $usr $param
	}
    }
    $usr rename $param
    handleOn NICK [list $prefix $param]
}
#
proc ircNUM {net number prefix param pargs} {
    set txt {}
    foreach arg [lrange $pargs 1 end] {
	if ![string match {} $arg] { append txt " $arg" }
    }
    append txt " $param"
    case $number {
    [45]* { mkInfoBox ERROR .@err$number "Error $number" $txt {OK {}} }
    default { $net display {} $txt }
    }
}
#
proc ircPART {net prefix param pargs} {
    set nkinfo [mungPrefix ${prefix}]
    set chn [lindex $pargs 0]
    set this [Channel :: find $chn]
    if [lindex $nkinfo 1] {
	$this delete
    } {
	set usr [lindex $nkinfo 0]
	$this optText LEAVE "*** [$usr name] has left channel $chn"
	$this killUser $usr
    }
    handleOn LEAVE [list ${chn} ${prefix}]
}
#
proc ircKICK {net prefix param pargs} {
    global myid
    set nkinfo [mungPrefix $prefix]
    set chan [lindex $pargs 0]
    set who [User :: make [lindex $pargs 1]]
    set kicker [[lindex $nkinfo 0] name]
    set id [Channel :: find $chan]
    if {$who == $myid} {
	$id delete
	mkDialog KICKED .@kicked "Kicked from ${chan}"\
	  "You have been kicked off channel ${chan} by $kicker ($param)" \
	  {} {OK {}}
    } {
	$id optText KICK \
	  "*** [$who name] has been kicked off channel ${chan} by $kicker ($param)"
	$id killUser $who
   }
   handleOn KICK [list $chan $prefix [$who name] $param]
}
#
proc netsplit {string} {
    return [regexp -nocase \
      {^([a-z0-9*_-]+\.)+([a-z0-9_-]+) ([a-z0-9*_-]+\.)+([a-z0-9_-]+)$} $string]
}
#
proc ircQUIT {net prefix param pargs} {
    set nkinfo [mungPrefix $prefix]
    set usr [lindex $nkinfo 0]
    set nk [$usr name]
    if [netsplit $param] {
	global Split
	if ![info exists Split($param)] {
	    [$net info] optText SPLIT "*** Netsplit - $param"
	    global TSplit Heal
	    set TSplit($param) 600000
	    catch {unset Heal($param)}
	}
	foreach id [Channel :: list] {
	    if [$id isJoined $usr] {
		set w [$id window]
		if {[set x [$w.users.menu index [$usr name]]] >=0} {
		    $w.users.menu entryconfigure $x -state disabled
	        }
		$w.cFrm.uFrm.userBtn.frame.$usr conf -state disabled
	    } \
	    elseif {[$id name] == [$usr lname]} {
		$id addText {} "*** Netsplit - $param"
		$id flag disabled
	    }
	}
	friends disable $usr
	$usr limbo 1
	lappend Split($param) $usr
    } {
	global toInfo
	if {[set ti [expr {[lsearch $toInfo SIGNOFF] >=0}]]} {
	    $net display @QUIT "*** Signoff: ${nk} ($param)"
	}
	set lnk [$usr lname]
	foreach id [Channel :: list] {
	    if [$id isJoined $usr] {
		if !$ti {
		    $id optText QUIT "*** Signoff: ${nk} ($param)"
		}
		$id killUser $usr
	    } \
	    elseif {[$id lname] == $lnk && [$id active]} {
		$id addText @QUIT "*** ${nk} has signed off!!!"
	    }
	}
	$usr off
	handleOn QUIT [list ${prefix}]
    }
}
#
proc ircINVITE {net prefix param pargs} {
    if ![ignoreSet [lindex [set nkinfo [mungPrefix $prefix]] 3] invites] {
	mkDialog {} .@invite "Invitation" \
	  "[[lindex $nkinfo 0] name] invites you to channel $param." {} \
	  "Join {channelJoin $param}" {Ignore {}}
    }
}
#
proc ircKILL {net prefix param pargs} {
    global myid
    set nkinfo [mungPrefix $prefix]
    set who [User :: make [lindex $pargs 0]]
    if {$myid == $who} {
	global sock
	if ![string match {} $sock] { closeIRC $sock }
	mkDialog KILLED .@killed "Killed"\
	  "You have been killed by [[lindex $nkinfo 0] name] ($param)" \
	  {} {OK {}}
    } {
	foreach id [Channel :: list] {
	    if [$id isJoined $who] {
		$id optText KILL \
		  "*** [$who name] has been killed by [[lindex $nkinfo 0] name] ($param)"
		$id killUser $who
	    }
	}
   }
   $who off
   handleOn KILL [list $prefix [$who name]]
}
#
proc ircTOPIC {net prefix param pargs} {
    set id [Channel :: find [set chan [lindex $pargs 0]]]
    $id setTopic $param
    set who [[lindex [mungPrefix $prefix] 0] name]
    $id optText TOPIC "*** $who has set the topic."
    $id log "*** $who has set the topic: $param"
    handleOn TOPIC [list ${chan} $prefix $param]
}
#
proc handlePing {nk line} {
    global zircon
    regexp {PING *(.*)} $line match t
    set t [exec $zircon(lib)/zping $t]
    mkInfoBox PING .@ctcp${nk} {CTCP Ping} \
      "CTCP Ping from $nk: $t secs" {OK {}}
}
