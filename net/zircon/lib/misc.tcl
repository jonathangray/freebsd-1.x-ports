proc credits {} {
    global zircon tk_version

    mkInfoBox {} .@credits "Zircon Credits" "The Zircon IRC Client\n\
Version $zircon(version)\n\
\n\
Brought to you by Jimbles and Lindsay\n\
\n\
Thanks to:\n\
\n\
ScottM, jim_bob, Fizzy, Mikero, dl, Vesa, GiGi, janl, Avalon, Zool, Daff\n\
\n\
tcl Version [info tclversion]\n\
tk Version $tk_version" {OK {}}
}
#
proc doLimit {chan string} {
    if [string match {0} $string] {
	unlimit ${chan}
    } {
	setMode ${chan} +l $string
    }
}
#
proc unlimit {chan args} { setMode ${chan} -l }
#
proc channel_setLimit {this} {
    set chan [$this name]
    mkEntryBox .@limit "Limit" "Enter limit value for ${chan}:" {{Limit {}}}\
      "Set {doLimit ${chan}}" "Unlimit {unlimit ${chan}}" {Cancel {}}
}
#
proc banKick {who chan} {
    global banInfo
    set banInfo [list $who ${chan}]
    sendIRC USERHOST $who
}
#
proc channel_banList {this args} { setMode [$this name] +b }
#
proc doBan {op chan string} { 
    if ![string match {} $string] { setMode ${chan} ${op}b $string }
}
#
proc channel_setBan {this} {
    set chan [$this name]
    if [$this operator] {
	mkEntryBox .@ban$this {Ban} \
	  "Enter name to be banned/unbanned from ${chan}." {{Pattern {}}}\
	  "Ban {doBan + ${chan}}" "Unban {doBan - ${chan}}" \
	  "List {$this banList}" {Cancel {}}
    } {
	$this banList
    }
}
#
proc doKey {chid string} {
    if [string match {} $string] {
	clearKey ${chid}
    } {
	mkDialog SETKEY .@[newName key] {Set Key} \
	  "Really set key for channel [$chid name]?" {} \
	  "OK {doSetKey $chid {$string}}" {Cancel {}}
    }
}
#
proc doSetKey {chid string} {
    if ![string match {} [$chid key]] { doClearKey $chid }
    $chid configure -key $string
    setMode [$chid name] +k $string
}
#
proc clearKey {chid args} {
    if [string match {} [$chid key]] { return }
    mkDialog CLEARKEY .@[newName key] {Clear Key} \
      "Really clear key for channel [$chid name]?" {} \
      "OK {doClearKey $chid}" {Cancel {}}
}
#
proc doClearKey {chid args} {
    setMode [$chid name] -k [$chid key]
    $chid configure -key {}
}
#
proc setKey {chan} {
    set chid [Channel :: find ${chan}]
    mkEntryBox .@[newName key] Key "Enter key for ${chan}:" \
      "{Key [$chid key]}" \
      "Set {doKey $chid}" "Clear {clearKey $chid}" {Cancel {}}
}
#
proc finger {nk} {
    if ![string match {} $nk] {
	global fingerInfo
	set fingerInfo $nk
	sendIRC USERHOST $nk
    }
}
#
# Changing servers.....
#
proc changeServer {srv} {
    global sock startup
    if ![string match {} $sock] {
	sendIRC QUIT "Changing Servers"
	catch "dp_shutdown $sock all"
	close $sock
	set sock {}
	flagControl disabled
	foreach ch [Channel :: list] { $ch flag disabled }
	global away
	if $away { invert .oFrm.bf1.away }
	set away 0
	after 5000
    }
    set startup 1
    if [startIRC $srv] {
	set zircon(host) $srv
	unmakeIRCOp 0
	entrySet .oFrm.nSFrm.server.entry [$srv name]
    }
}

proc doBanKick {who chan msg ptr} {
    sendIRC MODE ${chan} +b $ptr
    sendIRC KICK ${chan} ${who} $msg
}

proc irc302 {net prefix param pargs} {
    set eq [string first "=" $param]
    set nk [string tolower [string range $param 0 [expr {$eq - 1}]]]
    set uh [string range $param [expr {$eq + 2}] end]
    global banInfo 
    if {[info exists banInfo] && [set who [lindex $banInfo 0]] == $nk} {
	set chan [lindex $banInfo 1]
	mkEntryBox .@kick "Ban+Kick" \
	  "Really ban and kick $who ($uh) from channel ${chan}?" \
	  [list {Message {}} [list Pattern "*!*$uh"]] \
	  "OK {doBanKick $who ${chan}}" {Cancel {}}
	unset banInfo
    } {
	global ignoreInfo
	if [info exists ignoreInfo] {
	    unset ignoreInfo
	} {
	    global fingerInfo
	    if [info exists fingerInfo] {
		unset fingerInfo
		set x [expr {[string first "@" $uh] + 1}]
		if ![catch "dp_connect [string range $uh $x end] 79" sk] {
		    set sock [lindex $sk 0]
		    global Finger ; set Finger($sock) $nk
		    dp_filehandler $sock re handleFinger
		    catch {destroy .@finger}
		    set w .@fng$nk
		    if [winfo exists $w] {
			$w.oFrm.text delete 1.0 end
		    } {
			toplevel $w -class Zircon
			wm title $w "Finger $nk"
			set oft [frame $w.oFrm]
			scrollbar $oft.vscroller -command "$oft.text yview"
			text $oft.text -yscrollcommand "$oft.vscroller set"
			pack $oft.text -side left -fill both -expand 1
			pack $oft.vscroller -side right -fill y
			button $w.ok -text OK -command "
			    destroy $w
			    catch {dp_filehandler $sock}
			    catch {close $sock}
			"
			pack $w.oFrm -expand 1 -fill x
			pack $w.ok -fill x
		    }
		    puts $sock $uh
		} {
		    $net display @ERROR "Finger Error $uh : $sk"
		}
	    } {
		$net display {} "$nk is $uh"
	    }
	}
    }
}

proc handleFinger {mode conn} {
    global Finger
    case $mode {
    r   {
	    if {[catch "gets $conn" msg] || $msg == {}} {
		catch "dp_filehandler $conn"
		catch "close $conn"
		unset Finger($conn)
	    } {
		if [winfo exists .@fng$Finger($conn)] {
		    regsub -all "\r" $msg {} msg
		    .@fng$Finger($conn).oFrm.text insert end $msg\n
		}
	    }
	}
    e { info0 addText ERROR {Error on finger connection} }
    }
}

proc irc311 {net prefix param pargs} {
    global whois
    set whois(info) [list "[lindex $pargs 1]" "[lindex $pargs 2]" \
      "[lindex $pargs 3]" "$param"]
}

proc irc312 {net prefix param pargs} {
    global whois ; lappend whois(info) "[lindex $pargs 2]" "$param"
}

proc irc313 {net prefix param pargs} { global whois ; set whois(ircop) 1 }

proc irc317 {net prefix param pargs} {
    global whois
    set val [lindex $pargs 2]
    if {$val == 1} {
	set whois(time) "1 second"
    } {
	if {$val >= 60} {
	    if {$val < 120} {
		set whois(time) "1 minute"
	    } {
		set whois(time) "[expr {$val / 60}] minutes"
	    }
	} {
	    set whois(time) "$val seconds"
	}
    }
}

proc max {a b} { return [expr $a > $b ? $a : $b] }

proc irc318 {net prefix param pargs} {
    global whois
    if ![info exists whois] { return }
    set who [lindex $whois(info) 0]

    set txt "Name: [lindex $whois(info) 1]@[lindex $whois(info) 2] ([lindex $whois(info) 3])"
    set st "Server: [lindex $whois(info) 4] ([lindex $whois(info) 5])"
    set wd [max [string length $txt] [string length $st]]
    append txt "\n$st\n"
    if [info exists whois(time)] { append txt "Idle: $whois(time)\n" }
    if [info exists whois(ircop)] { append txt "$who is an IRC operator.\n" }
    if [info exists whois(away)] {
	set wd [max $wd [string length $whois(away)]]
	append txt "Away: $whois(away)\n"
    }
    set w .@whois$who
    catch "destroy $w"
    toplevel $w -class Zircon
    wm title $w "WHOIS $who"
    frame $w.f1 -borderwidth 0
    text $w.f1.t -relief raised -height 5 -width $wd
    $w.f1.t insert end $txt
    frame $w.f1.b -relief raised
    pack $w.f1.b -fill x -side bottom
    pack $w.f1.t -expand 1 -fill both -side top
    button $w.f1.b.ok -text OK -command "destroy .@whois$who"
    pack $w.f1.b.ok -expand 1 -side left -fill x
    pack $w.f1 -fill both -expand 1 -side left
    if {[info exists whois(channels)] && $whois(channels) != {}} {
	button $w.f1.b.all -text {Join All} \
	  -command "joinAll $whois(channels) ; destroy .@whois$who"
	pack $w.f1.b.all -expand 1 -side left -fill x
	makeLB $w.f2
	foreach chn "$whois(channels)" { $w.f2.l insert end $chn }
	bind $w.f2.l <Double-Button-1> { joinAll [%W get [%W nearest %y]] }
	pack $w.f2 -side right -fill both -expand 1
    }
    unset whois
}

proc joinAll {args} {
    foreach ch $args {
	regsub {^@} $ch {} ch
	channelJoin $ch
    }
}

proc irc319 {net prefix param pargs} {
    global whois ; append whois(channels) " $param"
}

proc irc314 {net prefix param pargs} {
    global whois
    append whois(info) [list [lindex $pargs 1] [lindex $pargs 2] [lindex $pargs 3] "$param"]
}

proc irc369 {net prefix param pargs} {
    global whois
    if [info exists whois(err)] {
	set txt "There was no such user as $whois(err)."
    } {
	set txt "Name: [lindex $whois(info) 1]@[lindex $whois(info) 2] ([lindex $whois(info) 3])\n\
Server: [lindex $whois(info) 4] ([lindex $whois(info) 5])"
    }
    mkInfoBox WHOWAS .@whowas Whowas "$txt" {OK {}}
    unset whois
}

proc irc341 {net prefix param pargs} {
    if [string match {nil} [set id [Channel :: find [set chan [lindex $pargs 2]]]]] {
	set id [$net info]
    }
    $id addText {} "*** Inviting [lindex $pargs 1] to channel ${chan}"
}

proc irc342 {net prefix param pargs} {
    $net display {} "*** Summoning [lindex $pargs 1] to IRC"
}

proc irc315 {net prefix param pargs} {
    global whoTxt
    if {[info exists whoTxt] && [string match {.@who*} $whoTxt]} {
	$whoTxt yview 0
    }
    catch {unset whoTxt}
}

proc irc352 {net prefix param pargs} {
    global whoTxt
    set fmt "%-9s\t%-10s\t%-3s\t%s@%s (%s)\n" 
    set txt [format $fmt [lindex $pargs 1] \
      [lindex $pargs 5] [lindex $pargs 6] [lindex $pargs 2] \
      [lindex $pargs 3] $param]
    if ![info exists whoTxt] {
	set whoTxt [mkInfoBox WHO .@[newName who] "Who [exec date]" {} {OK {}}]
    }
    $whoTxt configure -state normal
    insertText [net0 info] $whoTxt $txt {}
    $whoTxt configure -state disabled
    set ln [lindex [split [$whoTxt index end] .] 0]
    if {$ln < 24 && $ln > 10} {
	$whoTxt conf -height $ln
    }
    $whoTxt yview -pickplace end
}
