proc credits {} {
    global zircon
    global tk_version

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

proc doLimit {chan string} {
    if {$string == "0"} { unlimit ${chan} "" } { setMode ${chan} +l $string }
}

proc unlimit {chan dummy} { setMode ${chan} -l }

proc setLimit {chan} {
    mkEntryBox .@limit "Limit" "Enter limit value for ${chan}:" {{Limit {}}}\
      "Set {doLimit ${chan}}" "Unlimit {unlimit ${chan}}" {Cancel {}}
}

proc banKick {who chan} {
    global banInfo
    set banInfo [list $who $chan]
    sendIRC USERHOST $who
}

proc banList {chan args} { setMode ${chan} +b }

proc doBan {op chan string} { 
    if {$string != ""} { setMode ${chan} ${op}b $string }
}

proc setBan {chan} {
    mkEntryBox .@ban "Ban" "Enter name to be banned/unbanned from ${chan}." \
      {{Pattern {}}}\
      "Ban {doBan + ${chan}}" "Unban {doBan - ${chan}}" \
      "List {banList ${chan}}" {Cancel {}}
}

proc doKey {chan string} {
    if {$string == ""} { clearKey ${chan} } { setMode ${chan} +k $string }
}

proc clearKey {chan args} {
    global ${chan}Key
    setMode $chan -k [set ${chan}Key]
}

proc setKey {chan} {
    global ${chan}Key
    mkEntryBox .@key "Key" "Enter key for ${chan}:" \
      [list [list Key [set ${chan}Key] ] ]\
      "Set {doKey ${chan}}" "Clear {clearKey ${chan}}" {Cancel {}}
}

proc finger nk {
    if {$nk != {}} {
	global fingerInfo
	set fingerInfo $nk
	sendIRC USERHOST $nk
    }
}

#
#	Changing servers.....
#
proc changeServer {host port} {
    global sock
    global server
    global startup
    global ircport
    global activeChannels
    if {$sock != {}} {
	sendIRC QUIT "Changing Servers"
	catch "dp_shutdown $sock all"
	close $sock
	set sock {}
	flagControl disabled
	foreach ch $activeChannels { flagChannel ${ch} disabled }
	global away
	if $away { invert .oFrm.bf1.away }
	set away 0
	after 5000
    }
    set startup 1
    if [startIRC $host $port] {
	set server $host
	set ircport port
	unmakeIRCOp 0
	entrySet .oFrm.nSFrm.server.entry "$host"
    }
}

proc doBanKick {who chan msg ptr} {
    sendIRC MODE $chan +b $ptr
    sendIRC KICK $chan $who
}

proc irc302 {prefix param args} {
    set eq [string first "=" $param]
    set nk [string tolower [string range $param 0 [expr {$eq - 1}]]]
    set uh [string range $param [expr {$eq + 2}] end]
    global banInfo 
    if {[info exists banInfo] && [set who [lindex $banInfo 0]] == $nk} {
	set chan [lindex $banInfo 1]
	mkEntryBox .@kick "Ban+Kick" \
	  "Really ban and kick $who ($uh) from channel ${chan}?" \
	  [list {Message {}} [list Pattern "*!*$uh"]] \
	  "OK {doBanKick $who $chan}" {Cancel {}}
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
			zpack $oft text {left fill expand}
			zpack $oft vscroller {right filly}
			button $w.ok -text OK -command "
			    destroy $w
			    catch {dp_filehandler $sock}
			    catch {close $sock}
			"
			zpack $w oFrm {expand fill}
			zpack $w ok {fillx}
		    }
		    puts $sock $uh
		} {
		    addText ERROR @info "Finger Error $uh : $sk"
		}
	    } {
		addText {} @info "$nk is $uh"
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
    e { addText ERROR @info {Error on finger connection} }
    }
}

proc irc311 {prefix param args} {
    global whois
    set whois(info) \
      [list [lindex $args 1] [lindex $args 2] [lindex $args 3] "$param"]
}

proc irc312 {prefix param args} {
    global whois ; lappend whois(info) [lindex $args 2] "$param"
}

proc irc313 {prefix param args} { global whois ; set whois(ircop) 1 }

proc irc317 {prefix param args} {
    global whois
    set val [lindex $args 2]
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

proc irc318 {prefix param args} {
    global whois
    if ![info exists whois] { return }
    set who [lindex $whois(info) 0]

    set txt "Name: [lindex $whois(info) 1]@[lindex $whois(info) 2] ([lindex $whois(info) 3])\n\
Server: [lindex $whois(info) 4] ([lindex $whois(info) 5])\n"

    if [info exists whois(time)] { append txt "Idle: $whois(time)\n" }
    if [info exists whois(ircop)] { append txt "$who is an IRC operator.\n" }
    if [info exists whois(away)] { append txt "Away: $whois(away)\n" }
    if [info exists whois(channels)] {
	append txt "Channels:"
	foreach chn "$whois(channels)" { append txt " $chn" }
    }
    mkInfoBox WHOIS .@whois "Whois $who" "$txt" {OK {}}
    unset whois
}

proc irc319 {prefix param args} {
    global whois ; append whois(channels) " $param"
}

proc irc314 {prefix param args} {
    global whois
    append whois(info) [list [lindex $args 1] [lindex $args 2] [lindex $args 3] "$param"]
}

proc irc369 {prefix param args} {
    global whois
    if [info exists whois(err)] {
	set txt "There was no such user as $whois(err)."
    } {
	set txt "Name: [lindex $whois(info) 1]@[lindex $whois(info) 2] ([lindex $whois(info) 3])\n\
Server: [lindex $whois(info) 4] ([lindex $whois(info) 5])"
    }
    mkInfoBox WHOWAS .@whowas "Whowas" "$txt" {OK {}}
    unset whois
}

proc irc341 {prefix param args} {
    addText {} @info "*** Inviting [lindex $args 1] to channel [lindex $args 2]"
}

proc irc342 {prefix param args} {
    addText {} @info "*** Summoning [lindex $args 1] to IRC"
}

proc irc315 {prefix param args} {
    global whoTxt 
    if [info exists whoTxt] {
	mkInfoBox WHO .@who "Who" "$whoTxt" {OK { unset whoTxt }}
    }
}

proc irc352 {prefix param args} {
    global whoTxt
    set fmt "%-9s\t%-10s\t%-3s\t%s@%s (%s)\n"
    append whoTxt [format $fmt [lindex $args 1] \
      [lindex $args 5] [lindex $args 6] [lindex $args 2] \
      [lindex $args 3] $param]
}
