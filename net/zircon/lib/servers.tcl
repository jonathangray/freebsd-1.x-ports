#
#  Server Operations
#
proc doStats {query srv} {
    if {[set param $query] == {}} {
	sendIRC STATS
    } {
	if {$srv != {}} {append param " $srv"}
	sendIRC STATS $param
    }
}

proc serverCmd {cmd} {
    case $cmd {
    Oper {
	    global nickname
	    global server
	    set opStuff [serverData $server]
	    set nk [lindex [lindex $opStuff 2] 0]
	    set pw [lindex [lindex $opStuff 2] 1]
	    mkEntryBox .@oper "Oper" "Enter name and password:" \
	      "{Name [expr {$nk == {} ? $nickname : $nk}]} \
	      {Password $pw}" \
	      {OK doOper} {Cancel {}}
	}
    {Rehash Restart} {
    	    set ucmd [string toupper $cmd]
	    mkDialog $ucmd .@$cmd "$cmd" "Really $cmd?" \
	      {} "OK {sendIRC $ucmd}" {Cancel {}}
	}
    Stats {
	    global statsInfo ; set statsInfo {}
	    global server
	    mkEntryBox .@$cmd "$cmd" "Enter Stats parameters:" \
	      "{Query {}} {Server $server}" \
	     "OK {sendIRC [string toupper $cmd]}" {Cancel {}}
	}
    Links {
	    global linksInfo ; set linksInfo {}
	    global server
	    mkEntryBox .@$cmd "$cmd" "Enter Links parameters:" \
	      [list [list "Ask from:" $server] [list "Ask what:" ""]] \
	      "OK {sendIRC [string toupper $cmd]}" {Cancel {}}
	}
    Connect {
	    global server
	    mkEntryBox .@$cmd "$cmd" "Enter Connect parameters:" \
	      "{Server $server} {Port {}} {Remote {}}" \
	      "OK {sendIRC [string toupper $cmd]}" {Cancel {}}
	}
    Info {
	    global infoInfo ; set infoInfo {}
	    global server
	    mkEntryBox .@$cmd "$cmd" "Enter Server name:" "{Server $server}"\
	      "OK {sendIRC [string toupper $cmd]}" {Cancel {}}
	}
    Trace {
	    global traceInfo ; set traceInfo {}
	    global server
	    mkEntryBox .@$cmd "$cmd" "Enter server name:" "{Server $server}"\
	      "OK {sendIRC [string toupper $cmd]}" {Cancel {}}
	}
    default {
	    global server
	    mkEntryBox .@$cmd "$cmd" "Enter pattern:" "{Server $server}"\
	      "OK {sendIRC [string toupper $cmd]}" {Cancel {}}
	}
    }
}

proc doOper {nick string} {
    if {$string != ""} { sendIRC OPER  $nick $string }
}

proc unmakeIRCOp {doit} {
    global ircop
    if {$ircop || $doit} {
	set ircop 0
	ircItems disabled
    }
}

proc makeIRCOp {} {global ircop ; if !$ircop {set ircop 1 ; ircItems normal}}

proc kill {who} {
    mkDialog {} .@kick "Kill" "Really kill $who?" \
      {{Message {}}} "OK {sendIRC KILL $who}" {Cancel {}}
}

proc userKill {} {
    mkEntryBox .@ukill "Kill" {Enter user name and message:} \
      {{User {}} {Message {}}} {OK {sendIRC KILL}} {Cancel {}}
}

proc statsProc {prefix param args} {
    global statsInfo
    foreach a [lindex $args 0] {
	if {$a != {}} {append statsInfo " $a"}
    }
    append statsInfo "\n"
}

proc traceProc {prefix param args} {
    set p {}
    foreach a [lindex $args 0] {
	if {$a != {}} {append p " $a"}
    }
    addText {} @info "[string range $prefix 1 end] : $p"
}

proc irc200 {prefix param args} { traceProc $prefix $param $args }
proc irc201 {prefix param args} { traceProc $prefix $param $args }
proc irc202 {prefix param args} { traceProc $prefix $param $args }
proc irc203 {prefix param args} { traceProc $prefix $param $args }
proc irc204 {prefix param args} { traceProc $prefix $param $args }
proc irc205 {prefix param args} { traceProc $prefix $param $args }
proc irc206 {prefix param args} { traceProc $prefix $param $args }
proc irc208 {prefix param args} { traceProc $prefix $param $args }
proc irc261 {prefix param args} { traceProc $prefix $param $args }

proc irc211 {prefix param args} { statsProc $prefix $param $args }
proc irc212 {prefix param args} { statsProc $prefix $param $args }
proc irc213 {prefix param args} { statsProc $prefix $param $args }
proc irc214 {prefix param args} { statsProc $prefix $param $args }
proc irc215 {prefix param args} { statsProc $prefix $param $args }
proc irc216 {prefix param args} { statsProc $prefix $param $args }
proc irc218 {prefix param args} { statsProc $prefix $param $args }
proc irc241 {prefix param args} { statsProc $prefix $param $args }
proc irc242 {prefix param args} { statsProc $prefix $param $args }
proc irc243 {prefix param args} { statsProc $prefix $param $args }
proc irc244 {prefix param args} { statsProc $prefix $param $args }
proc irc249 {prefix param args} { statsProc $prefix $param $args }

proc irc219 {prefix param args} {
    global statsInfo
    mkInfoBox STATS .@stats Stats $statsInfo {OK {unset statsInfo}}
}

proc irc364 {prefix param args} {
    global linksInfo
    append linksInfo "[lindex $args 1] [lindex $args 2] ${param}\n"
}

proc irc365 {prefix param args} {
    global linksInfo
    mkInfoBox LINKS .@slnk Links $linksInfo {OK {unset linksInfo}}
}

proc irc371 {prefix param args} {
    global infoInfo ; append infoInfo "${param}\n"
}

proc irc374 {prefix param args} {
    global infoInfo
    mkInfoBox INFO .@sinf Info $infoInfo {OK {unset infoInfo}}
}

