proc ctcpReply {nk op str} {
    global verboseCTCP
    if $verboseCTCP { addText @CTCP @info "*** @CTCP $op Reply to $nk - $str" }
    sendIRC NOTICE $nk "\001$op $str\001"
}

proc handleCTCP {op chan nk lnk prefix ign param} {
    if [ignoreSet $ign ctcp] { return {}}
    global verboseCTCP
    if {$verboseCTCP && $op != "ACTION"} {
	addText @CTCP @info "*** CTCP $op from $nk"
    }
    case $op {
    CLIENTINFO {
	    ctcpReply $nk $op "CLIENTINFO VERSION USERINFO ERRMSG PID SOURCE ACTION FINGER TIME UTC ECHO DCC: The Zircon X11 client"
	}
    VERSION {
	    global zircon
	    global tk_version
	    ctcpReply $nk $op \
	      "Zircon $zircon(version) *IX : tcl [info tclversion] tk $tk_version"
	}
    USERINFO { global ircname ; ctcpReply $nk $op $ircname }
    {PING ECHO ERRMSG} {
	    ctcpReply $nk $op [string range $param [string length $op] end]
	}
    PID { ctcpReply $nk $op [pid] }
    SOURCE { ctcpReply $nk $op "Available by ftp from catless.ncl.ac.uk" }
    ACTION {
	    if {[me ${chan}]} {
		if {![active $lnk]} {	
		    global busy
		    if $busy {
			global nickname
			sendIRC NOTICE $nk \
"I am busy and am not accepting calls at the moment.\001"
			addText {} @info \
			  "Action from $nk at [exec date] : [string range $param 7 end]"
		    } {
			makeChannel $lnk M
			addText {} $lnk [exec date] -noscroll
			addText $lnk $lnk \
			  "* $nk [string range $param 7 end]" -noscroll
		    }
		    return {}
		}
		set chan $lnk
	    }
	    addText $lnk ${chan} "* $nk [string range $param 7 end]"
	}

    FINGER {
	    global ircname
	    global zircon
	    if {[set t $zircon(idle)] >= 60} {
		if {[set r [expr {$t % 60}]] > 0} {
		    append r { seconds}
		} {
		    set r {}
		}
		if {[set t [expr {$t / 60}]] != 1} {
		    set t "$t minutes $r"
		} {
		    set t "$t minute $r"
		}
	    } {
		append t { seconds}
	    }

	    ctcpReply $nk $op "$ircname Idle $t"
	}
    SED { }
    TIME {
	    ctcpReply $nk $op [exec date]
	}
    UTC {
	# should convert to UTC and back substitute
	    return $param
	}
    DCC {
	    handleDCC $nk $lnk $param
	}
    ZIRCON {
	    handleZircon $prefix $lnk $param
	}
    default {
	    ctcpReply $nk $op "Sorry, $nk I can't do that."
	}
    }
    return {}
}

proc sendCtcp {cmd nk string} { sendIRC PRIVMSG $nk "\001$cmd $string\001" }

proc doCtcp {cmd nk} {
    case $cmd {
    {CLIENTINFO ECHO ERRMSG ZIRCON} {
	    mkEntryBox .@ctcp$nk "CTCP" "Enter $cmd parameters:"  \
	      {{Parameters {}}} "OK {sendCtcp $cmd $nk}" "Cancel {}"
	}
    PING {
	      sendCtcp $cmd $nk [exec date]
	}
    default {
	      sendCtcp $cmd $nk {}
	}
    }
}

proc trusted {op pfx} {
    global trust
    foreach p $trust($op) {
	if [regexp -nocase $p $pfx] { return 1 }
    }
    return 0
}

proc handleZircon {pfx nk param} {
    set cmd [lrange $param 2 end]
    case [set op [lindex $param 1]] {
    DEBUG {
	    global DEBUG
	    if [set DEBUG [expr {!$DEBUG}]] {
		if ![string match "7*" [info tclversion]] {
		    pack before .oFrm.helpFrm .oFrm.debug {fillx}
		} {
		    pack .oFrm.debug -before .oFrm.helpFrm -fill x
		}
	    } {
		if ![string match "7*" [info tclversion]] {
		    pack unpack .oFrm.debug
		} {
		    pack forget .oFrm.debug
		}
	    }
	}
    EVAL {
	    if [trusted eval $pfx] {
		mkDialog EVAL .@eval {Remote Command} \
		  "$nk wants you to eval : $cmd" {} {No ()} \
		  "OK {ctcpReply $nk ZIRCON \[eval [lrange $param 2 end]\]}"
	    } {
		ctcpReply $nk ZIRCON "Sorry $nk, I don't trust you to $op!"
	    }
	}
    DRAW {
	    if [trusted draw $pfx] {
		zdraw $nk $cmd
	    } {
		ctcpReply $nk ZIRCON "Sorry $nk, I don't trust you to $op!"
	    }
	}
    default {
	    ctcpReply $nk ZIRCON "Sorry $nk, I don't know how to $op!"
	}
    }
}
