#
# Upgrade the format of the rc file
#
proc upgradeTest {} {
    global servers userInfo services channelInfo \
      popUp popDown noJump quiet noDraw closeTime history noMessage
    set flg 0
    foreach v {servers userInfo services channelInfo \
      popUp popDown noJump quiet noDraw closeTime history noMessage} {
	set flg [expr {$flg | [info exists $v]}]
    }
    if $flg {
	if [info exists servers] {
	    foreach s $servers {
		set id [Server [lindex $s 0]]
		if {[set port [lindex $s 1]] != 6667} {
		    $id configure -port $port
		}
		set opstuff [lindex $s 2]
		if {[set oper [lindex $opstuff 0]] != {}} {
		    set operpw [lindex $opstuff 1]
		    $id configure -oper $oper -operpw $operpw
		}
	    }
	    if ![string match {} $servers] {
		Server :: select [lindex [lindex $servers 0] 0]
	    }
	    unset servers
	}
	if [info exists userInfo] {
	    foreach u $userInfo {
		set id [User [lindex $u 0] -friend 1]
		if {[lindex $u 1] != {}} { $id notify 1}
	    }
	    unset userInfo
	}
	if [info exists services] {
	    foreach u $services {
		Service [lindex $u 0] -host [lindex $u 1] -ops [lindex $u 2]
	    }
	    unset services
	}
	if [info exists channelInfo] {
	    foreach chn $channelInfo {
		set cn [lindex $chn 0]
		if [string match {[#&]*} $cn] {
		    set id [Channel $cn]
		} {
		    set id [Message $cn]
		    puts stderr \
"*** Warning Conversation window $cn found in rc file - the upgrade
process does not currently support this. You will have to add this by hand."
		}
		foreach x { {-msg 2} {-ops 3} {-history 4} \
		  {-closetime 5} {-icon 6} {-topics 7} {-logfile 8} \
		  {-bindings 9} {-patterns 10} } {
		    if {[set h [lindex $chn [lindex $x 1]]] != {}} {
			$id configure [lindex $x 0] $h
		    }
		}
		foreach auto [lindex $chn 1] {
		    switch [string tolower $auto] {
		    nodraw { $id configure -draw 0 }
		    nojump { $id configure -jump 0 }
		    default { $id configure -$auto 1 }
		    }
		}
	    }
	    unset channelInfo
	}
	set def [Channel :: find *default*]
	foreach v {{popUp open} {popDown close} {noJump jump} \
	  {quiet quiet} {noDraw draw} {closeTime closetime} {history history} \
	  {noMessage msg}} {
	    if ![info exists [set var [lindex $v 0]]] continue
	    switch $var {
	    noDraw -
	    noJump { $def configure -[lindex $v 1] [expr ![set $var]] }
	    closeTime { $def configure -closetime [expr $closeTime / 1000] }
	    default { $def configure -[lindex $v 1] [set $var] }
	    }
	}
	wm iconify .
	switch [tk_dialog .@upgrade Upgrade \
	  {The format of the zircon rc file has been changed. Upgrade?} \
	  {} 0 Upgrade Cancel ] {
	0 { upgradeRC }
	1 { exit }
	}
    }
}
#
proc upgradeRC {} {
    set file [glob ~]/.zirconrc
    exec mv $file $file.old
    saverc
    tk_dialog .@upgrade Upgrade \
      {Upgrade Complete. Now exit and restart zircon.} {} 0 Exit
    exit
}
#
proc upgradeWarn {} {
    tk_dialog .@upgrade Upgrade \
      {Please ask your zircon administrator to upgrade the system rc file.} \
      {} 0 OK
}
