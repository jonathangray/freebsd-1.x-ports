#
proc flagControl {state} {
    foreach w {helpFrm.help nSFrm.cr.invis nSFrm.cr.wallop nSFrm.cr.srvmsg 
	       bf2.servers bf2.users bf2.channels bf2.services
	       bf1.away bf1.brb bf1.friends cmdLine.channel
	      } {
	.oFrm.$w conf -state $state
    }
}
#
proc busyFlag {} {global busy ; if $busy { } { } }
#
proc Control {ctl args} {
    global zircon nicks nickname ircnames Ops aways friendsStyle DEBUG
    if {$ctl != {.}} {
	toplevel $ctl
	set oFrm [frame $ctl.oFrm -relief raised]
    } {
	set oFrm [frame .oFrm -relief raised]
    }
    wm title $ctl {Zircon Control Panel}
    wm iconname $ctl {Zircon Control}

    frame $oFrm.helpFrm
    set om [makeMB $oFrm.helpFrm.conf Configure]
    foreach nn {IRC People Channels Windows Info} {
	$om add command -label $nn -command conf${nn}
    }
    $om entryconfigure 3 -state disabled
    $om add command -label {Save Current} -command saveCurrent
    $om add command -label {Reread rc} -command reread -state disabled
    button $oFrm.helpFrm.version -relief raised -command credits \
      -text "Zircon V$zircon(version)"

    button $oFrm.helpFrm.reg -relief raised -text Register -command register
    button $oFrm.helpFrm.help -relief raised -text Help -command getHelp

    pack $oFrm.helpFrm.conf $oFrm.helpFrm.version $oFrm.helpFrm.reg \
      $oFrm.helpFrm.help -side left -expand 1

    frame $oFrm.debug
    frame $oFrm.debug.mb
    checkbutton $oFrm.debug.mb.mo -text {Monitor Out} -variable monitorOut
    checkbutton $oFrm.debug.mb.mi -text {Monitor In} -variable monitorIn
    pack $oFrm.debug.mb.mo $oFrm.debug.mb.mi -side left -expand 1 -fill x
    frame $oFrm.debug.tcl
    emacsEntry $oFrm.debug.tcl.entry
    pack $oFrm.debug.tcl.entry $oFrm.debug.mb $oFrm.debug.tcl -expand 1 -fill x
    bind $oFrm.debug.tcl.entry <Return> { uplevel #0 [%W get] }

    set oc [frame [frame $oFrm.nSFrm].cr]
    checkbutton $oc.busy -text Busy -command busyFlag -variable busy
    checkbutton $oc.invis -text Invisible -command {setFlag invisible} \
      -variable invisible
    checkbutton $oc.wallop -text Wallop -command {setFlag wallops} \
      -variable wallops
    checkbutton $oc.srvmsg -text SrvMsg -command {setFlag srvmsg} \
      -variable srvmsg
    checkbutton $oc.ircop -text {IRC Op} -command deIRCOp \
      -variable zircon(ircop) -state disabled
    pack $oc.busy $oc.invis $oc.wallop $oc.srvmsg $oc.ircop -side left
    pack $oFrm.nSFrm.cr -expand 1

    NNSBuild Nickname nickname $nicks
    NNSBuild IRCName ircname $ircnames

    frame $oFrm.nSFrm.server
    set sm [makeMB $oFrm.nSFrm.server.label Server]
    set sl {}
    foreach nn [Server :: list] {
	set hst [$nn host]
	if {[set x [lsearch $sl "${hst}*"]] >= 0} {
	    continue
	}
	lappend sl [list $hst $nn]
    }
    foreach nn [lsort $sl] {
	set lbl [lindex $sl 1]
	$sm add command -label [lindex $nn 0] \
	   -command "changeServer [lindex $nn 1]"
    }

    emacsEntry $oFrm.nSFrm.server.entry

    bind $oFrm.nSFrm.server.entry <Return> {
	changeServer [Server :: make [%W get]]
    }
    bind $oFrm.nSFrm.server.entry <Escape> { \
	set h [%W get]
	mkEntryBox .@port {Port Number} "Enter port number for $h:" \
	  {{Port 6667}} \
	  "OK {changeServer \[Server :: make $h\]}" {Cancel {}}
    }

    pack $oFrm.nSFrm.server.label -side left
    pack $oFrm.nSFrm.server.entry -side left -expand 1 -fill x
    pack $oFrm.nSFrm.server -expand 1 -fill x
    $oFrm.nSFrm.server.entry insert end [$zircon(host) host]

    frame $oFrm.bf2
    makeMB $oFrm.bf2.servers Servers 
    foreach cmd $Ops(server) {
	$oFrm.bf2.servers.menu add command -label $cmd \
	  -command "serverCmd ${cmd}"
    }

    if !$zircon(ircop) { setState $oFrm.bf2.servers.menu ircSrv disabled }

    makeMB $oFrm.bf2.users Users
    foreach cmd $Ops(user) {
	case $cmd {
	DCC {
		$oFrm.bf2.users.menu add cascade -label DCC \
		    -menu $oFrm.bf2.users.menu.dcc
		menu $oFrm.bf2.users.menu.dcc
		foreach nn {List Send Chat Close} {
		    $oFrm.bf2.users.menu.dcc add command -label $nn \
		      -command "usersDCC $nn"
		}
	    }
	CTCP {
		addCTCPMenu $oFrm.bf2.users.menu {{}}
	    }
	default {
		$oFrm.bf2.users.menu add command -label $cmd \
		  -command "userCmd ${cmd}"
	    }
	}
    }
    if !$zircon(ircop) { setState $oFrm.bf2.users.menu ircop disabled }

    makeMB $oFrm.bf2.channels Channels
    foreach cmd "Join Who List Names Notice Monitor" {
	$oFrm.bf2.channels.menu add command -label $cmd \
	  -command "channel${cmd} \[.oFrm.cmdLine.channel get\]"
    }

    addCTCPMenu $oFrm.bf2.channels.menu \
      {[Channel :: make [.oFrm.cmdLine.channel get]]}

    $oFrm.bf2.channels.menu add separator

    foreach chan [lsort [Channel :: list]] {
	if [$chan menu] {
	    $oFrm.bf2.channels.menu add command -label [$chan name] \
	      -command "$chan sendJoin"
	}
    }
    makeMB $oFrm.bf2.services Services
    set i 0
    foreach chn [lsort [Service :: list]] {
	$oFrm.bf2.services.menu add cascade -label [$chn name] \
	  -menu $oFrm.bf2.services.menu.$i
	set m [menu $oFrm.bf2.services.menu.$i]
	foreach nn [$chn ops] {
	    $m add command -label $nn -command "$chn do $nn"
	}
	incr i
    }

    tk_menuBar $oFrm.bf2 $oFrm.bf2.servers $oFrm.bf2.services\
      $oFrm.bf2.users $oFrm.bf2.channels

    frame $oFrm.bf1
    makeMB $oFrm.bf1.away Away
    $oFrm.bf1.away.menu add command -label Back -command "doAway {}"
    $oFrm.bf1.away.menu add command -label New -command "getAway"
    $oFrm.bf1.away.menu add separator

    foreach act $aways {
	$oFrm.bf1.away.menu add command \
	  -label "[prune $act 15]" -command "doAway {$act}"
    }

    button $oFrm.bf1.brb -command doBRB -width 10 -text BRB
    if [friends_menu] {
	menubutton $oFrm.bf1.friends -width 10 -text Friends \
	    -menu $oFrm.bf1.friends.menu
	Friends friends
    } {
	button $oFrm.bf1.friends -command {Friends friends} \
	  -width 10 -text Friends
	global showFriends
	if $showFriends { $oFrm.bf1.friends invoke }
    }
    button $oFrm.bf1.quit -command quitZircon -width 10 -text Quit

    pack $oFrm.bf1.away $oFrm.bf1.brb $oFrm.bf1.friends $oFrm.bf1.quit \
      $oFrm.bf2.servers $oFrm.bf2.users $oFrm.bf2.channels \
      $oFrm.bf2.services -side left -expand 1 -fill x

    frame $oFrm.cmdLine -relief raised
    label $oFrm.cmdLine.label -relief raised -text { Channel }
    emacsEntry $oFrm.cmdLine.channel
    pack $oFrm.cmdLine.label -side left
    pack $oFrm.cmdLine.channel -side left -expand 1 -fill x
    if $DEBUG { pack $oFrm.debug -fill x }
    pack $oFrm.helpFrm $oFrm.nSFrm $oFrm.bf1 $oFrm.bf2 $oFrm.cmdLine  -fill x

    pack $oFrm -expand 1 -fill both 

    bind $oFrm.cmdLine.channel <Return> { channelJoin [%W get] {}}

    flagControl disabled
    tkwait visibility $ctl
}
#
# Build the Nickname, Ircname and server entries for the control window
#
proc NNSBuild {lbl var lst} {
    set frm .oFrm.nSFrm
    set name [string tolower $lbl]
    pack append $frm [frame $frm.$name] {expand fillx}
    set name $frm.$name

    set mn [makeMB $name.label $lbl]
    foreach nn [lsort $lst] {
	$mn add command -label "$nn" -command "change${lbl} {$nn}"
    }
    emacsEntry $name.entry
    bind $name.entry <Return> "change${lbl} \[%W get\]"

    pack $name.label -side left
    pack $name.entry -side left -expand 1 -fill x
    $name.entry insert end [lindex $lst 0]
    global $var ; set $var [lindex $lst 0]
}
#
proc register {} {
    mkEntryBox .@reg Register "If you wish to be notified \
of changes to zircon dynamically. Click register. If you want to join the email \
list for zircon users, enter your email address in the box and click list. \
You can leave and deregister in the obvious ways!" \
      {{Email {}}} {Register doRegister} {Deregister doDeregister} \
      {List doZList} {Leave doZLeave} {Cancel {}}
}
#
proc doRegister {email} {
    global zircon
    sendIRC PRIVMSG ZirconBot "!register $email $zircon(version)"
    close [open ~/.zirconreg w]
}
#
proc doDeregister {email} {
    sendIRC PRIVMSG ZirconBot "!deregister $email"
    if [file exists ~/.zirconreg] { exec rm [glob ~/.zirconreg] }
}
#
proc doZList {email} {
    sendIRC PRIVMSG ZirconBot "!zlist $email"
}
#
proc doZLeave {email} {
    sendIRC PRIVMSG ZirconBot "!zleave $email"
}
#
proc saveCurrent {} {
    foreach x [Channel :: list] { $x configure -keep 1 }
    foreach x [Message :: list] { $x configure -keep 1 }
    saverc
}
#
proc reread {} {
}

