#
#  Server Operations
#
proc doStats {query srv} {
    if [string match {} $query] {
	sendIRC STATS
    } {
	sendIRC STATS $query $srv
    }
}
#
proc serverCmd {cmd} {
    global zircon
    set srv [$zircon(host) host]
    set def "(Default host is $srv)"
    switch $cmd {
    Oper {
	    global myid
	    set opStuff [$zircon(host) oper]
	    set nk [lindex $opStuff 0]
	    set pw [lindex $opStuff 1]
	    mkEntryBox .@oper Oper {Enter name and password:} \
	      "{Name [expr {$nk == {} ? [$myid name] : $nk}]} \
	      {Password $pw}" {OK doOper} {Cancel {}}
	}
    Rehash -
    Restart {
    	    set ucmd [string toupper $cmd]
	    mkDialog $ucmd .@$cmd $cmd "Really $cmd $srv?" \
	      {} "OK {sendIRC $ucmd}" {Cancel {}}
	}
    Stats {
	    global statsInfo
	    set statsInfo {}
	    mkEntryBox .@Stats Stats "Enter Stats parameters $def:" \
	      {{Query {}} {Server {}}} {OK {sendIRC STATS}} {Cancel {}}
	}
    Links {
	    global linksInfo
	    set linksInfo {}
	    mkEntryBox .@Links Links "Enter Links parameters $def:" \
	      {{Server {}} {Mask {}}} {OK {sendIRC LINKS}} {Cancel {}}
	}
    Connect {
	    mkEntryBox .@Connect Connect "Enter Connect parameters $def:" \
	      {{Server {}} {Port {}} {Remote {}}} \
	      {OK {sendIRC CONNECT}} {Cancel {}}
	}
    Info {
	    global infoInfo
	    set infoInfo {}
	    mkEntryBox .@Info Info "Enter Server name $def:" {{Server {}}}\
	      {OK {sendIRC INFO}} {Cancel {}}
	}
    Trace {
	    global traceInfo
	    set traceInfo {}
	    mkEntryBox .@Trace Trace "Enter server name $def:" {{Server {}}}\
	      {OK {sendIRC TRACE}} {Cancel {}}
	}
    Motd {
	    mkEntryBox .@Trace MOTD "Enter server name $def:" {{Server {}}}\
	      {OK {sendIRC MOTD}} {Cancel {}}
	}
    default {
	    set ucmd [string toupper $cmd]
	    mkEntryBox .@$cmd $cmd "Enter server pattern $def:" {{Server {}}}\
	      "OK {sendIRC $ucmd}" {Cancel {}}
	}
    }
}
#
proc doOper {nick string} {
    if ![string match {} $string] { sendIRC OPER  $nick $string }
}
#
proc unmakeIRCOp {doit} {
    global zircon
    if {$zircon(ircop) || $doit} {
	set zircon(ircop) 0
	ircItems disabled
    }
}
#
proc makeIRCOp {} {
    global zircon
    if !$zircon(ircop) { set zircon(ircop) 1 ; ircItems normal }
}
#
proc kill {who} {
    mkDialog {} .@kill Kill "Really kill $who?" \
      {{Message {}}} "OK {sendIRC KILL $who}" {Cancel {}}
}
#
proc doKill {nk msg} {
    if ![string match {} $nk] { sendIRC KILL $nk $msg }
}
#
proc userKill {} {
    mkEntryBox .@ukill "Kill" {Enter user name and message:} \
      {{User {}} {Message {}}} {OK doKill} {Cancel {}}
}

proc statsProc {prefix param args} {
    global statsInfo
    set p [string range $prefix 1 end]:
    foreach a [lrange [lindex $args 0] 1 end] {
	if ![string match {} $a] {append p " $a"}
    }
    if ![string match {} $param] {append p " $param"}
    append statsInfo "$p\n"
}

proc traceProc {net prefix param args} {
    set p [string range $prefix 1 end]:
    foreach a [lrange [lindex $args 0] 1 end] {
	if ![string match {} $a] {append p " $a"}
    }
    if ![string match {} $param] {append p " $param"}
    $net display {} $p
}

proc irc200 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc201 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc202 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc203 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc204 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc205 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc206 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc208 {net prefix param pargs} { traceProc $net $prefix $param $pargs }
proc irc261 {net prefix param pargs} { traceProc $net $prefix $param $pargs }

proc irc211 {net prefix param pargs} { statsProc $prefix $param $pargs }
proc irc212 {net prefix param pargs} { statsProc $prefix $param $pargs }
proc irc213 {net prefix param pargs} { statsProc $prefix $param $pargs }
proc irc214 {net prefix param pargs} { statsProc $prefix $param $pargs }
proc irc215 {net prefix param pargs} { statsProc $prefix $param $pargs }
proc irc216 {net prefix param pargs} { statsProc $prefix $param $pargs }
proc irc218 {net prefix param pargs} { statsProc $prefix $param $pargs }
proc irc241 {net prefix param pargs} { statsProc $prefix $param $pargs }
proc irc242 {net prefix param pargs} { statsProc $prefix $param $pargs }
proc irc243 {net prefix param pargs} { statsProc $prefix $param $pargs }
proc irc244 {net prefix param pargs} { statsProc $prefix $param $pargs }
proc irc249 {net prefix param pargs} { statsProc $prefix $param $pargs }

proc irc219 {net prefix param pargs} {
    global statsInfo
    set w .@[newName stats]
    mkInfoBox STATS $w "[string range $prefix 1 end] Stats [exec date]" $statsInfo {OK {}}
    unset statsInfo
}

proc irc364 {net prefix param pargs} {
    global linksInfo
    append linksInfo "[lindex $pargs 1] [lindex $pargs 2] ${param}\n"
}

proc irc365 {net prefix param pargs} {
    global linksInfo
    set w .@[newName links]
    mkInfoBox LINKS $w "[string range $prefix 1 end] Links [exec date]" $linksInfo {OK {}}
    unset linksInfo
}
#
proc irc371 {net prefix param pargs} {
    global infoInfo
    append infoInfo "${param}\n"
}
#
proc irc374 {net prefix param pargs} {
    global infoInfo
    set w .@[newName info]
    mkInfoBox INFO $w "[string range $prefix 1 end] Info [exec date]" $infoInfo {OK {}}
    unset infoInfo
}
#
proc ircItems {state} {
    foreach cid [Channel :: list] { $cid ircOp $state }
    setState .oFrm.bf2.servers.menu ircSrv $state
    setState .oFrm.bf2.users.menu ircop $state
    .oFrm.nSFrm.cr.ircop conf -state $state
}

