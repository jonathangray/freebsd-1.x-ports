proc irc401 {net prefix param pargs} {
    if [regexp -nocase {^zirconbot$} [set chan [lindex $pargs 1]]] {
	return
    }
    if ![string match {nil} [set this [Message :: find $chan]]] {
	$this addText @ERROR "*** $chan is not on IRC"
    } {
	$net display @ERROR "*** $param - $chan"
    }
}

proc irc404 {net prefix param pargs} {
    set this [Channel :: find [set chan [lindex $pargs 1]]]
    if ![string match {nil} $this] {
	$this addText @ERROR "*** $param"
    } {
	$net display @ERROR "*** Cannot send to channel ${chan}"
    }
}

proc irc406 {net prefix param pargs} {
    global whois
    set whois(err) [lindex $pargs 1]
}

proc resetNick {} {
    global nickname
    entrySet .oFrm.nSFrm.nickname.entry "$nickname"
}

proc irc432 {net prefix param pargs} {
    resetNick
    mkInfoBox ERROR .@nicker {Nickname Error} "[lindex $pargs 1] : $param" \
      {OK {}}
}

proc irc433 {net prefix param pargs} {
    resetNick
    mkInfoBox ERROR .@nicker {Nickname Error} "[lindex $pargs 1] : $param" \
      {OK {}}
}

proc irc443 {net prefix param pargs} {
    mkInfoBox ERROR .@inver {Invite Error} \
      "[lindex $pargs 1] $param [lindex $pargs 2]" {OK {}}
}

proc irc471 {net prefix param pargs} {
    set chn [Channel :: find [set chan [lindex $pargs 1]]]
    mkDialog {} .@full "Channel Full" "Channel ${chan} if full!" \
      {} {OK {}} "{Try Again} {$chn sendJoin}"
}

proc irc473 {net prefix param pargs} {
    mkInfoBox ERROR .@invonly {Error 473} \
      "Channel [lindex $pargs 1] is invite only!" {OK {}}
}

proc irc474 {net prefix param pargs} {
    mkInfoBox ERROR .@banned {Error 474} \
      "You are banned from channel [lindex $pargs 1]!" {OK {}}
}

proc irc475 {net prefix param pargs} {
    set chn [Channel :: find [set chan [lindex $pargs 1]]]
    if [string match {} [$chn key]] {
	mkEntryBox .@key Key "Enter key for channel ${chan}:" \
	  {{Key {}}} "Join {$chn sendJoin}" {Cancel {}}
    } {
	mkDialog {} .@key "Bad Key" \
	  "Bad key for channel ${chan}!" [list [list Key [$chn key]]] \
	  "{Try Again} {$chn sendJoin}" {Cancel {}}
    }
}

