proc irc401 {prefix param args} {
    set chan [string tolower [lindex $args 1]]
    if [active ${chan}] {
	addText @ERROR $chan "*** $chan is not on IRC"
    } {
	addText @ERROR @info "*** $param - $chan"
    }
}

proc irc404 {prefix param args} {
    set chan [string tolower [lindex $args 1]]
    if [active ${chan}] {
	addText @ERROR ${chan} "*** $param"
    } {
	addText @ERROR @info "*** Cannot send to channel ${chan}"
    }
}

proc irc406 {prefix param args} {
    global whois
    set whois(err) [lindex $args 1]
}

proc resetNick {} {
    global nickname
    entrySet .oFrm.nSFrm.nickname.entry "$nickname"
}

proc irc432 {prefix param args} {
    resetNick
    mkInfoBox ERROR .@nicker {Nickname Error} "[lindex $args 1] : $param" \
      {OK {}}
}

proc irc433 {prefix param args} {
    resetNick
    mkInfoBox ERROR .@nicker {Nickname Error} "[lindex $args 1] : $param" \
      {OK {}}
}

proc irc443 {prefix param args} {
    mkInfoBox ERROR .@inver {Invite Error} \
      "[lindex $args 1] $param [lindex $args 2]" {OK {}}
}

proc irc471 {prefix param args} {
    set chan [lindex $args 1]
    mkDialog {} .@full "Channel Full" "Channel ${chan} if full!" \
      {} {OK {}} "{Try Again} {channelJoin ${chan}}"
}

proc irc473 {prefix param args} {
    mkInfoBox ERROR .@invonly {Error 473} \
      "Channel [lindex $args 1] is invite only!" {OK {}}
}

proc irc474 {prefix param args} {
    mkInfoBox ERROR .@banned {Error 474} \
      "You are banned from channel [lindex $args 1]!" {OK {}}
}

proc irc475 {prefix param args} {
    set chan [lindex $args 1]
    global ${chan}Key
    if {[set ${chan}Key] != ""} {
	mkDialog {} .@key "Bad Key" \
	  "Bad key for channel ${chan}!" [list [list Key [set ${chan}Key]]] \
	  "{Try Again} {channelJoin ${chan}}" {Cancel {}}
    } {
	mkEntryBox .@key Key "Enter key for channel ${chan}:" \
	  "{Key [set ${chan}Key]}" "Join {channelJoin ${chan}}" {Cancel {}}
    }
}

