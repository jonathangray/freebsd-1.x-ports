#
#	Users cmds
#
proc doInvite {nick chan} {
    if ![string match {} ${chan}] { sendIRC INVITE $nick ${chan} }
}
#
proc userInvite {chan} {
    mkEntryBox .@invite Invite {Enter user and channel:} \
      "{User {}} {Channel ${chan}}" {OK {doInvite}} {Cancel {}}
}
#
proc doMsg {nk} {
    if ![string match {} $nk] { Message :: make $nk }
}
#
proc doUNotice {nick text} {
    if {![string match {} $nick] && ![string match {} $text]} {
	sendIRC NOTICE $nick $text
    }
}
#
proc doWhowas {nick count} {
    if ![string match {} $nick] { sendIRC WHOWAS $nick $count }
}
#
proc userMsg {} {
    mkEntryBox .@msg Message {Enter user name:} \
      {{User {}}} {OK {doMsg}} {Cancel {}}
}
#
proc userFinger {} {
    mkEntryBox .@msg Finger {Enter user name:} \
      {{User ""}} {OK {finger}} {Cancel {}}
}
#
proc userNotice {} {
    mkEntryBox .@not Message {Enter user name and notice text:} \
      {{User {}} {Notice {}}} {OK {doUNotice}} {Cancel {}}
}
#
proc userWhois {} {
    mkEntryBox .@wis Whois {Enter user name and server:} \
      {{User {}} {Where {}}} {OK {doWhois}} {Cancel {}}
}
#
proc userWhowas {} {
    mkEntryBox .@was Whowas {Enter user name and count:} \
      {{User {}} {Count {}}} {OK {doWhowas}} {Cancel {}}
}
#
proc doUMode {nk mode} {
    if {![string match {} $nk] && [string match {[+-]*} mode]} {
	sendIRC MODE $nk $mode
    }
}
#
proc userMode {} {
    global myid
    mkEntryBox .@umode {User Mode} {Enter user and mode:} \
      "{User [$myid name]} {Mode {}}" {OK {doUMode}} {Cancel {}}
}
#
proc userCmd {cmd args} {
    case $cmd {
    Mode { userMode }
    Finger {userFinger }
    Invite { userInvite {} }
    Msg {userMsg}
    Notice {userNotice}
    Kill {userKill}
    Whois {userWhois}
    Whowas {userWhowas}
    default {
	    mkEntryBox .@$cmd "$cmd" {Enter user pattern:} {{User {}}}\
	      "OK {sendIRC [string toupper $cmd]}" {Cancel {}}
	}
    }
}
