#
proc doMisc2 {chid win} {
    set line [$chid addToHist [$win get]]
    $win delete 0 end
    if [regexp {^(/[a-zA-Z]+)( (.*))?$} $line dummy pr r1 rest] {
	set pr [string toupper $pr]
	if ![string match {} [info procs $pr]] {
	    $chid addText {} "!> $line"
	    $pr $chid $rest
	    return
	} 
    }
    $chid send $line
}
#
proc /ADMIN {chid args} { sillyPerson $chid }
#
proc /ALIAS {chid args} { sillyPerson $chid }
#
proc /ASSIGN {chid args} { sillyPerson $chid }
#
proc /AWAY {chid args} {  doAway $args }
#
proc /BIND {chid args} { sillyPerson $chid }
#
proc /BYE {chid args} { doQuit [lindex $args 0]}
#
proc /CD {chid args} { sillyPerson $chid }
#
proc /CHANNEL {chid args} { sillyPerson $chid }
#
proc /CLEAR {chid args} { sillyPerson $chid }
#
proc /COMMENT {chid args} { sillyPerson $chid }
#
proc /CONNECT {chid args} { sillyPerson $chid }
#
proc /CTCP {chid args} { sillyPerson $chid }
#
proc /DATE {chid args} { sillyPerson $chid }
#
proc /DCC {chid args} { sillyPerson $chid }
#
proc /DEOP {chid args} { deIRCOp }
#
proc /DESCRIBE {chid args} { sillyPerson $chid }
#
proc /DIE {chid args} { sillyPerson $chid }
#
proc /DIGRAPH {chid args} { sillyPerson $chid }
#
proc /DMSG {chid args} { sillyPerson $chid }
#
proc /DQUERY {chid args} { sillyPerson $chid }
#
proc /ECHO {chid args} { sillyPerson $chid }
#
proc /ENCRYPT {chid args} {
    sillyPerson $chid
}
#
proc /EVAL {chid args} { sillyPerson $chid }
#
proc /EXEC {chid args} { sillyPerson $chid }
#
proc /EXIT {chid args} { sillyPerson $chid }
#
proc /FLUSH {chid args} { sillyPerson $chid }
#
proc /FOREACH {chid args} { sillyPerson $chid }
#
proc /HELP {chid args} { sillyPerson $chid }
#
proc /HISTORY {chid args} { sillyPerson $chid }
#
proc /HOOK {chid args} { sillyPerson $chid }
#
proc /IF {chid args} { sillyPerson $chid }
#
proc /IGNORE {chid args} { sillyPerson $chid }
#
proc /INFO {chid args} { sillyPerson $chid }
#
proc /INPUT {chid args} { sillyPerson $chid }
#
proc /INVITE {chid args} { eval sendIRC INVITE [$chid name] $args }
#
proc /JOIN {chid args} {
    channelJoin [lindex $args 0]
}
#
proc /KICK {chid args} { eval sendIRC KICK $args }
#
proc /KILL {chid args} { eval sendIRC KILL $args }
#
proc /LASTLOG {chid args} { sillyPerson $chid }
#
proc /LEAVE {chid args} { /PART $chid $args }
#
proc /LINKS {chid args} { eval sendIRC LINKS $args }
#
proc /LIST {chid args} { sillyPerson $chid }
#
proc /LOAD {chid args} { sillyPerson $chid }
#
proc /LUSERS {chid args} { sillyPerson $chid }
#
proc /ME {chid args} { $chid action [lindex $args 0] }
#
proc /MLOAD {chid args} { sillyPerson $chid }
#
proc /MODE {chid args} { eval sendIRC MODE [lindex $args 0] }
#
proc /MOTD {chid args} { sendIRC MOTD [lindex $args 0] }
#
proc /MSG {chid args} {
    set args [lindex $args 0]
    sendIRC PRIVMSG [lindex $args 0] [join [lrange $args 1 end]]
}
#
proc /NAMES {chid args} { sillyPerson $chid }
#
proc /NICK {chid args} { setNickname [lindex $args 0] }
#
proc /NOTE {chid args} { sillyPerson $chid }
#
proc /NOTICE {chid args} { sillyPerson $chid }
#
proc /NOTIFY {chid args} { sillyPerson $chid }
#
proc /ON {chid args} { sillyPerson $chid }
#
proc /OPER {chid args} {
    set args [lindex $args 0]
    sendIRC OPER [lindex $args 0] [lindex $args 1]
}
#
proc /PARSEKEY {chid args} { sillyPerson $chid }
#
proc /PART {chid args} {
    set args [lindex $args 0]
    if [string match {} $args] {
	$chid leave
    } {
	if ![string match {nil} [set id [find [lindex $args 0]]]] {
	    $id leave
	}
    }
}
#
proc /PING {chid args} {
    foreach nk $args {
	doCtcp PING ${nk}
    }
}
#
proc /QUERY {chid args} {
    set msg [Message :: make [set usr [User :: make [lindex $args 0]]]]
    $msg show
    $msg addUser $usr 0 0
}
#
proc /QUIT {chid args} { doQuit [lindex $args 0] }
#
proc /QUOTE {chid args} { sillyPerson $chid }
#
proc /REDIRECT {chid args} { sillyPerson $chid }
#
proc /REHASH {chid args} { sendIRC REHASH }
#
proc /RESTART {chid args} { sendIRC RESTART }
#
proc /SAVE {chid args} { sillyPerson $chid }
#
proc /SAY {chid args} { sillyPerson $chid }
#
proc /SEND {chid args} { sillyPerson $chid }
#
proc /SENDLINE {chid args} { sillyPerson $chid }
#
proc /SERVER {chid args} {
    set args [lindex $args 0]
    set sv [Server :: make [lindex $args 0]]
    if ![string match {} [set port [lindex $args 1]]] {
	$sv configure -port $port
    }
    changeServer $sv
}
#
proc /SET {chid args} { sillyPerson $chid }
#
proc /SIGNOFF {chid args} { doQuit [lindex $args 0] }
#
proc /SLEEP {chid args} { sillyPerson $chid }
#
proc /SQUIT {chid args} { sillyPerson $chid }
#
proc /STATS {chid args} { sillyPerson $chid }
#
proc /SUMMON {chid args} { sillyPerson $chid }
#
proc /TIME {chid args} { sillyPerson $chid }
#
proc /TIMER {chid args} { sillyPerson $chid }
#
proc /TOPIC {chid args} { $chid configure -topic [lindex $args 0] }
#
proc /TRACE {chid args} { sillyPerson $chid }
#
proc /TYPE {chid args} { sillyPerson $chid }
#
proc /USERHOST {chid args} { sillyPerson $chid }
#
proc /USERS {chid args} { sillyPerson $chid }
#
proc /VERSION {chid args} { sillyPerson $chid }
#
proc /WAIT {chid args} { sillyPerson $chid }
#
proc /WALLOPS {chid args} { sillyPerson $chid }
#
proc /WHICH {chid args} { sillyPerson $chid }
#
proc /WHILE {chid args} { sillyPerson $chid }
#
proc /WHO {chid args} { eval sendIRC WHO [lindex $args 0] }
#
proc /WHOIS {chid args} { eval sendIRC WHOIS [lindex $args 0] }
#
proc /WHOWAS {chid args} { eval sendIRC WHOWAS [lindex $args 0] }
#
proc /WINDOW {chid args} { sillyPerson $chid }
#
proc /XECHO {chid args} { sillyPerson $chid }
#
proc /XTYPE {chid args} { sillyPerson $chid }
#
proc sillyPerson {chid} {
    mkDialog {} .@silly Silly {Don't be silly!!!} {} {OK {}}
    tkwait window .@silly
}
