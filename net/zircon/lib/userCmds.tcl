#
#	Users cmds
#
proc makeFriends {} {
    if [winfo exists .@friends] {
	wm deiconify .@friends
	raise .@friends
	return
    }
    toplevel .@friends -class Zircon
    wm title .@friends "Friends"
    wm grid .@friends 10 10 10 10
    wm minsize .@friends 10 1

    set win [frame .@friends.users -relief raised]
    scrollbar $win.vscroller -command "$win.userList yview" 
    canvas $win.userList -yscrollcommand "$win.vscroller set"
    frame $win.userList.frame -border 0
    $win.userList create window 0 0 -window $win.userList.frame -anchor nw
    zpack $win {userList vscroller} {left filly}
    button $win.ok -text OK -command {killWindow .@friends }
    zpack $win ok {expand filly}
    pack append .@friends $win {expand filly}
    global friendsOn
    global userInfo
    if $friendsOn {
	global TellMe
	global notify
	global ISON
	foreach fr $userInfo {
	    set who [lindex $fr 0]
	    set lwho [string tolower $who]
	    if [info exists ISON($lwho)] { doNewFriend $who }
	    set TellMe($lwho) 1
	    if {[lsearch $notify $lwho] < 0} { lappend notify $lwho }
	}
	sendISON
    } {
	foreach fr $userInfo { doNewFriend [lindex $fr 0] }
    }
}

proc doNewFriend {fr} {
    if {$fr != ""} {
	set win .@friends.users.userList
	set winf $win.frame
	set nm [string tolower $fr]
	if {[winfo exists $winf.$nm]} { return }
	menubutton $winf.$nm -text $fr -menu $winf.$nm.menu
	makeUserMenu "" $winf.$nm.menu $nm 0 0
	set wd [winfo reqwidth $winf.$nm]
	set sht [set ht [winfo reqheight $winf.$nm]]
	set ht [expr { $ht * [llength [winfo children $winf]]}]
	$win conf \
	  -width $wd -scrollregion [list 0 0 $wd $ht] -scrollincrement $sht
	$win conf -width $wd -height $ht
	zpack $winf $nm { }
	global ISON
	if [info exists ISON($nm)] { markButton $winf.$nm ison }
    }
}

proc doInvite {nick chan} {
    if {${chan} != ""} { sendIRC INVITE $nick ${chan} }
}

proc userInvite chan {
    mkEntryBox .@invite "Invite" "Enter user and channel:" \
      "{User {}} {Channel $chan}" {OK {doInvite}} {Cancel {}}
}

proc doMsg nick {
    if {$nick != ""} { makeChannel $nick M}
}

proc doNotice {nick text} {
    if {$nick != {} && $text != {}} { sendIRC NOTICE $nick $text }
}

proc doWhowas {nick count} {
    if {$nick != {}} { sendIRC WHOWAS $nick $count }
}

proc userMsg {} {
    mkEntryBox .@msg "Message" "Enter user name:" \
      {{User ""}} {OK {doMsg}} {Cancel {}}
}

proc userFinger {} {
    mkEntryBox .@msg "Finger" "Enter user name:" \
      {{User ""}} {OK {finger}} {Cancel {}}
}

proc userNotice {} {
    mkEntryBox .@not "Message" "Enter user name and notice text:" \
      {{User {}} {Notice {}}} {OK {doNotice}} {Cancel {}}
}

proc userWhois {} {
    mkEntryBox .@wis "Whois" "Enter user name and server:" \
      {{User {}} {Where {}}} {OK {doWhois}} {Cancel {}}
}

proc userWhowas {} {
    mkEntryBox .@was "Whowas" "Enter user name and count:" \
      {{User {}} {Count {}}} {OK {doWhowas}} {Cancel {}}
}

proc buildDCCList {} {
    set oFrm .@dcclist
    if [winfo exists .@dcclist] {
	wm deiconify .@dcclist
	raise .@dcclist
	$oFrm.dcc.list delete 0 end
    } {
	toplevel .@dcclist -class Zircon -relief raised -borderwidth 2
	wm title .@dcclist "DCC Connections"
	wm minsize .@dcclist 10 1
	frame $oFrm.dcc -relief raised
	scrollbar $oFrm.dcc.vscroller -command {.@dcclist.dcc.list yview}
	listbox $oFrm.dcc.list \
	  -xscrollcommand {.@dcclist.hsFrm.hscroller set} \
	  -yscrollcommand {.@dcclist.dcc.vscroller set} -setgrid 1
	zpack $oFrm.dcc list {left expand fill}
	zpack $oFrm.dcc vscroller {left filly} 

	frame $oFrm.hsFrm
	scrollbar $oFrm.hsFrm.hscroller -command {.@dcclist.dcc.list xview} \
	  -orient horizontal

	frame $oFrm.hsFrm.pf0
	zpack $oFrm.hsFrm hscroller {left expand fillx}
	zpack $oFrm.hsFrm pf0 {left padx 20} 

	frame $oFrm.btn
	button $oFrm.btn.ok -text OK -command {destroy .@dcclist} -relief raised
	button $oFrm.btn.clear -text Close -relief raised \
	  -command { dccClose .@dcclist.dcc.list }
	zpack $oFrm.btn {ok clear} {left expand fillx}
	zpack $oFrm dcc {fill}
	zpack $oFrm {hsFrm btn} {fillx}
    }
    global AChat
    global Chat
    set AChat(@me) 0 ; unset AChat(@me)
    set Chat(@me) 0 ; unset Chat(@me)
    foreach nn [array names AChat] {
	$oFrm.dcc.list insert end "Call to $nn"
    }
    foreach nn [array names Chat] {
	$oFrm.dcc.list insert end "Chat to $nn"
    }
    global ASend
    global Send
    set ASend(@me) 0 ; unset ASend(@me)
    set Send(@me) 0 ; unset Send(@me)
    foreach nn [array names ASend] {
	foreach fl $ASend($nn) {
	    $oFrm.dcc.list insert end "Offer to $nn : [lindex $fl 1]"
	}
    }
    foreach nn [array names Send] {
	foreach fl $Send($nn) {
	    $oFrm.dcc.list insert end "Send to $nn : [lindex $fl 1]"
	}
    }
    set Get(@me) 0 ; unset Get(@me)
    foreach nn [array names Get] {
	foreach fl $Get($nn) {
	    $oFrm.dcc.list insert end "Get from $nn : [lindex $fl 1]"
	}
    }
}

proc usersDCC cmd {
    case $cmd {
    List { buildDCCList }
    Close { buildDCCList }
    default {
	    mkEntryBox .@$cmd "$cmd" "Enter user name for DCC $cmd:" \
	      {{User ""}} "OK {doDCC [string toupper $cmd]}" {Cancel {}}
	}
    }
}

proc userCmd {cmd args} {
    case $cmd {
    Finger {userFinger }
    Invite { userInvite "" }
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
