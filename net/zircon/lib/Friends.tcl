#
# Handle all stuff related to Friends menu/window
#
#
proc friends {op args} {
    switch $op {
    enable { eval friends_state normal $args }
    disable { eval friends_state disabled $args }
    default { eval friends_$op $args }
    }
}
#
proc friends_rename {usr nnk} {
    if [friends menu] {
	if {[set x [indexHack .oFrm.bf1.friends.menu [$usr name] 0]] >= 0} {
	    .oFrm.bf1.friends.menu entryconfigure $x -label $nnk
	}
    } {
	if [winfo exists .@friends.users.userList.frame.$usr] {
	    .@friends.users.userList.frame.$usr config -text $nnk
	}
    }
}
#
proc friends_absent {usr} {
    if [friends menu] {
	if {[set x [indexHack .oFrm.bf1.friends.menu [$usr name] 0]] >= 0} {
	    set v [.oFrm.bf1.friends.menu entryconfigure $x -state]
	    return ![string match {normal} [lindex $v 4]]
	}
	return 0	
    } {
	return [expr {[winfo exists .@friends.users.userList.frame.$usr] \
	      && ![normal .@friends.users.userList.frame.$usr]}]
    }
}
#
proc friends_menu {} {
    global friendsStyle ; return [string match {menu} $friendsStyle]
}
#
proc friends_mark {usr what} {
    if [friends menu] {
# Dont do anything at the moment
    } \
    elseif [winfo exists .@friends.users.userList.frame.$usr] {
	markButton .@friends.users.userList.frame.$usr $what
	popup .@friends
    }
}
#
proc friends_delete {} {
    if [winfo exist .@friends] { destroy .@friends }
}
#
proc friends_state {state args} {
    if [friends menu] {
	foreach id $args {
	    if {[set x [indexHack .oFrm.bf1.friends.menu $id 0]] >= 0} {
		.oFrm.bf1.friends.menu entryconfigure $x -state $state
	    }
	}
    } {
	foreach id $args {
	    if [winfo exists .@friends.users.userList.frame.$id] {
		.@friends.users.userList.frame.$id conf -state $state
	    }
	}
    }
}
#
proc Friends {name args} {
    if [friends menu] {
	makeFriendsMenu .oFrm.bf1.friends.menu
    } \
    elseif [winfo exists .@friends] {
	popup .@friends
    } {
	makeFriends
    }
}
#
proc makeFriendsMenu {win} {
    menu $win
    insertFriends
}
#
proc makeFriends {} {
    toplevel .@friends -class Zircon
    wm title .@friends "Friends"
    wm grid .@friends 10 10 10 10
    wm minsize .@friends 10 1

    set win [frame .@friends.users -relief raised]
    scrollbar $win.vscroller -command "$win.userList yview" 
    canvas $win.userList -yscrollcommand "$win.vscroller set"
    frame $win.userList.frame -border 0
    $win.userList create window 0 0 -window $win.userList.frame -anchor nw
    pack $win.vscroller -side right -fill y
    pack $win.userList -side left -fill y
    button $win.ok -text OK -command {killWindow .@friends }
    pack $win.ok -expand 1 -side right -fill y
    pack .@friends.users -expand 1 -fill y
    menubutton $win.userList.frame.@d -text None
    set wd [winfo reqwidth $win.userList.frame.@d]
    set sht [set ht [winfo reqheight $win.userList.frame.@d]]
    set ht [expr { $ht * [llength [winfo children $win.userList.frame]]}]
    $win.userList conf \
      -width $wd -scrollregion [list 0 0 $wd $ht] -scrollincrement $sht
    $win.userList conf -width $wd -height $ht
    destroy $win.userList.frame.@d
    insertFriends
}
#
proc insertFriends {} {
    global friendsOn UTO myid
    foreach fr [array names UTO] {
	set usr $UTO($fr)
	if {[$usr isFriend] && $usr != $myid} {
	    if $friendsOn {
		$usr notify 1
		if [$usr ison] { friends add $usr }
	    } {
		friends add $usr
	    }
	}
    }
    if $friendsOn { sendISON }
}
#
proc friends_add {usr} {
    if ![string match {nil} $usr] {
	if [friends menu] {
	    set w .oFrm.bf1.friends.menu
	    if ![winfo exists $w.$usr] {
		$w add cascade -label [$usr name] -menu $w.$usr
		makeUserMenu nil $w.$usr $usr
	    }
	} \
	elseif [winfo exists .@friends] {
	    set win .@friends.users.userList
	    set winf $win.frame
	    if ![winfo exists $winf.$usr] {
		menubutton $winf.$usr -text [$usr name] -menu $winf.$usr.menu
		makeUserMenu nil $winf.$usr.menu $usr
		set wd [winfo reqwidth $winf.$usr]
		set sht [set ht [winfo reqheight $winf.$usr]]
		set ht [expr { $ht * [llength [winfo children $winf]]}]
		$win conf -width $wd -scrollregion [list 0 0 $wd $ht] \
		  -scrollincrement $sht -height $ht
#		$win conf -width $wd -height $ht
		pack $winf.$usr
		if [$usr ison] { markButton $winf.$usr ison }
	    }
	}
    }
}
#
proc friends_remove {usr} {
    if [friends menu] {
	if {[set x [indexHack .oFrm.bf1.friends.menu [$usr name] 0]] >= 0} {
	    .oFrm.bf1.friends.menu delete $x
	}
    } {
	if [winfo exists .@friends.users.userList.frame.$usr] {
	    global friendsOn
	    if $friendsOn {
		if [normal .@friends.users.userList.frame.$usr] {
		    destroy .@friends.users.userList.frame.$usr
		}
	    } {
		friends mark $usr {}
	    }
	    popup .@friends
	}
    }
}



