#
#	Procedure to build up a channel window. Conversation windows
#	have fewer buttons and no user area down the side.
#
#	Channel names with characters like $ in them are currently
#	a problem.....
#
proc cleanup name {
    set pattern "\[ ,\n\r\]"
    regsub -all "$pattern" $name {} name
    return name
}

proc makePalette chan {
    set w .@palette
    if [winfo exists $w] { wm deiconify $w ; raise $w ; return }
    global Ft
    killWindow $w
    toplevel $w -class Zircon
    wm title $w {Zircon Palette}
    for {set i 2} {$i < 16} {incr i} {
	frame $w.f$i
	for {set j 0} {$j < 16} {incr j} {
	    set c [format "%c" [expr {$i * 16 + $j}]]
	    button $w.f$i.l$j -text $c -relief raised -width 1 \
	      -command "\[focus\] insert insert $c"
	    pack append $w.f$i $w.f$i.l$j left
	}
	pack append $w $w.f$i top
    }
    button $w.ok -text OK -command "destroy $w"
    pack append $w $w.ok fillx
}

proc chanSet {chan array flag} {
    global $array
    chanUpdate [set ${array}(${chan})] ${chan} 0 $flag
}

proc drawSet chan {
    chanSet ${chan} Draw noDraw
    global Draw
    set mn .${chan}.oFrm.cmds.cmds0.channel.menu
    $mn entryconfigure [$mn index {Draw}] \
      -state [expr {$Draw(${chan}) ? "disabled" : "normal"}]
}

proc setChanVars chan {
    global popUp ; global Open ; set Open(${chan}) $popUp
    global popDown ; global Close ; set Close(${chan}) $popDown
    global noJump ; global Jump ; set Jump(${chan}) $noJump
    global quiet ; global Quiet ; set Quiet(${chan}) $quiet
    global noDraw ; global Draw ; set Draw(${chan}) $noDraw
    global Actions ; set Actions(${chan}) 0
    foreach auto [getAuto ${chan}] {
	case $auto {
	open	{ set Open(${chan}) 1 }
	close	{ set Close(${chan}) 1 }
	nodraw	{ set Draw(${chan}) 1 }
	nojump	{ set Jump(${chan}) 1 }
    	quiet	{ set Quiet(${chan}) 1 }
	}
    }
    global History ;
    if {[set History(${chan}) [getHistory ${chan}]] == {} } {
	global history ; set History(${chan}) $history
    }
    global CloseTime
    global closeTime
    if {[set CloseTime(${chan}) [getCloseTime ${chan}]] == {} } {
	set CloseTime(${chan}) $closeTime
    } {
	set CloseTime(${chan}) [expr {$CloseTime(${chan}) * 1000}]
	global testTime
	if {$CloseTime(${chan}) < $testTime} {
	    set testTime $CloseTime(${chan})
	}
	if {$closeTime == 0} { set closeTime $CloseTime(${chan}) }
    }
    global testTime
    if {$CloseTime(${chan}) != 0 && $CloseTime(${chan}) < $testTime} {
	set testTime $CloseTime(${chan})
    }
    global CloseCount
    set CloseCount(${chan}) $CloseTime($chan)
}

proc makeChannel {chan type} {
    set rchan ${chan}
    set chan [set lrchan [string tolower ${chan}]]
    case $type {
    D { set chan ~${chan} ; set dcc 1}
    default { set dcc 0 }
    }
    global HPos ; set HPos(${chan}) 0
    global HBuff ; set HBuff(${chan}) {}
    global Active ; set Active(${chan}) 0
    setChanVars ${chan}
    set realChan [expr {[channel ${chan}] && $type != "M"}]
    if [winfo exists .${chan}] { wm deiconify .${chan} ; raise .${chan} ; return}
    if {[set dot [string first "." ${chan}]] >= 0} {
	set bits [split ${chan} .]
	set name [lindex $bits 0]
	set win .${chan}
	toplevel $win -class Zircon
	set prv $win
	foreach bit [lrange $bits 1 end] {
	    append name ".$bit"
	    frame .${chan} -border 0
	    pack append $prv .${chan} {expand}
	    set prv .${chan}
	}
    } {
	set win .${chan}
	toplevel $win -class Zircon
    }
    global Name ; set Name($win) ${chan}
    global monitor 
    if {[set nn [lsearch $monitor ${chan}]] >= 0} {
	catch "destroy .@mon${chan}"
	listdel monitor $nn
    }
    global activeChannels
    if {[lsearch $activeChannels ${chan}] < 0 && $type != "N" && !$dcc} {
	lappend activeChannels ${chan}
    }
    case $type {
    C {set lp "IRC Channel " ; set hgh 24 ; set wname ${rchan}}
    N {set lp "IRC Notice from" ; set hgh 10 ; set wname ${rchan}}
    D {set lp {DCC Chat with} ; set hgh 16 ; set wname "Chat ${rchan}"}
    M {set lp "IRC Conversation with" ; set hgh 16 ; set wname ${rchan}}
    }
    wm title $win "$lp $rchan"
    wm minsize $win 40 1
    setIcon $win ${chan} ${wname}

    set w .${chan}
    set oFrm [frame $w.oFrm -relief raised]

    if {$realChan} {
	set wu [frame $w.users -relief raised]
	scrollbar ${wu}.vscroller -command "$wu.userList yview" 
	canvas $wu.userList -yscrollcommand "$wu.vscroller set"
	frame $wu.userList.frame -border 0
	$wu.userList create window 0 0 -window $wu.userList.frame -anchor nw
	zpack $wu {userList vscroller} {left filly} 
	zpack $w users {right filly}

	global lcNickname
	global nickname
	global ${chan}Spk
	global ${chan}Op ; set ${chan}Op($lcNickname) 0
	set ${chan}Spk($lcNickname) 0
	menubutton $wu.userList.frame.@me -text $nickname \
	  -menu $wu.userList.frame.@me.menu
	makeUserMenu ${chan} $wu.userList.frame.@me.menu $lcNickname 0 0
	set wd [winfo reqwidth $wu.userList.frame.@me]
	set ht [winfo reqheight $wu.userList.frame.@me] 
	$wu.userList conf -width $wd  -scrollincrement $ht \
	  -scrollregion [list 0 0 $wd $ht]
	zpack $wu.userList.frame @me {}
	frame $oFrm.topic
	menubutton $oFrm.topic.label -text Topic -relief raised
	$oFrm.topic.label conf -menu $oFrm.topic.label.menu
	set om [menu $oFrm.topic.label.menu]
	$om add command -label New -command "getTopic ${chan}"
	$om add separator
	foreach nn [getTopics ${chan}] {
	    $om add command -label "[prune $nn]" \
	      -command "doTopic ${chan} {${nn}}"
	}
	emacsTEntry $oFrm.topic.entry -relief raised
	zpack $oFrm.topic label left
	zpack $oFrm.topic entry {left expand fillx}
	bind $oFrm.topic.entry <Return> {sendTopic %W ; notIdle %W}
    }

    frame $oFrm.cmds -borderwidth 0
    set of [frame $oFrm.cmds.cmds0]
    set om [makeMB $of.mode Mode]
    $om add checkbutton -label {Pop Up} -variable Open(${chan}) \
	-command "chanSet ${chan} Open open"
    $om add checkbutton -label {Pop Down} -variable Close(${chan}) \
	-command "chanSet ${chan} Close close"
    if !$dcc {
	$om add checkbutton -label {No Draw} -variable Draw(${chan}) \
	  -command "drawSet ${chan}"
    }
    $om add checkbutton -label {No Jump} -variable Jump(${chan}) \
	-command "chanSet ${chan} Jump nojump"
    $om add checkbutton -label Quiet -variable Quiet(${chan}) \
	-command "chanSet ${chan} Quiet quiet"
    if !$dcc {
	$om add checkbutton -label Actions -variable Actions(${chan}) \
	  -command "flipActions ${chan}"
    }
    global Draw
    if $realChan {
	global ${chan}
	foreach vl {{p Private} {m Moderated} {s Secret} {i {Invite Only}} \
	  {t Topic} {n {No Msg}}} {
	    set v [lindex $vl 0]
	    set ${chan}($v) 0
	    $om add checkbutton -command "changeMode ${chan} $v" \
	      -variable ${chan}(${v}) -state disabled -label [lindex $vl 1]
	}	
	$om add command -command "setBan ${chan}" -state disabled -label Ban
	$om add command -command "setLimit ${chan}" -state disabled -label Limit
	$om add command -command "setKey ${chan}" -state disabled -label Key

	set om [makeMB $of.channel Channel]
	$om add command -command "sendIRC WHO ${lrchan}" -label Who
	$om add command -command "channelInvite ${chan}" -label Invite
	$om add command -command "channelNotice ${chan}" -label Notice
	$om add command -command "makeZDraw ${chan}" -label Draw
	if $Draw(${chan}) { $om entryconfigure 3 -state disabled }
	addCTCPMenu $om ${chan}
    } {
	set om [makeMB $of.channel User]
	$om add command -command "sendIRC WHOIS ${lrchan}" -label Whois
	$om add command  -command "channelNotice ${lrchan}" -label Notice
	if !$dcc {
	    $om add command -command "makeZDraw ${chan}" -label Draw
	    if $Draw(${chan}) { $om entryconfigure 2 -state disabled }
	}
	addCTCPMenu $om ${lrchan}
	addDCCMenu $om ${lrchan}
    }
    $om add cascade -label Log -menu $om.log
    menu $om.log
    foreach cmd {Close Open Flush Empty} {
	$om.log add command -label $cmd -command "doLog ${chan} $cmd" \
	  -state disabled
    }
    global LogFile
    if {[set LogFile(${chan}) [getLog ${chan}]] != {}} {
	global Log
	if [catch "open $LogFile(${chan}) a" Log(${chan})] {
	    addText ERROR @info \
	      "*** Cannot open log file $LogFile(${chan}) for channel ${chan} : $Log(${chan})"
	    unset Log(${chan})
	    $om.log entryconfigure 1 -state normal
	} {
	    $om.log entryconfigure 0 -state normal
	    $om.log entryconfigure 2 -state normal
	    $om.log entryconfigure 3 -state normal
	}
    } {
	$om.log entryconfigure 1 -state normal
    } 
    set om [makeMB $of.action Action]
    if !$dcc {
	$om add command -label New -command "getAction ${chan}"
	$om add separator

	global actions
	foreach act $actions {
	    $om add command -label "[prune $act]" -command "sendAction ${chan} {$act}"
	}
    } {
	$of.action configure -state disabled
    }
    frame $oFrm.cmds.cmds1
    set lp [expr {$dcc ? "leaveDCC ${lrchan}" : "leaveChannel ${chan}"}]
    button $oFrm.cmds.cmds1.quit -command "$lp" -text Leave
    bind $oFrm.cmds.cmds1.quit <Shift-1> {tk_butDown %W}
    bind $oFrm.cmds.cmds1.quit <Shift-ButtonRelease-1> {
	set sc [%W configure -command]
	%W configure -command quitZircon
	tk_butUp %W
	%W configure -command $sc
    }
    if $realChan {
	bind $oFrm.cmds.cmds1.quit <Control-1> {tk_butDown %W}
	bind $oFrm.cmds.cmds1.quit <Control-ButtonRelease-1> "
	    tk_butUp %W
	    global monitor
	    lappend monitor ${chan}
	    sendIRC NAMES ${chan}
	"
    }

    button $oFrm.cmds.cmds1.clear -command "clearChannel ${chan} 0" -text Clear
    bind $oFrm.cmds.cmds1.clear <Shift-1> {tk_butDown %W}
    bind $oFrm.cmds.cmds1.clear <Shift-ButtonRelease-1> "
	set sc \[%W configure -command\]
	%W configure -command {clearChannel ${chan} 1}
	tk_butUp %W
	%W configure -command \$sc
    "
    zpack $of {mode channel action} {left expand fillx} 
    tk_menuBar $oFrm.cmds.cmds0 $oFrm.cmds.cmds0.mode \
      $oFrm.cmds.cmds0.channel $oFrm.cmds.cmds0.action

    zpack $oFrm.cmds.cmds1 {quit clear} {left expand fillx} 
    zpack $oFrm.cmds {cmds0 cmds1} {left expand fillx}

    set oft [frame $oFrm.textFrm -relief raised]
    scrollbar $oft.vscroller -command "doScroll $oft.text"
    set ot \
      [text $oft.text -yscrollcommand "setScroll $oft.text $oft.vscroller" -height $hgh]
    rebind $ot
    global BF
    global Fg
    global Bg
    global Ft
    set BF(${chan}) [getOValue $ot font boldFont Font]
    set Ft(${chan}) [getOValue $ot font font Font]
    set Fg(${chan}) [getOValue $ot foreground foreground Foreground]
    set Bg(${chan}) [getOValue $ot background background Background]
    $oft.text conf -selectforeground $Bg(${chan}) -selectbackground $Fg(${chan})
    zpack $oft vscroller {right filly} 
    zpack $oft text {left expand fill}

    set om [frame $oFrm.cmdLine -relief raised]
    scrollbar $om.cscroller -orient horizontal -command "$om.commandLine view"
    emacsEntry $om.commandLine -scrollcommand "$om.cscroller set"
    zpack $om {commandLine cscroller} {expand fillx}

    if {$realChan} { zpack $oFrm topic fillx }

    zpack $oFrm cmds {top fillx}
    zpack $oFrm cmdLine {bottom fillx}
    zpack $oFrm textFrm {expand fill}
    zpack $w oFrm {left expand fill}

    set occ $oFrm.cmdLine.commandLine
    bind $occ <Meta-b> \
      { notIdle %W ; %W insert insert \002 ;tk_entrySeeCaret %W }
    bind $occ <Meta-v> \
      { notIdle %W ; %W insert insert \026 ; tk_entrySeeCaret %W }
    bind $occ <Meta-u> \
      { notIdle %W ; %W insert insert \037 ; tk_entrySeeCaret %W }
    bind $occ <Meta-s> {
	notIdle %W
	global smiley ; %W insert insert $smiley
	tk_entrySeeCaret %W
    }
    bind $occ <Shift-Meta-S> {
	notIdle %W
	global scowl ; %W insert insert $scowl
	tk_entrySeeCaret %W
    }
    bind $occ <Control-Meta-s> {
	notIdle %W
	global wink ; %W insert insert $wink
	tk_entrySeeCaret %W
    }
    bind $occ <Meta-j> {
	notIdle %W
	if {![catch {selection get} bf] && $bf != {}} {
	    channelJoin [cleanup $bf]
	}
    }
    bind $occ <Meta-m> {
	notIdle %W
	if {![catch {selection get} bf] && $bf != {}} {
	    makeChannel [string range [cleanup $bf] 0 8] M
	}
    }
    bind $occ <Meta-f> {
	notIdle %W
	if {![catch {selection get} bf] && $bf != {}} {
	    finger [string range [cleanup $bf] 0 8]
	}
    }
    bind $occ <Meta-q> {
	notIdle %W
	set x [split [%W get]]
	sendIRC PRIVMSG [lindex $x 0] [join [lrange $x 1 end]]
	%W delete 0 end
    }
    bind $occ <Meta-w> {
	notIdle %W
	sendIRC WHOIS [%W get]
	%W delete 0 end
    }
    bind $occ <Shift-Meta-W> {
	notIdle %W
	sendIRC WHO [%W get]
	%W delete 0 end
    }
    bind $occ <Any-KeyPress> {
	notIdle %W
	if {"%A" != {}} { %W insert insert %A ; tk_entrySeeCaret %W }
    }
    bind $occ <Delete> \
      { notIdle %W ; tk_entryBackspace %W; tk_entrySeeCaret %W }
    bind $occ <BackSpace> \
      { notIdle %W ; tk_entryBackspace %W; tk_entrySeeCaret %W }
    bind $occ <Control-h> \
      { notIdle %W ; tk_entryBackspace %W; tk_entrySeeCaret %W }
    bind $occ <Control-d> { notIdle %W ; %W delete insert }
    bind $occ <Control-u> { notIdle %W ; %W delete 0 end }
    bind $occ <Control-w> \
      { notIdle %W ; tk_entryBackword %W; tk_entrySeeCaret %W }
    bind $occ <Escape> " notIdle %W ; makePalette ${chan} "
    set lp [expr {$dcc ? "sendToDCC $lrchan" : "sendToChannel ${chan}"}]
    bind $occ <Return> "
	$lp \[addToHist ${chan} \[%W get\]\]
	%W delete 0 end
    "
    if !$dcc {
	bind $occ <Shift-Return> "
	    sendAction ${chan} \[addToHist ${chan} \[%W get\]\]
	    %W delete 0 end
	"
    }
    bind $occ <Control-Return> "
	doNotice ${chan} \[addToHist ${chan} \[%W get\]\]
	%W delete 0 end
    "
    bind $occ <Control-p> "
	notIdle %W
	%W delete 0 end ; %W insert insert \[getPrev ${chan}\]
	tk_entrySeeCaret %W
    "
    bind $occ <Control-n> "
	notIdle %W
	%W delete 0 end ; %W insert insert \[getNext ${chan}\]
	tk_entrySeeCaret %W
    "
    set lp [expr {$dcc ? "insertDCCSelect $lrchan" : "insertSelect ${chan}"}]
    bind $occ <ButtonPress-2> "notIdle %W ; $lp %W ; tk_entrySeeCaret %W"
    setBindings $occ ${chan}
    focus $occ
    bind $ot <Enter> "focus $occ ; notIdle %W"
    bind $ot <Configure> {%W yview -pickplace end ; notIdle %W}
    bind $oFrm <Enter> "focus $occ ; notIdle %W"
    bind $oFrm <Destroy> "doLeave ${chan}"
    bind $oFrm <Visibility> {
	set win [winfo toplevel %W]
	global Icon
	if [info exists Icon($win)] {wm iconname $win $Icon($win)}
	global IconBM
	if [info exists IconBM($win)] {
	    wm iconbitmap $win [lindex $IconBM($win) 0]
	}
	notIdle %W
    }
    tkwait visibility $w
    global Open
    global startup
    if {$Open(${chan}) && [lsearch [getAuto ${chan}] join] >= 0} {
	wm iconify $win
    }
    if {$realChan} { sendIRC MODE ${chan} }
}
