proc capitalise str {
    return [string toupper [string index $str 0]][string range $str 1 end]
}
#
proc getOption {var dflt} {
    global $var
    set uVar [capitalise $var]
    if {[set $var [option get . $var $uVar]] == {}} { set $var $dflt }
}
#
proc normal win {
    return [expr {[lindex [$win conf -state] 4] == "normal"}]
}
#
proc zbind {chan sequence action} {
    if {$chan == {}} {
	global bindings ; lappend bindings [list $sequence $action]
    } {
	chanUpdate 1 ${chan} 8 [list $sequence $action]
    }
}
#
proc setBindings {ent chan} {
    global bindings
    foreach b $bindings {
	bind $ent [lindex $b 0] [lindex $b 1]
    }
    foreach b [getBindings ${chan}] {
	bind $ent [lindex $b 0] [lindex $b 1]
    }
}
#
# ON condition support code
#
proc on {action pattern code} {
    set pat {}
    foreach p $pattern {
	lappend pat [string tolower $p]
    }
    global OnCode
    lappend OnCode($action) [list $pat $code]
}

proc processOns {} {
    global OnCode
    global ons
    if {$ons != {}} {
	mkDialog {} .@ons {On Warning} \
	  {Your configuration file needs updating ("on" format).} {} \
	  {Update {forceSave}} {Cancel {}}

	foreach act $ons {
	    lappend OnCode([lindex $act 0]) [lrange $act 1 end]
	}
    } {
	set OnCode(XXXX) { }
	unset OnCode(XXXX)
    }
}

proc handleOn {action pattern} {
    global OnCode
    if [info exists OnCode($action)] {
	foreach act $OnCode($action) {
	    set re [lindex $act 0]
	    set i 0
	    set match 1
	    foreach pat $pattern {
		set up [lindex $re $i]
		if { $up != {} && ![regexp -nocase $up $pat]} {
		    set match 0
		    break
		}
		incr i
	    }
	    if $match {
		set i 0
		foreach pat $pattern {
		    global onPar${i}
		    set onPar${i} $pat
		    incr i
		}
		uplevel #0 [lindex $act 1]
		while {$i >= 0} { catch "unset onPar${i}" ; incr i -1}
		return
	    }
	}
    }
}
#
proc channelName	lst { return [lindex $lst 0] }
#
proc channelAuto	lst { return [lindex $lst 0] }
proc channelMsg		lst { return [lindex $lst 1] }
proc channelAutoOp	lst { return [lindex $lst 2] }
proc channelHistory	lst { return [lindex $lst 3] }
proc channelCloseTime	lst { return [lindex $lst 4] }
proc channelIcons	lst { return [lindex $lst 5] }
proc channelTopics	lst { return [lindex $lst 6] }
proc channelLog		lst { return [lindex $lst 7] }
proc channelBindings	lst { return [lindex $lst 8] }
proc channelPatterns	lst { return [lindex $lst 9] }
proc getInfo chan {
    global CInfo
    if [info exists CInfo(${chan})] {return $CInfo(${chan})} { return {}}
}
proc getAuto		chan { return [channelAuto  	[getInfo ${chan}]] }
proc getMsg		chan { return [channelMsg	[getInfo ${chan}]] }
proc getAutoOp		chan { return [channelAutoOp	[getInfo ${chan}]] }
proc getHistory		chan { return [channelHistory	[getInfo ${chan}]] }
proc getCloseTime	chan { return [channelCloseTime	[getInfo ${chan}]] }
proc getIcons		chan { return [channelIcons	[getInfo ${chan}]] }
proc getTopics		chan { return [channelTopics	[getInfo ${chan}]] }
proc getLog		chan { return [channelLog	[getInfo ${chan}]] }
proc getBindings	chan { return [channelBindings	[getInfo ${chan}]] }

proc InitGlobals {} {
    global env
    global user
    global host
    global argv
    global zircon
    if [file exist $zircon(lib)/zircon.ad] {
#	option readfile $zircon(lib)/zircon.ad startupFile
    }
    option add *Checkbutton.relief flat widgetDefault
    option add *Checkbutton.height 1 widgetDefault
    option add *Button.height 1 widgetDefault
    option add *Frame*borderWidth 2 widgetDefault
    option add *Scrollbar*relief raised widgetDefault
    option add *Listbox*relief raised widgetDefault
    option add *Entry*relief raised widgetDefault
    option add *Menubutton*relief raised widgetDefault
    option add *Menubutton*width 10 widgetDefault
    option add *Text*setGrid 1 widgetDefault
    option add *Text*wrap word widgetDefault
    option add *Text*relief raised widgetDefault
    option add *Canvas*relief raised widgetDefault
    getOption defaultPort 6667
    getOption defaultServer {}
    getOption smiley ":-)"
    getOption scowl ":-("
    getOption wink ";-)"
    getOption noPopup 0
    getOption popUp 0
    getOption popDown 0
    getOption noJump 0
    getOption noDraw 0
    getOption quiet 0
    getOption noRefresh 1
    getOption popInfo 0
    getOption closeInfo 1
    getOption history 50
    getOption closeTime 0
    getOption friendsOn 1
    getOption killPath 1
    getOption ons {}
    getOption bindings {}
    getOption nicks {}
    getOption ircnames {}
    getOption servers {}
    getOption services {}
    getOption userInfo {}
    set zircon(autos) {join open close menu nojump quiet}
    set zircon(idle) 0
    getOption channelInfo {}
    getOption actions {}
# control panel
    global aways ; set aways {}
    global namesTxt ; set namesTxt {}
    global namesChan ; set namesChan {}
    getOption invisible 0
    getOption wallops 0
    getOption srvmsg 0
    getOption showLocal 1
    getOption showPublic 1
    getOption showPrivate 0
    getOption listPattern {.*}
    getOption topicPattern {.*}
    getOption topicOnly 0
    getOption minMembers 3
    getOption monitor {}
    getOption monitorTime 60
    global monitorTime ; set monitorTime [expr {$monitorTime * 1000}]
# Debugging
    getOption DEBUG 0
    getOption monitorOut 0
    getOption monitorIn 0
    getOption notifyInterval 30
    global notifyInterval ; set notifyInterval [expr {$notifyInterval * 1000}]
# Messages
    getOption signoff {I run Zircon - so should you}
    getOption noConfirm {}
    getOption toInfo {}
    getOption noMessage {}
    getOption verboseCTCP 0
    getOption helpService "Help_UK"
    getOption ignores {}
    global trust
    set trust(eval) {}
    set trust(draw) {.+}
    set jarg 0
    set zarg 0
    set iarg {}
    set Narg {}
    set sarg {}
    set parg {}
    set opts {}
    foreach arg $argv {
	if {[string index $arg 0] == "-"} {
	    foreach bit [string range $arg 1 end] {
		case $bit {
		{j z} { set ${bit}arg 1 }
		{i N s p} { append opts $bit}
		default { puts stderr "Unknown option -$bit" }
		}
	    }
	} {
	    set opt [lindex $opts 0]
	    set $opts [lrange $opts 1 end]
	    case $opt {
	    {i n s p} { set ${opt}arg $arg }
	    default { }
	    }
	}
    }
#
# Source the system and then the user's rc file if they exist. The -z
# flag turns off reading rc files.
#
    if !$zarg {
	if [file exist $zircon(lib)/rc] {
	    if [catch "uplevel #0 source $zircon(lib)/rc" msg] {
		puts stderr "**** Error in the system rc file!!! - $msg"
		exit
	    }
	}
	if [file exist ~/.zirconrc] {
	    if [catch "uplevel #0 source ~/.zirconrc" msg] {
		puts stderr "**** Error in your .zirconrc file!!! - $msg"
		exit
	    }
	}
    }

    global closeTime ; set closeTime [expr {$closeTime * 1000}]
    set host \
      [expr {[info exists env(HOSTNAME)] ? $env(HOSTNAME) : [exec hostname]}]
    global hostIPAddress
    set a1 [dp_address create $host 0]
    set hostIPAddress [lindex [dp_address info $a1] 0]
    dp_address delete $a1

    set user [expr {[info exists env(USER)] ? $env(USER) : [exec whoami]}]
    envCheck $Narg IRCNICK nicks $user
    envCheck $iarg IRCNAME ircnames $user@$host
    global servers
    if {$sarg != {} || [info exists env(IRCSERVER)]} {
	set v [expr {$sarg != {} ? $sarg : $env(IRCSERVER)}]
	if {[set x [listmatch $servers $v]] > 0} { listdel servers $x }
	if {$x != 0} { set servers [linsert $servers 0 [list $v]] }
    } {
	if {$servers == {}} {
	    global defaultServer
	    set servers $defaultServer
	}
    }
    if {$parg != {} || [info exists env(IRCPORT)]} {
	set v [expr {$parg != {} ? $parg : $env(IRCPORT)}]
	if {[set srv [lindex $servers 0]] != {} && [lindex $srv 1] != $v} {
	    set servers [linsert servers 0 [list [lindex $srv 0] $v]
	}
    }
    global activeChannels
    set activeChannels {}
    global channelInfo
    global CInfo
    global Ptns
    foreach chn $channelInfo {
	set cn [lindex $chn 0]
	set CInfo($cn) [lrange $chn 1 end]
	set Ptns($cn) [channelPatterns $CInfo($cn)]
	if {!$jarg && [lsearch [getAuto $cn] join] >= 0} {
	    lappend activeChannels $cn
	}
    }
#
# Re-Initialise things in case they were set in the rc file....
#
    set zircon(version) "1.14.R2"
#
# Configuration panel stuff
#
    global cVars
    set cVars(IRC) {nicks ircnames servers}
    set cVars(People) {userInfo ignores}
    set cVars(Channels) {channelInfo history closeTime noMessage popUp \
      popDown noDraw noJump quiet}
    set cVars(Info) {showLocal showPublic showPrivate topicOnly minMembers \
      signoff noConfirm toInfo popInfo closeInfo verboseCTCP helpService \
	noPopup noRefresh friendsOn killPath listPattern topicPattern}
    set cVars(Others) {aways actions invisible wallops srvmsg services}
#    set cVars(On) {ons}
#    set cVars(Bindings) {bindings}
    global confData
    set confData(single) {history closeTime showLocal showPublic showPrivate \
      topicOnly minMembers signoff popInfo closeInfo verboseCTCP helpService \
      invisible wallops srvmsg noRefresh noPopup friendsOn killPath \
      popUp popDown noDraw noJump quiet listPattern topicPattern}
    set confData(msg) {Join Kick Kill Leave Mode Quit Topic}
    set confData(info) {Ctcp Signoff Who Whois Whowas Error Ison Info}
    set confData(nconf) {Quit Leave Kill SaveConf}
#
# Array variables
#
    global Log ; set Log(@info) 1 ; unset Log(@info)
    global ISON ; set ISON(@me) 0 ; unset ISON(@me)
    global popInfo ; global Open ; set Open(@info) $popInfo
    global closeInfo ; global Close ; set Close(@info) $closeInfo
    global Split ; set Split(0) {} ; unset Split(0)
    global Limbo ; set Limbo(@me) {} ; unset Limbo(@me)
    global TSplit ; set TSplit(0) {} ; unset TSplit(0)
    global Heal ; set Heal(0) {} ; unset Heal(0)
    global Name ; set Name(.@info) @info
    global Jump ; set Jump(@info) 0
    global Draw ; set Draw(@info) 0
    global Quiet ; set Quiet(@info) 1
    global Active ; set Active(@info) 1
    global CInfo ; set CInfo(@info) 1 ; unset CInfo(@info)
    global Ptns ; set Ptns(@info) {}
    global MkOp ; set MkOp(@info) {} ; unset MkOp(@info)
    global TFg ; set TFg(@me) 1 ; unset TFg(@me)
    global TBg ; set TBg(@me) 1 ; unset TBg(@me)
    global TAF ; set TAF(@me) 1 ; unset TAF(@me)
    global TAB ; set TAB(@me) 1 ; unset TAB(@me)
    global history ; global History ; set History(@info) $history
    global CloseTime ; set CloseTime(@info) $closeTime
    global CloseCount ; set CloseCount(@info) $closeTime
    global Secure
    set Secure(nickserv) nickserv@service.de
    set Secure(noteserv) noteserv@service.de
#
#
#
    global away ; set away 0
    global busy ; set busy 0
    global sock ; set sock {}
    global startup ; set startup 1
    global allChannels ; set allChannels {}
    global confChange ; set confChange 0
#
#	1 if user is an IRC operator
#
    global ircop ; set ircop 0
#
# Some command information
#
    global Ops
    set Ops(server) {Links Version Time Trace Admin Lusers Info Stats \
      Oper Connect Rehash Restart}
    set Ops(ircSrv) {Connect Rehash Restart}
    set Ops(user) {Who Whois Whowas Mode CTCP DCC Invite Msg Notice \
      Finger Time Trace Userhost Kill}
    set Ops(userMenu) {Whois Msg Notice Time CTCP DCC Notify Ignore
      Finger Speak ChanOp Kick Ban+Kick Kill}
    set Ops(chanop) {Speak ChanOp Kick Ban+Kick}
    set Ops(ircop) {Kill}
    set zircon(ignore) {Notices Public Invites Wallops Notes CTCP Others}
#
    global notify ; set notify {} ; setupUsers
    processOns
}
#
proc notIdle win {
    if {$win != {}} {
	set win [winfo toplevel $win]
	global Name
	if [info exists Name($win)] {
	    global Active
	    set Active($Name($win)) 1
	}
    }
    global zircon ; set zircon(idle) 0
}
#
proc zpack {w sw pars} {foreach b $sw { pack append $w $w.$b $pars }}
#
# List utilities
#
proc listmatch {list val} {
    set i 0
    foreach item $list {
	if {[lindex $item 0] == $val} { return $i }
	incr i
    }
    return -1
}

proc listdel {v item} {
    set cmd \
      "if {\[llength \$$v\] > $item} { set $v \[lreplace \$$v $item $item\] }"
    uplevel 1 $cmd
}

proc listupdate {list item val} {
    uplevel 1 "while { \[llength \$${list}\] <= $item} {lappend ${list} {} }"
    uplevel 1 "set $list \[lreplace \$$list $item $item {$val} \]"
}

proc listmove {list from to val} {
    set cmd "set $list \[linsert \[lreplace \$$list $from $from\] $to $val \]"
    uplevel 1 $cmd
}
#
# proc to determine if a message is wanted
#
proc wantMessage {msg chan} {
    global noMessage
    set cm [getMsg $chan]
    return [expr {[lsearch $cm "!$msg"] >= 0 || ([lsearch $cm $msg] < 0 &&  \
      [lsearch $noMessage $msg] < 0)}]
}
#
proc doScroll {win where} { $win yview $where }
#
proc setScroll {txt win total window first last} {
#    global Name
#    global History
#    set chan $Name([winfo toplevel $win])
#    if {$total > $History(${chan})} {
#	incr total -$History($chan)
#	$txt delete 1.0 "1.0 + $total lines"
#	set total $History(${chan})
#    }
    $win set $total $window $first $last
}
#
# Proc to truncate window history
#
proc chopText {chan txt} {
    global History
    set lng [lindex [split [$txt index end] .] 0]
    if {$lng > $History($chan)} {
	incr lng -$History($chan)
	$txt delete 1.0 "1.0 + $lng lines"
    }
}
#
# alter menu item states - used for oping and ircoping
#
proc setState {name pick state} {
    global Ops
    foreach cmd $Ops($pick) {
	$name entryconfigure [$name index $cmd] -state $state
    }
}
#
# Make an entry with some emacs-like edit keys......
#
proc emacsInsertSelect {ent} {
    if {[normal $ent] && ![catch {selection get} bf] && $bf != {}} {
	$ent insert insert $bf
	tk_entrySeeCaret $ent
    }
}

proc emacsTInsertSelect {ent} {
    if {[normal $ent] && ![catch {selection get} bf] && $bf != {}} {
	$ent insert insert $bf	
	$ent yview insert
    }
}

proc emacsEntry {args} {
    set name [eval entry $args]
    bind $name <Control-a> { notIdle %W ; %W icursor 0 }
    bind $name <Control-b> {
	notIdle %W
	%W icursor [expr {[%W index insert] - 1}]
    }
    bind $name <Control-d> { notIdle %W ; %W delete insert }
    bind $name <Control-e> { notIdle %W ; %W icursor end }
    bind $name <Control-f> {
	notIdle %W
	%W icursor [expr {[%W index insert] + 1}]
    }
    bind $name <Control-k> { notIdle %W ; %W delete insert end }
    bind $name <Control-u> { notIdle %W ; %W delete 0 end }
    bind $name <ButtonPress-2> {notIdle %W ; emacsInsertSelect %W}
    bind $name <Delete> \
      {notIdle %W ; tk_entryBackspace %W; tk_entrySeeCaret %W}
    bind $name <BackSpace> \
      {notIdle %W ; tk_entryBackspace %W; tk_entrySeeCaret %W}
    bind $name <Control-h> \
      {notIdle %W ; tk_entryBackspace %W; tk_entrySeeCaret %W}
    bind $name <Meta-b> \
      { notIdle %W ; %W insert insert \002 ;tk_entrySeeCaret %W }
    bind $name <Meta-o> \
      { notIdle %W ; %W insert insert \017 ;tk_entrySeeCaret %W }
    bind $name <Meta-u> \
      { notIdle %W ; %W insert insert \037 ; tk_entrySeeCaret %W }
    bind $name <Meta-v> \
      { notIdle %W ; %W insert insert \026 ; tk_entrySeeCaret %W }
    return $name
}

proc emacsTEntry {args} {
    set name [eval text $args -height 1 -wrap none -setgrid 0]
    bind $name <Control-a> { notIdle %W ; %W mark set insert 1.0 }
    bind $name <Control-e> { notIdle %W ; %W mark set insert end }
    bind $name <Control-k> { notIdle %W ; %W delete insert end }
    bind $name <Control-d> { notIdle %W ; %W delete insert }
    bind $name <ButtonPress-2> {notIdle %W ; emacsTInsertSelect %W}
    bind $name <Control-u> { notIdle %W ; %W delete 1.0 end }
    bind $name <Control-v> { notIdle %W ; %W insert insert %A }
    bind $name <Meta-b> { notIdle %W ; %W insert insert \002 }
    bind $name <Meta-o> { notIdle %W ; %W insert insert \017 }
    bind $name <Meta-u> { notIdle %W ; %W insert insert \037 }
    bind $name <Meta-v> { notIdle %W ; %W insert insert \026 }
    return $name
}

proc entrySet {win val} { $win delete 0 end ; $win insert end $val }

proc labelEntry {t name opts init code} {
    frame $name
    eval label $name.label $opts
    [expr {$t ? "emacsTEntry" : "emacsEntry"}] $name.entry -relief raised
    $name.entry insert end $init
    zpack $name label {left}
    zpack $name entry {left expand fillx}
    bind $name.entry <Return> "notIdle %W ; $code"
}
#
# Procedure used to shorten menu labels to 10 characters. Used
# when adding user provided items to menus
#
proc prune name {
    regsub -all "\[\002\017\026\037\]" $name {} name
    return [expr {[string length $name] > 10 ? \
      "[string range $name 0 7]..." : $name}]
}
#
# Proc to search the servers list for a server and return its entry or {}
#
proc serverData srv {
    global servers
    return [expr \
	{[set x [listmatch $servers $srv]] < 0 ? {} : [lindex $servers $x]}]
}

proc setIcon {win chan title} {
    global Icon ; wm iconname $win $title ; set Icon($win) $title
    if {[set icn [getIcons $chan]] != {} } {
	global IconBM ; set IconBM($win) $icn
	wm iconbitmap $win [lindex $icn 0]
    }
}

proc killWindow win {
    global Icon ; catch "unset Icon($win)"
    global IconBM ; catch "unset IconBM($win)"
    catch "destroy $win"
}
#
proc makeMB {win text} {
    menubutton $win -text $text -menu $win.menu
    return [menu $win.menu]
}
#
#	Build the Zircon Information Window.
#
proc makeInfo {} {
    global server
    toplevel .@info -relief raised -borderwidth 2 -class Zircon
    global Icon
    wm title .@info {Zircon Information Window}
    wm iconname .@info [set Icon(.@info) {Zircon Info}]
    wm minsize .@info 10 1

    set fr [frame .@info.oFrm]
    frame $fr.textFrm -relief raised
    scrollbar $fr.textFrm.vscroller -command "doScroll $fr.textFrm.text"
    set oft [text $fr.textFrm.text -height 10 -width 80 \
      -yscrollcommand "setScroll $fr.textFrm.text $fr.textFrm.vscroller" ]
    rebind $oft
    bind $oft <Configure> {%W yview -pickplace end ; notIdle %W}
    bind .oFrm <Visibility> {notIdle %W}
    zpack $fr.textFrm text {left expand fill}
    zpack $fr.textFrm vscroller {left filly} 

    zpack $fr textFrm {expand fill}
    zpack .@info oFrm {expand fill}
    tkwait visibility .@info
    global BF
    set BF(@info) [getOValue $oft font boldFont Font]
    global Fg 
    set Fg(@info) [getOValue $oft foreground foreground Foreground]
    global Bg 
    set Bg(@info) [getOValue $oft background background Background]
    global Ft
    set Ft(@info) [getOValue $oft font font Font]
}

proc keepAway value {
    doAway "$value"
    .oFrm.bf1.away.menu add command -label "[prune $value]" \
      -command "doAway {$value}"
    global aways
    lappend aways $value
    global confChange ; set confChange 1
}

proc getAway {} {
    mkEntryBox .@away {Away Message} {Enter your away message:} \
      {{Away {}}} \
      {OK doAway} {Keep keepAway} {Back {doAway {}}} {Cancel {}}
}

proc keepAction {chan value} {
    sendAction ${chan} "$value"
    .${chan}.oFrm.cmds.cmds0.action.menu add command -label "[prune $value]" \
      -command "sendAction ${chan} {$value}"
    global actions ; lappend actions $value
    global confChange ; set confChange 1
}

proc getAction {chan} {
    mkEntryBox .${chan}.action "Action" "Enter your action:" \
      {{Action {}}}\
      "OK {sendAction ${chan}}" "Keep {keepAction ${chan}}" {Cancel {}}
}

proc getPrev {chan} {
    global HPos
    global HBuff
    if {[set line [lindex $HBuff($chan) $HPos($chan)]] == {}} {
	set HPos($chan) 0
	set line [lindex $HBuff($chan) 0]
    } {
	incr HPos($chan)
    }
    return $line
}

proc getNext {chan} {
    global HPos
    global HBuff
    if {[set line [lindex $HBuff($chan) $HPos($chan)]] == {}} {
	set HPos($chan) 0
	set line [lindex $HBuff($chan) 0]
    } {
	incr HPos($chan) -1
    }
    return $line
}

proc addToHist {chan txt} {
    global HPos ; set HPos($chan) 0
    if {$txt != {}} {
	global HBuff ; set HBuff($chan) [linsert $HBuff($chan) 0 $txt]
	set HBuff($chan) [lrange $HBuff($chan) 0 9]
    }
    return $txt
}

proc getOValue {win opt lc uc} {
    if {[set x [option get $win $lc $uc]] == {}} {
	set x [lindex [$win conf -$opt] 4]
    }
    return $x
}

proc flipActions chan {
    set win .${chan}.oFrm.cmdLine.commandLine
    set ret [bind $win <Return>]
    set sret [bind $win <Shift-Return>]
    bind $win <Return> $sret
    bind $win <Shift-Return> $ret
}

proc logOpen {chan mode file} {
    if {$file == {}} return
    global Log
    if [catch "open $file $mode" Log(${chan})] {
	addText ERROR @info \
	  "*** Cannot open log file for channel ${chan} : $Log(${chan})"
	unset Log(${chan})
	return
    }
    global LogFile
    set LogFile(${chan}) $file
    set w .${chan}.oFrm.cmds.cmds0.channel.menu.log
    $w entryconfigure 0 -state normal
    $w entryconfigure 1 -state disabled
    $w entryconfigure 2 -state normal
    $w entryconfigure 3 -state normal
}

proc doLog {chan op} {
    global Log
    global LogFile
    set w .${chan}.oFrm.cmds.cmds0.channel.menu.log
    case $op {
    Close {
	    if [info exists Log(${chan})] {
		close $Log(${chan})
		unset Log(${chan})
	    }
	    $w entryconfigure 0 -state disabled
	    $w entryconfigure 1 -state normal
	    $w entryconfigure 2 -state disabled
	    $w entryconfigure 3 -state disabled
	}
    Empty {
	    if [info exists $Log(${chan})] {
		close $Log(${chan})
		set Log(${chan}) [open $LogFile(${chan}) w]
	    }
	}
    Flush { if [info exists Log(${chan})] { flush $Log(${chan}) } }
    Open {
	    set fl \
	      [expr {$LogFile(${chan}) != {} ? $LogFile(${chan}) : "${chan}.log"}]
	    mkFileBox .@log${chan} "Log ${chan}" {}\
	      "Log file for channel ${chan}:" \
	      "Append {logOpen ${chan} a}"\
	      "Truncate {logOpen ${chan} w}" {Cancel {}}
	}
    }
}
proc doHelp {topic service} {
    if {$service != {}} {sendIRC PRIVMSG $service $topic}
}

proc getHelp {} {
    global helpService
    mkEntryBox .@help "Help" "Enter topic on which you need help:" \
      "{Topic {zircon ?}} {Service $helpService}" \
      {OK {doHelp}} {Cancel {}}
}

proc insertSelect {chan ent} {
    if {![catch {selection get} bf] && $bf != {}} {
	while {[set nl [string first "\012" $bf]] >= 0} {
	    $ent insert insert [string range $bf 0 [incr nl -1]]
	    tk_entrySeeCaret $ent
	    sendToChannel $chan [$ent get]
	    $ent delete 0 end
	    set bf [string range $bf [incr nl 2] end]
	}
	if {$bf != {}} { $ent insert insert $bf ; tk_entrySeeCaret $ent }
    }
}
#
proc setFlag flag {
    global $flag
    global nickname
    setMode ${nickname} [expr {[set $flag] ? "+" : "-"}][string index $flag 0]
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
    foreach nn $lst {
	$mn add command -label "$nn" -command "change${lbl} {$nn}"
    }
    emacsEntry $name.entry
    bind $name.entry <Return> "change${lbl} \[%W get\]"

    zpack $name label {left}
    zpack $name entry {left expand fillx} 
    $name.entry insert end [lindex $lst 0]
    global $var ; set $var [lindex $lst 0]
}

proc sendService {sv op par} {
    sendIRC PRIVMSG $sv "$op $par"
}

proc doService {sv op} {
    mkEntryBox .@sv $sv "Enter any parameters needed for $sv:" \
	[list [list $op {}]] "OK {sendService $sv $op }" {Cancel {}}
}

proc rebind txt {
    bind $txt <Control-d> { }
    bind $txt <Return> { }
    bind $txt <Control-v> { }
    bind $txt <Control-h> { }
    bind $txt <Delete> { }
    bind $txt <BackSpace> { }
    bind $txt <Any-KeyPress> { }
    bind $txt <1> {
	set tk_priv(selectMode) char
	%W mark set insert @%x,%y
	%W mark set anchor insert
	tk_textSelectTo %W @%x,%y
    }
    bind $txt <Double-1> { }
    bind $txt <Triple-1> { }
    bind $txt <B1-Motion> {tk_textSelectTo %W @%x,%y}
    bind $txt <Shift-1> {
	tk_textResetAnchor %W @%x,%y
	tk_textSelectTo %W @%x,%y
    }
    bind $txt <Shift-B1-Motion> {tk_textSelectTo %W @%x,%y}
    bind $txt <2> {%W scan mark %y}
    bind $txt <B2-Motion> {%W scan dragto %y}
}
#
#	Build the Zircon control window
#
proc flagControl state {
    foreach w {helpFrm.help nSFrm.cr.invis nSFrm.cr.wallop nSFrm.cr.srvmsg 
	       bf2.servers bf2.users bf2.channels bf2.services
	       bf1.away bf1.brb bf1.friends cmdLine.channel
	      } {
	.oFrm.$w conf -state $state
    }
}

proc busyFlag {} {
    global busy
    if $busy {
    } {
    }
}

proc makeControl {} {
    wm title . {Zircon Control Panel}
    wm iconname . {Zircon Control}
    set oFrm .oFrm
    frame $oFrm -relief raised

    global zircon
    frame $oFrm.helpFrm
    set om [makeMB $oFrm.helpFrm.conf Configure]
    foreach nn {IRC People Channels Info} {
	$om add command -label $nn -command conf${nn}
    }
    button $oFrm.helpFrm.version -relief raised -command credits \
      -text "Zircon V$zircon(version)"

    button $oFrm.helpFrm.help -relief raised -text Help -command getHelp

    zpack $oFrm.helpFrm {conf version help} {left expand}

    frame $oFrm.debug
    frame $oFrm.debug.mb
    checkbutton $oFrm.debug.mb.mo -text {Monitor Out} -variable monitorOut
    checkbutton $oFrm.debug.mb.mi -text {Monitor In} -variable monitorIn
    zpack $oFrm.debug.mb {mo mi} {left expand fillx}
    frame $oFrm.debug.tcl
    emacsEntry $oFrm.debug.tcl.entry
    zpack $oFrm.debug.tcl entry {expand fillx}
    zpack $oFrm.debug {mb tcl} {expand fillx}
    bind $oFrm.debug.tcl.entry <Return> { uplevel #0 [%W get] }

    set oc [frame [frame $oFrm.nSFrm].cr]
    checkbutton $oc.busy -text Busy -command busyFlag \
      -variable busy
    checkbutton $oc.invis -text Invisible -command {setFlag invisible} \
      -variable invisible
    checkbutton $oc.wallop -text Wallop -command {setFlag wallops} \
      -variable wallops
    checkbutton $oc.srvmsg -text SrvMsg -command {setFlag srvmsg} \
      -variable srvmsg
    global ircop
    checkbutton $oc.ircop -text {IRC Op} -command deIRCOp -variable ircop \
      -state disabled
    zpack $oc {busy invis wallop srvmsg ircop} {left}
    zpack $oFrm.nSFrm cr {expand}

    global nicks
    NNSBuild Nickname nickname $nicks
    global nickname
    global lcNickname
    set lcNickname [string tolower $nickname]
    global ircnames
    NNSBuild IRCName ircname $ircnames

    frame $oFrm.nSFrm.server
    makeMB $oFrm.nSFrm.server.label Server
    global server
    global servers
    global defaultPort
    global ircport
    set srv [lindex $servers 0]
    set server [lindex $srv 0]
    if {[set ircport [lindex $srv 1]] == {}} { set ircport $defaultPort}
    foreach nn $servers {
	set sn [lindex $nn 0]
	if {[set p [lindex $nn 1]] == {}} { set p $defaultPort}
	$oFrm.nSFrm.server.label.menu add command \
	  -label $sn -command "changeServer $sn $p"
    }

    emacsEntry $oFrm.nSFrm.server.entry

    bind $oFrm.nSFrm.server.entry <Return> { \
	changeServer [%W get] $defaultPort
    }

    bind $oFrm.nSFrm.server.entry <Escape> { \
	set h [%W get]
	mkEntryBox .@port "Port Number" "Enter port number for $h:" \
	  "{Port $defaultPort}" "OK {changeServer $h}" {Cancel {}}
    }

    zpack $oFrm.nSFrm.server label {left}
    zpack $oFrm.nSFrm.server entry {left expand fillx} 
    zpack $oFrm.nSFrm server {expand fillx}
    $oFrm.nSFrm.server.entry insert end "$server"

    frame $oFrm.bf2
    makeMB $oFrm.bf2.servers Servers 
    global Ops
    foreach cmd $Ops(server) {
	$oFrm.bf2.servers.menu add command -label $cmd \
	  -command "serverCmd ${cmd}"
    }

    if !$ircop { setState $oFrm.bf2.servers.menu ircSrv disabled }

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
    if !$ircop { setState $oFrm.bf2.users.menu ircop disabled }

    makeMB $oFrm.bf2.channels Channels
    foreach cmd "Join Who List Names Notice Monitor" {
	$oFrm.bf2.channels.menu add command -label $cmd \
	  -command "channel${cmd} \[.oFrm.cmdLine.channel get\]"
    }

    addCTCPMenu $oFrm.bf2.channels.menu \
      {[.oFrm.cmdLine.channel get]}

    $oFrm.bf2.channels.menu add separator

    global CInfo
    foreach chan [array names CInfo] {
	if {[lsearch [getAuto $chan] menu] >= 0} {
	    $oFrm.bf2.channels.menu add command -label $chan \
	      -command "channelJoin $chan"
	}
    }
    makeMB $oFrm.bf2.services Services
    global services
    $oFrm.bf2.services.menu add cascade -label nickserv \
      -menu $oFrm.bf2.services.menu.nickserv
    set m [menu $oFrm.bf2.services.menu.nickserv]
    foreach chn {ACCESS BUG HELP IDENTIFY INFO INTRO LIST NEWS OPERWHO \
      PASSWD REGISTER STATS TIMEZONE UNREG WHOIS} {
	$m add command -label $chn \
	  -command "doService nickserv@service.de $chn"
    }

    $oFrm.bf2.services.menu add cascade -label noteserv \
      -menu $oFrm.bf2.services.menu.noteserv
    set m [menu $oFrm.bf2.services.menu.noteserv]
    foreach chn {AWAY DELETE IDENTIFY INFO INTRO LIST MSG QUERY READ WAITFOR} {
	$m add command -label $chn \
	  -command "doService noteserv@service.de $chn"
    }

    foreach chn $services {
	set nm [lindex $chn 0]
	set lnm [string tolower $nm]
	if {[set ad [lindex $chn 1]] == {}} {
	    set ad $nm
	} {
	    global Secure
	    set Secure($lnm) ${lnm}@${ad}
	    set ad ${lnm}@${ad}
	}
	$oFrm.bf2.services.menu add cascade -label $nm \
	  -menu $oFrm.bf2.services.menu.$lnm
	set m [menu $oFrm.bf2.services.menu.$lnm]
	foreach nn [lindex $chn 2] {
	    $m add command -label $nn \
	      -command "doService $ad $nn"
	}	
    }

    tk_menuBar $oFrm.bf2 $oFrm.bf2.servers $oFrm.bf2.services\
      $oFrm.bf2.users $oFrm.bf2.channels

    frame $oFrm.bf1
    makeMB $oFrm.bf1.away Away
    $oFrm.bf1.away.menu add command -label Back -command "doAway {}"
    $oFrm.bf1.away.menu add command -label New -command "getAway"
    $oFrm.bf1.away.menu add separator

    global aways
    foreach act $aways {
	$oFrm.bf1.away.menu add command \
	  -label "[prune $act]" -command "doAway {$act}"
    }

    button $oFrm.bf1.brb -command doBRB -width 10 -text BRB
    button $oFrm.bf1.friends -command makeFriends -width 10 -text Friends
    button $oFrm.bf1.quit -command quitZircon -width 10 -text Quit

    zpack $oFrm.bf1 {away brb friends quit} {left expand fillx}
    zpack $oFrm.bf2 {servers users channels services} {left expand fillx}

    frame $oFrm.cmdLine -relief raised
    label $oFrm.cmdLine.label -relief raised -text { Channel }
    emacsEntry $oFrm.cmdLine.channel
    zpack $oFrm.cmdLine label {left}
    zpack $oFrm.cmdLine channel {left expand fillx}
    global DEBUG
    if $DEBUG { zpack $oFrm debug {fillx} }
    zpack $oFrm {helpFrm nSFrm bf1 bf2} {fillx}
    zpack $oFrm cmdLine {fillx}

    pack append . $oFrm {expand fill} 

    bind $oFrm.cmdLine.channel <Return> { channelJoin [%W get] {}}

    flagControl disabled
    tkwait visibility .
}

proc pickvar {val v1 v2} { return [expr {$val != {} ? $v1 : $v2}] }

proc setTags {chan nk} {
    global TFn
    global TFa
    global TFg
    global TBg
    global TAF
    global TAB
    set ch .${chan}.oFrm.textFrm.text
    set fn [getOValue $ch font ${nk}Font Font]
    set fg [getOValue $ch foreground ${nk}Foreground Foreground]
    set bg [getOValue $ch background ${nk}Background Background]
    set fa [getOValue $ch font ${nk}ActionFont Font]
    set af [getOValue $ch foreground ${nk}ActionForeground Foreground]
    set ab [getOValue $ch background ${nk}ActionBackground Background]
    set indx "$chan, $nk"
    set TFn($indx) $fn
    set TFa($indx) $fa
    set TFg($indx) $fg
    set TBg($indx) $bg
    set TAF($indx) $af
    set TAB($indx) $ab
}

proc doAddText {tagInfo name chan text args} {
    global Active ; set Active($chan) 1
    global Log
    if [info exists Log(${chan})] {
	puts $Log(${chan}) $text
    }
    set tag [lindex $tagInfo 0]
    set start [$name index end]
    set pattern *\[\002\026\037\007\]*
    if ![string match $pattern $text] {
	$name insert end $text
	set end [$name index end]
	$name insert end " \n"
    } {
	global BF
	global Fg
	global Bg
	set fg [expr {$tag != {} ? [lindex $tagInfo 1] : $Fg($chan)}]
	set bg [expr {$tag != {} ? [lindex $tagInfo 2] : $Bg($chan)}]
	set bold {}
	set boldEnd {}
	set uline {}
	set ulEnd {}
	set invert {}
	set invEnd {}
	set ends {}
	foreach ch [split $text {}] {
	    case $ch {
	    \002 {
		    set [pickvar $bold boldEnd bold] [$name index end]
		}
	    \017 {
		    if {$bold != {}} { set boldEnd [$name index end] }
		    if {$invert != {}} { set invEnd [$name index end] }
		    if {$uline != {}} { set ulEnd [$name index end] }
		}
	    \026 {
		    set [pickvar $invert invEnd  invert] [$name index end]
		}
	    \037 {
		    set [pickvar $uline ulEnd uline] [$name index end]
		}
	    \007 {
		    global Quiet
		    if !$Quiet($chan) {
			puts -nonewline stdout "\007" ; flush stdout
		    }
		    $name insert end " "
		    set bp [$name index end]
		    $name insert end BEEP
		    set bpe [$name index end]
		    $name insert end " "
		    $name tag add @beep${tag} $bp $bpe
		    $name tag configure @beep${tag} -background $bg -borderwidth 2 \
		      -relief raised
		}
	    default {
		    $name insert end "$ch"
		    if {$boldEnd != {}} {
			if {$BF($chan) != {}} {
			    $name tag add @b${tag} $bold $boldEnd
			    $name tag configure @b${tag} -font $BF($chan)
			}
			set bold {}
			set boldEnd {}
		    }
		    if {$ulEnd != {}} {
			$name tag add @u${tag} $uline $ulEnd
			$name tag configure @u${tag} -underline 1
			set uline {}
			set ulEnd {}
		    }
		    if {$invEnd != {}} {
			$name tag add @v${tag} $invert $invEnd
			$name tag configure @v${tag} -foreground $bg -background $fg
			set invert {}
			set invEnd {}
		    }
		}
	    }
	}
	set end [$name index end]
	$name insert end " \n"
	if {$bold != {}} {
	    if {$BF($chan) != {}} {
		$name tag add @b${tag} $bold $end
		$name tag configure @b${tag} -font $BF($chan)
	    }
	}
	if {$uline != {}} {
	    $name tag add @u${tag} $uline $end
	    $name tag configure @u${tag} -underline 1
	}
	if {$invert != {}} {
	    $name tag add @v${tag} $invert $end
	    $name tag configure @v${tag} -foreground $bg -background $fg
	}
    }
    if {$tag != {}} {
	$name tag add $tag $start $end
	$name tag lower $tag sel
	global Fg
	global Bg
	set fg [lindex $tagInfo 1]
	set bg [lindex $tagInfo 2]
	if {$fg != $Fg($chan)} { $name tag configure $tag -foreground $fg }
	if {$bg != $Bg($chan)} { $name tag configure $tag -background $bg }
	$name tag configure $tag -font [lindex $tagInfo 3]
	if ![string match {@*} $tag] {
	    $name tag bind $tag <Double-Button-1> "makeChannel $tag M"
	    $name tag bind $tag <Shift-Double-Button-1> "sendIRC WHOIS $tag $tag"
	    $name tag bind $tag <Control-Double-Button-1> "finger $tag"
	}
    }
    chopText ${chan} $name
    global Jump
    if {$args == {{}}  && !$Jump(${chan})} { $name yview -pickplace end }
}

proc doPopUp chan {
    global Open
    if {$Open(${chan}) && ![winfo ismapped .${chan}]} {
	global noPopup;
	if $noPopup {
	    global Icon ; wm iconname .${chan} "*$Icon(.${chan})*"
	    global IconBM
	    if {[info exists IconBM(.${chan})] && \
		[set icn [lindex $IconBM(.${chan}) 1]] != {}} {
		wm iconbitmap .${chan} $icn
	    }
	} {
	    wm deiconify .${chan}
	}
    }
}

proc tagInfo {chan tag} {
    set tag [string tolower $tag]
    set indx "$chan, $tag"
    global TFn
    global TFg
    global TBg
    if {$tag != {}} {
	if ![info exists TFn($indx)] { setTags $chan $tag }
	return [list $tag $TFg($indx) $TBg($indx) $TFn($indx)] 
    }
    return {};
}

proc addText {tag chan text args} {
    set chan [string tolower ${chan}]
    doPopUp ${chan}
    doAddText [tagInfo $chan $tag] .${chan}.oFrm.textFrm.text $chan \
      $text $args
}

proc noPopAdd {tag chan text args} {
    set chan [string tolower ${chan}]
    doAddText [tagInfo $chan $tag] .${chan}.oFrm.textFrm.text $chan \
      $text $args
}

proc optText {name chan string} {
    if {[wantMessage $name $chan]} { addText @$name ${chan} $string }
}

proc operator chan {
    global ${chan}Op ; global lcNickname ; return [set ${chan}Op($lcNickname)]
}
#
#	proc me : Returns true if nk is this user
#		  Assumes that nk is in lower case!!!
#
proc me nk { global lcNickname ; return [expr {$nk == $lcNickname}] }
#
#	proc active : returns true if name is an active channeln
#
proc active chan { return [winfo exists .${chan}] }
#
#	proc friend : returns true if name is a friend
#
proc friend who {
    global userInfo
    set lwho [string tolower $who]
    foreach w $userInfo {
	if {[string tolower [lindex $w 0]] == $lwho} { return 1 }
    }
    return 0
}

proc ignore {pattern args} {
    global ignores
    set lst [list $pattern $args]
    if {[set x [listmatch $ignores $pattern]] >= 0} {
	listupdate ignores $x $lst
    } {
	lappend ignores $lst
    }
}
#
# Look and see if there are any ignores for this nick/name. List has format:
#	{{pattern {list of what} ......}
#
proc z_ignore {nk nm} {
    global ignores
    foreach ig $ignores {
	if [string match [lindex $ig 0] $nk!$nm] { return [lindex $ig 1] }
    }
    return {}
}

proc ignoreSet {lst what} {
    return [expr {[lsearch $lst $what] >= 0}]
}

proc ignoreFlag {lst what} {
    set nk [lindex $lst 0]
    set v [lindex $lst 1]
    if {[set x [lsearch $v $what]] < 0} {
	return [list $nk [lappend v $what]]
    } {
	return [list $nk [listdel v $x]]
    }
}

proc flipIgnore {nk what} {
    global ignores
    global IFlag
    if {[set x [listmatch $ignores ${nk}!*@*]] < 0} {
	lappend ignores [ignoreFlag [list $nk {}] $what]
    } {
	listupdate ignores $x [ignoreFlag [lindex $ignores $x] $what]
    }
    global confChange ; set confChange 1
}

proc addIgnoreMenu {win nk} {
    $win add cascade -label Ignore -menu $win.ignore
    menu $win.ignore
    global zircon
    global IFlag
    set lst [z_ignore $nk *@*]
    foreach x $zircon(ignore) {
	set lx [string tolower $x]
	$win.ignore add checkbutton -label $x -variable IFlag(${nk},${lx}) \
	  -command "flipIgnore $nk $lx"
	set IFlag(${nk},${lx}) [ignoreSet $lst $lx]
    }
}
#
#	proc channel : Returns true if name is a channel name
#
proc channel {name} { return [string match {[#&]*} $name] }
#
#	proc mungPrefix : breaks up the prefix to an IRC message
#	returns : {nick, me?, lowercase nick, name, ignores}
#
proc mungPrefix {prefix} {
    if {[set pos [string first ! $prefix]] >= 0} {
	set nk [string range $prefix 1 [incr pos -1]]
	set nm [string range $prefix [expr {$pos + 2}] end]
	set lnm [string tolower $nm]
    } {
	set nk [string range $prefix 1 end]
	set lnm [set nm {}]
    }
    set lnk [string tolower $nk]
    return [list $nk [me $lnk] $lnk $nm [z_ignore $lnk $lnm]]
}
#
#	Send strings to the server
#
proc sendIRC {op args} {
    global monitorOut
    set msg $op
    set last {}
    foreach arg $args  {
	if {$arg != {}} {
	    if {$last !={}} { append msg " $last" }
	    set last $arg
	}
    }
    if {$last != {}} { append msg " :$last" }
    global sock
    if {$sock != {}} {
	if [catch [list puts $sock "$msg\r\n"]] {
	    closeIRC $sock
	} {
	    if {$monitorOut} {puts stdout >$msg}
	}
    }
}

proc inactiveTest args {
    global Close
    global CloseTime
    global CloseCount
    global Active
    global testTime
    foreach chan [array names Close] {
	if $Close($chan) {
	    if $Active($chan) {
		set CloseCount(${chan}) $CloseTime(${chan})
	    } {
		if {[incr CloseCount(${chan}) -$testTime] <= 0} {
		    if !$Active($chan) {
			catch "wm iconify .$chan"
			if [winfo exists .@zd${chan}] {
			    catch "wm iconify .@zd${chan}"
			}
		    }
		    set CloseCount(${chan}) $CloseTime(${chan})
		}
	    }
	}
	set Active($chan) 0
    }
}
#
# Send string to channel and echo to channel window
#
proc sendToChannel {chan string args} {
    notIdle {}
    if {$string != {}} {
	global Secure
	set rchan [expr {[info exists Secure($chan)] ? $Secure($chan) : $chan}]
	sendIRC PRIVMSG ${rchan} $string
	[expr {$args == {} ? "addText" : "noPopAdd"}] @me ${chan} "> $string"
    }
}
#
# Send string to channel as an ACTION and echo to channel window
#
proc sendAction {chan string} {
    notIdle {}
    if {$string != {}} {
	global nickname
	sendIRC PRIVMSG ${chan} "\001ACTION $string\001"
	addText @me ${chan} "* $nickname $string"
    }
}

proc clearChannel {name hist} {
    if $hist {
	.$name.oFrm.textFrm.text delete 1.0 end
    }
    .$name.oFrm.textFrm.text yview end
}
#
# Leaving channels :
#	doLeave sends the PART message
#	leaveChannel does the are you sure dialog
#	ircPART is called when the PART message comes back from the server.
#
#	Conversation and Notice channels do not need a PART message
#
proc doLeave chan {
    global sock
    global Leaving
    if {[channel ${chan}] && $sock != {} && ![info exists Leaving(${chan})]} {
	sendIRC PART ${chan}
	set Leaving(${chan}) 1
    } {
	killChannel ${chan}
    }
}

proc leaveChannel chan {
    mkDialog LEAVE .@${chan} "Leaving ${chan}" "Really leave ${chan}?" {} \
      "OK {doLeave ${chan}}" {Cancel {}}
}

proc ircPART {prefix param args} {
    set nkinfo [mungPrefix ${prefix}]
    set chn [lindex $args 0]
    set chan [string tolower $chn]
    if [lindex $nkinfo 1] {
	killChannel ${chan}
    } {
	optText LEAVE ${chan} "*** [lindex $nkinfo 0] has left channel $chn"
	handleOn LEAVE [list ${chan} ${prefix}]
	killUser ${chan} [lindex $nkinfo 2]
    }
}
proc setMode {which mode args} { sendIRC MODE $which $mode [lindex $args 0] }

proc changeMode {chan mode} {
    global ${chan}
    setMode ${chan} [expr {[set ${chan}(${mode})] ? {+} : {-}}]${mode}
}

proc chngMode {nk chan mode} {
    case $mode {
    o { global ${chan}Op ; set val [set ${chan}Op($nk)] }
    v { global ${chan}Spk ; set val [set ${chan}Spk($nk)] }
    }
    setMode ${chan} [expr {$val ? {+} : {-}}]$mode $nk
}

proc kick {who chan} {
    mkDialog {} .@kick {Kick} "Really kick $who from channel ${chan}?" \
      {{Message {}}} "OK {sendIRC KICK ${chan} $who}" {Cancel {}}
}

#
#	Sets or clears the Away message at the server
#
proc doAway args { sendIRC AWAY [join $args] }
#
#	Called when the BRB button is pressed
#
proc doBRB {args} {
    global activeChannels
    global away
    if {$away == 0} {
	.oFrm.bf1.brb conf -text Back
	foreach chans $activeChannels { sendToChannel ${chans} brb -nopop }
	doAway {Back soon.}
    } {
	.oFrm.bf1.brb conf -text BRB
	foreach chans $activeChannels { sendToChannel ${chans} back -nopop }
	doAway {}
    }
}
#
#	channel Operations
#
proc channelInvite {chan args} { if {${chan} != {}} { userInvite $chan } }

proc doNotice {chan string} {
    if {$string != {}} {
	if [winfo exists .${chan}] { addText @me ${chan} "- $string" }
	sendIRC NOTICE ${chan} $string
    }
}

proc channelNotice {chan args} {
    if {${chan} != {}} {
	mkEntryBox .@notice${chan} "Notice to ${chan}" \
	  {Enter your notice text:} \
	  {{Notice {}}} "OK {doNotice ${chan}}" {Cancel {}}
    }
}	

proc channelJoin {chan args} {
    set key [lindex $args 0]
    global ${chan}Key ; set ${chan}Key $key
    sendIRC JOIN [string tolower ${chan}] $key
}

proc channelMonitor chan {
    if {$chan != {}} {
	global monitor
	set chan [string tolower ${chan}]
	if {[lsearch $monitor ${chan}] < 0} {
	    lappend monitor ${chan}
	} {
	    set monitor [list ${chan}]
	}
	sendIRC NAMES ${chan}
    }
}

proc channelWho chan { sendIRC WHO ${chan} }

proc channelNames chan { sendIRC NAMES ${chan} }

proc channelList doit {
    global allChannels
    set allChannels {}
    if ![winfo exists .@list] {
	global showLocal
	global showPublic
	global showPrivate
	global minMembers 
	global topicOnly
	toplevel .@list -class Zircon
	wm title .@list {IRC Channel List}
	wm iconname .@list {IRC Channel List}
	wm minsize .@list 10 1
	set oFrm .@list
	frame $oFrm.filter -relief raised
	checkbutton $oFrm.filter.public -variable showPublic -text Public
	checkbutton $oFrm.filter.local -variable showLocal -text Local
	checkbutton $oFrm.filter.private -variable showPrivate -text Private
	checkbutton $oFrm.filter.topic -variable topicOnly -text {With Topic}

	set tmp $minMembers
	scale $oFrm.filter.members \
	  -from 1 -to 25 -label "Minimum Number of Members" \
	  -showvalue 1 -orient horizontal \
	  -command {global minMembers ; set minMembers }

	$oFrm.filter.members set $tmp

	zpack $oFrm.filter members {fillx}
	zpack $oFrm.filter {public local private topic} {left fillx}
	global listPattern
	labelEntry 0 $oFrm.filter2 {-text Channel} $listPattern {}
	global topicPattern
	labelEntry 0 $oFrm.filter3 {-text Topic} $listPattern {}

	frame $oFrm.channels -relief raised
	scrollbar $oFrm.channels.vscroller -command {.@list.channels.list yview}
	listbox $oFrm.channels.list -geometry 20x8 \
	  -xscrollcommand {.@list.hsFrm.hscroller set} \
	  -yscrollcommand {.@list.channels.vscroller set} -setgrid 1

	zpack $oFrm.channels list {left expand fill}
	zpack $oFrm.channels vscroller {left filly} 

	frame $oFrm.hsFrm
	scrollbar $oFrm.hsFrm.hscroller -command {.@list.channels.list xview} \
	  -orient horizontal

	frame $oFrm.hsFrm.pf0
	zpack $oFrm.hsFrm hscroller {left expand fillx}
	zpack $oFrm.hsFrm pf0 {left padx 20} 

	frame $oFrm.btn
	button $oFrm.btn.ok -text OK -command {destroy .@list} -relief raised
	button $oFrm.btn.clear -text Clear -relief raised \
	  -command {.@list.channels.list delete 0 end ; set allChannels {}}
	button $oFrm.btn.list -text List -relief raised \
	  -command {
	    .@list.channels.list delete 0 end
	    sendIRC LIST ;
	    set allChannels {}
	  }
	zpack $oFrm.btn {list clear ok} {left expand fillx}
	zpack $oFrm {filter filter2 filter3} {fillx}
	zpack $oFrm channels {expand fillx filly}
	zpack $oFrm {hsFrm btn} {fillx}
	bind $oFrm.channels.list <Double-Button-1> {
	    channelJoin [lindex $allChannels [%W nearest %y]] {}
	}
	bind $oFrm.channels.list <Double-Button-2> {
	    whoAction [lindex $allChannels [%W nearest %y]]
	}
	bind $oFrm.channels.list <Button-1> {
	    entrySet .oFrm.cmdLine.channel \
	      "[lindex $allChannels [%W nearest %y]]"
	}
    } {
	deiconify .@list
	raise .@list
	if {$doit != {}} {.@list.channels.list delete 0 end}
    }
    global showList
    set showList 0
    if {$doit != {}} {
	sendIRC LIST $doit
	set showList 1
    }
}

proc mungNotice msg {
    if [string match {*Received KILL message*} $msg] {
	regsub {Path:.*\(} $msg {Path: ... (} msg
    }
    return $msg
}
#
#   Handle IRC cmds
#
proc ircNOTICE {prefix param args} {
    set nkinfo [mungPrefix $prefix]
    if [ignoreSet [lindex $nkinfo 4] notices] { return }
    if {[lindex $nkinfo 3] == {}} {
	addText {} @info [mungNotice $param]
    } {
	set nk [lindex $nkinfo 2]
	set chan [lindex $args 0]
	if {[set fst [string first "\001" $param]] >= 0} {
	    incr fst
	    set lst [string last "\001" $param]
 	    incr lst -1
	    set cp [string range $param $fst $lst]
	    if ![string match {ZIRCON Sorry*} $cp] {
		mkInfoBox CTCP .@ctcp$nk "CTCP Reply" \
		  "CTCP Reply from [lindex $nkinfo 0]: $cp" {OK {}}
	    }
	} {
	    if [string match {[#$&]*} ${chan}] {
		if [winfo exists .${chan}] {
		    addText ${nk} ${chan} "-$nk- $param"
		} {
		    addText ${nk} @info "-${nk}:${chan}- $param"
		}
	    } {
		if {![active ${nk}]} {
		    global busy
		    if $busy {
			addText {} @info \
			  "Notice from $nk at [exec date] : $param"
		    } {
			makeChannel ${nk} N
			addText {} ${nk} [exec date] -noscroll
			addText {} ${nk} "$param" -noscroll
		    }
		} {
		    addText ${nk} ${nk} "$param"
		}
	    }
	}
    }
}

proc addCTCPMenu {name nk} {
    $name add cascade -label CTCP -menu $name.ctcp
    menu $name.ctcp
    foreach cmd \
      {Clientinfo Echo Errmsg Finger Pid Ping
       Source Time Version Userinfo Zircon} {
	$name.ctcp add command -label $cmd \
	  -command "doCtcp [string toupper $cmd] $nk"
    }
}

proc addDCCMenu {name nk} {
    $name add cascade -label DCC -menu $name.dcc
    menu $name.dcc
    foreach cmd {Send Chat} {
	$name.dcc add command -label $cmd \
	  -command "doDCC [string toupper $cmd] $nk"
    }
}

proc doNotify {who} {
    global TellMe
    global notify
    set y [lsearch $notify ${who}]
    if $TellMe($who) {
	if {$y < 0}  { lappend notify $who ; sendISON }
    } {
	global ISON
	if [info exists ISON($who)] {
	    if [winfo exists .@friends.users.userList.frame.$who] {
		markButton .@friends.users.userList.frame.$who {}
	    }
	    unset ISON($who)
	}
	listdel notify $y
    }
}

proc doWhois {nk where} {if {$nk != {}} { sendIRC WHOIS $where $nk } }

proc makeUserMenu {chan name nk oper spk} {
    global ircop
    menu $name
    $name add command -label Whois -command "doWhois $nk {}"
    $name add command -label Msg -command "makeChannel $nk M"
    $name add command -label Notice -command "channelNotice $nk"
    $name add command -label Time -command "sendIRC TIME $nk"
    addCTCPMenu $name $nk
    addDCCMenu $name $nk
    $name add checkbutton -label Notify -variable TellMe($nk) \
	  -command "doNotify $nk"
    if {$chan != {}} {
	addIgnoreMenu $name $nk
	$name add command -label Finger -command "finger $nk"
        set st [expr {[operator ${chan}] ? "normal" : "disabled"}]
	$name add checkbutton -label Speak -variable ${chan}Spk($nk) \
	  -command "chngMode $nk ${chan} v" -state $st
	$name add checkbutton -label ChanOp -variable ${chan}Op($nk) \
	  -command "chngMode $nk ${chan} o" -state $st
	$name add command -label Kick -command "kick $nk ${chan}" -state $st
	$name add command -label Ban+Kick -command "banKick $nk ${chan}" \
	  -state $st
	$name add command -label Kill -command "kill $nk" \
	  -state [expr {$ircop ? "normal" : "disabled"}]
    }
    return $name
}

proc makeUserButton {chan win nk nm op sp} {
    if {[winfo exists $win.$nk]} {return}
    global ${chan}Op
    global ${chan}Spk
    set ${chan}Op($nk) $op
    set ${chan}Spk($nk) $sp
    menubutton $win.$nk -text "$nm"
    $win.$nk conf -menu [makeUserMenu ${chan} $win.$nk.menu $nk $op $sp]
    if $op {
	markButton $win.$nk operator
    } {
	if $sp { markButton $win.$nk speaker }
    }
    zpack $win $nk { }
    set wd [winfo reqwidth $win.$nk]
    set ht [winfo reqheight $win.$nk]
    set ht [expr { $ht * [llength [winfo children $win]]}]
    .${chan}.users.userList conf -scrollregion [list 0 0 $wd $ht]
    return
}

proc doHeal lnk {
    global Limbo
    catch "unset Limbo($lnk)"
    global Split
    global Heal
    foreach sl [array names Split] {
	if {[set x [lsearch $Split($sl) $lnk]] >= 0} {
	    if ![info exists Heal($sl)] { optText HEAL @info "*** Heal - $sl" }
	    set v $Split($sl)
	    listdel v $x
	    if {$v != {}} {
		set Split($sl) $v
		set Heal($sl) 120000
	    } {
		unset Split($sl)
		catch "unset TSplit($sl)"
		catch {unset Heal($sl)}
	    }
	}
    }
    if [winfo exists .@friends.users.userList.frame.$lnk] {
	.@friends.users.userList.frame.$lnk conf -state normal
    }
}

proc ircJOIN {prefix param args} {
    set nkinfo [mungPrefix $prefix]
    set nk [lindex $nkinfo 0]
    set chan [string tolower $param]
    if [lindex $nkinfo 1] {
	makeChannel $chan C
    } {
	set lnk [lindex $nkinfo 2]
	if ![winfo exists .${chan}.users.userList.frame.$lnk] {
	    optText JOIN ${chan} \
	      "*** $nk ([lindex $nkinfo 3]) has joined channel $param"
	    makeUserButton ${chan} .${chan}.users.userList.frame $lnk $nk 0 0
	    handleOn JOIN [list $chan $prefix]
	    setOps $prefix ${chan} $lnk
	} {
	    .${chan}.users.userList.frame.$lnk conf -state normal
	    doHeal ${lnk}
	}
    }
}

proc setOps {prefix chan lnk} {
    foreach n [getAutoOp ${chan}] {
	if [regexp -nocase $n $prefix] {
	    global MkOp
	    lappend MkOp(${chan}) $lnk
	    break
	}
    }
}

proc killUser {chan who} {
    global ${chan}Op
    global ${chan}Spk
    set who [string tolower $who]
    set win .${chan}.users.userList.frame
    catch "destroy $win.$who"
    set wd [winfo reqwidth $win.@me]
    set ht [winfo reqheight $win.@me]
    set ht [expr { $ht * [llength [winfo children $win]]}]
    .${chan}.users.userList conf -scrollregion [list 0 0 $wd $ht]
    catch "unset ${chan}Op($who)"
    catch "unset ${chan}Spk($who)"
}

proc ircNICK {prefix param args} {
    set nkinfo [mungPrefix $prefix]
    if {[lindex $nkinfo 1]} {
	setNickname $param
    } {
	global activeChannels
	set nk [lindex $nkinfo 0]
	set lnk [lindex $nkinfo 2]
	set lpr [string tolower $param]
	foreach chan $activeChannels {
	    if {[winfo exists .${chan}.users.userList.frame.$lnk]} {
		global ${chan}Op
		global ${chan}Spk
		set op [set ${chan}Op($lnk)]
		set sp [set ${chan}Spk($lnk)]
		killUser ${chan} $lnk
		makeUserButton ${chan} .${chan}.users.userList.frame \
		  $lpr $param $op $sp
		optText NICK ${chan} "*** $nk is now known as $param"
		handleOn NICK [list $prefix $param]
	    }
	}
	if {[winfo exists .$lnk]} {
	    wm title .$lnk "Conversation with $param"
	    rename .$lnk .$lpr
	}
    }
}

proc flagChannel {chan state} {
    foreach w {mode channel action} {
	catch ".${chan}.oFrm.cmds.cmds0.$w conf -state $state"
    }
    if [winfo exists .${chan}.oFrm.topic] {
	.${chan}.oFrm.topic.label conf -state disabled
	.${chan}.oFrm.topic.entry conf -state disabled
	foreach w [winfo children .${chan}.users.userList.frame] {
	    $w conf -state $state
	}
	if {$state == "normal"} {
	    global Split
	    foreach w [array names Split] {
		foreach n $Split($w) {
		    if [winfo exists .${chan}.users.userList.frame.$n] {
			.${chan}.users.userList.frame.$n conf -state disabled
		    }
		}
	    }
	}
    }	
}

proc killChannel {chan} {
    set chan [string tolower ${chan}]
    if [catch "winfo toplevel .$chan" win] { return }
    catch "destroy $win"
    global Name ; catch "unset Name($win)"
    foreach v {Open Close Active History BF Bg Fg Jump Quiet HPos \
      HBuff Icon Actions IconBM LogFile Leaving} {
	global $v ; catch "unset ${v}($chan)"
    }
    foreach v {Op Spk Key} {
	global ${chan}${v}
	catch "unset ${chan}${v}"
    }
    global activeChannels
    if {[set dx [lsearch $activeChannels ${chan}]] >= 0} {
	listdel activeChannels $dx
    }
    global Log
    if [info exists Log(${chan})] { close $Log(${chan}) ; unset Log(${chan}) }
}

proc ircError {prefix param args} {
    addText @ERROR @info "*** ERROR : $param"
}

proc ircINVITE {prefix param args} {
    set nkinfo [mungPrefix $prefix]
    if [ignoreSet [lindex $nkinfo 4] invites] { return }
    mkDialog {} .@invite "Invitation" \
      "[lindex $nkinfo 0] invites you to channel $param." {} \
      "Join {channelJoin $param}" {Ignore {}}
}

proc ircKILL {prefix param args} {
    set nkinfo [mungPrefix $prefix]
    set who [string tolower [lindex $args 0]]
    if {[me $who]} {
	global sock
	if {$sock != {}} {
	    closeIRC $sock
	}
	mkDialog KILLED .@killed "Killed"\
	  "You have been killed by [lindex $nkinfo 0] ($param)" \
	  {} {OK {}}
    } {
	optText KILL ${chan} \
	  "*** $who has been killed by [lindex $nkinfo 0] ($param)"
	handleOn KILL [list $prefix $who]
	killUser ${chan} $who
   }
}

proc ircKICK {prefix param args} {
    set nkinfo [mungPrefix $prefix]
    set chan [lindex $args 0]
    set who [string tolower [lindex $args 1]]
    if {[me $who]} {
	killChannel ${chan}
	mkDialog KICKED .@kicked "Kicked from ${chan}"\
	  "You have been kicked off channel ${chan} by [lindex $nkinfo 0] ($param)" \
	  {} {OK {}}
    } {
	optText KICK ${chan} \
	  "*** $who has been kicked off channel ${chan} by [lindex $nkinfo 0] ($param)"
	handleOn KICK [list ${chan} ${prefix} ${who}]
	killUser ${chan} $who
   }
}

proc opItems {chan state} {
    foreach name [winfo children .${chan}.users.userList.frame] {
	setState $name.menu chanop $state
    }
    set mn .${chan}.oFrm.cmds.cmds0.mode.menu
    set vl [$mn index {Actions}]
    incr vl
    set last [$mn index last]
    while {$vl <= $last} {
	$mn entryconfigure $vl -state $state
	incr vl
    }
    global ${chan}
    if {$state == "normal" || [set ${chan}(t)]} {
	.${chan}.oFrm.topic.entry conf -state $state
	.${chan}.oFrm.topic.label conf -state $state
    }
}

proc markButton {name which} {
    foreach opt {font foreground background activeForeground \
      activeBackground} {
	set uopt [capitalise $opt]
	set fopt ${which}[expr {$which != {} ? $uopt : $opt}]
	set lopt [string tolower $opt]
	if {[set cl [option get $name $fopt $uopt]] != {}} {
	    $name conf -$lopt $cl
	} {
	    if {$which == {}} {
		if {[set cl [lindex [$name conf -$lopt] 3]] != {}} {
		    $name conf -$lopt $cl
		}
	    }
	}
    }
    set af [lindex [$name conf -activeforeground] 4]
    set fg [lindex [$name conf -foreground] 4]
    if {$af == $fg} {
	set bg [lindex [$name conf -background] 4]
	$name conf -activeforeground $bg -activebackground $fg
    }
}

proc markOp {chan who} {
    global ${chan}Op ; set ${chan}Op($who) 1
    if {[me $who]} {
	opItems $chan normal
	set who @me
    }
    markButton .${chan}.users.userList.frame.$who operator
}

proc unmarkOp {chan who} {
    global ${chan}Op ; set ${chan}Op($who) 0
    global ${chan}Spk
    set par [expr  {[set ${chan}Spk($who)] ? "speaker" : {}}]
    if {[me $who]} {
	opItems $chan disabled
	set who @me
    }
    markButton .${chan}.users.userList.frame.$who $par
}

proc markV {chan who} {
    global ${chan}Spk ; set ${chan}Spk($who) 1
    if {[me $who]} {
	if {![operator ${chan}]} {
	    markButton .${chan}.users.userList.frame.@me speaker
	 }
    } {
	global ${chan}Op
	if {![set ${chan}Op($who)]} {
	    markButton .${chan}.users.userList.frame.$who speaker
	}
    }
}

proc unmarkV {chan who} {
    global ${chan}Spk ; set ${chan}Spk($who) 0
    if {[me $who]} {
	if {![operator ${chan}]} {
	    markButton .${chan}.users.userList.frame.@me {}
	}
    } {
	global ${chan}Op
	if {![set ${chan}Op($who)]} {
	    markButton .${chan}.users.userList.frame.$who {}
	}
    }
}

proc unsetUser {md} {
    case $md {
    [oO] { unmakeIRCOp 0 }
    w	 { global wallops ; set wallops 0 }
    s	 { global srvmsg ; set srvmsg 0 }
    i	 { global invisible ; set invisible 0 }
    }
}

proc setUser {md} {
    case $md {
    [Oo] { makeIRCOp }
    w	 { global wallops ; set wallops 1 }
    s	 { global srvmsg ; set srvmsg 1 }
    i	 { global invisible ; set invisible 1 }
    }
}

proc handleMode {chan vals} {
    global ${chan}
    set nxt {}
    set flag {}
    foreach par $vals {
	if {$nxt != {}} {
	    set flag [string index $nxt 0]
	    set m [string index $nxt 1]
	    set nxt [string range $nxt 2 end]
	    set lpar [string tolower $par]
	    case $m {
	    o { [expr {$flag == {+} ? {markOp} : {unmarkOp}}] ${chan} $lpar }
	    v { [expr {$flag == {+} ? {markV} : {unmarkV}}] ${chan} $lpar }
	    k   {
		    global ${chan}Key
		    set ${chan}Key [expr {$flag == {+} ? $par : {}}]
		}
	    }
	    handleOn MODE [list ${chan} ${flag}${m} $lpar]
	} {
	    set nxt {}
	    foreach m [split $par {}] {
		case $m {
		[+-] { set flag $m }
		[kovlb] { append nxt ${flag}${m} }
		[psinm] {
			set ${chan}($m) [expr {$flag == "+"}]
			handleOn MODE [list ${chan} ${flag}${m}]
		    }
		t   {
			set x [expr {$flag == "+"}]
			set ${chan}(t) $x
			if {![operator ${chan}]} {
			    set st [expr {$x ? "disabled" : "normal"}]
			    .${chan}.oFrm.topic.label conf -state $st
			    .${chan}.oFrm.topic.entry conf -state $st
			    .${chan}.oFrm.cmds.cmds0.channel.menu entryconfigure 5 \
			      -state $st
			}
			handleOn MODE [list ${chan} ${flag}t]
		    }
		}
	    }
	}
    }
}

proc ircMODE {prefix param args} {
    set nkinfo [mungPrefix $prefix]
    set nk [lindex $nkinfo 0]
    set chan [string tolower [lindex $args 0]]
    if ![channel ${chan}] {
	if [me ${chan}] {
	    if {[set md [lindex $args 1]] == {}} { set md $param }
	    foreach m [split $md {}] {
		case $m {
	 	- { set cmd unsetUser }
		+ { set cmd setUser }
		* { $cmd $m }
		}
	    }
	}
	return
    }
    optText MODE ${chan} "*** Mode change \"[string trim \
      [join [lrange $args 1 end]]]\" on channel ${chan} by $nk"
    handleMode $chan [lrange $args 1 end]
}

proc ircPRIVMSG {prefix param args} {
    set nkinfo [mungPrefix $prefix]
    set nk [lindex $nkinfo 0]
    set lnk [lindex $nkinfo 2]
    set ign [lindex $nkinfo 4]
    set chan [string tolower [lindex $args 0]]
    if {[set fst [string first "\001" $param]] >= 0} {
	incr fst
	set lst [string last "\001" $param]
 	incr lst -1
	set cp [string range $param $fst $lst]
	set ctcp [split $cp]
	set value \
	  [handleCTCP [lindex $ctcp 0] ${chan} $nk $lnk $prefix $ign "$cp"]
	if {$value == {}} { return }
	incr fst -2
	incr lst +2
	set param \
	  "[string range $param 0 $fst]$value[string range $param $lst end]"
    }
    set pfx "<$nk>"
    if {[me ${chan}]} {
	if [ignoreSet $ign notes] { return }
	if {![active $lnk]} {
	    global busy
	    if $busy {
		global nickname
		sendIRC NOTICE $nk \
"I am busy and am not accepting calls at the moment."
		addText {} @info "Message from $nk at [exec date] : $param"
	    } {
		makeChannel $lnk M
		addText {} $lnk [exec date] -noscroll
		addText $lnk $lnk "$pfx $param" -noscroll
	    }
	    return
	}
	set chan $lnk
    }
    if [ignoreSet $ign public] { return }
    if [winfo exists .${chan}] {
	set where ${chan}
    } {
	set where @info
	set pfx "<${nk}/${chan}>"
    }
    addText $lnk ${where} "$pfx $param"
    global Ptns
    if [info exists Ptns(${where})] {
	foreach p $Ptns($where) {
	    set pt [lindex $p 0]
	    if {[regexp -nocase [lindex $pt 0] $pfx] && \
		[regexp [lindex $pt 1] $param]} {
		uplevel #0 "[lindex $p 1]"
	    }
	}
    }
}

proc netsplit {string} {
    return [regexp -nocase \
      {^([a-z0-9*_-]+\.)+([a-z0-9_-]+) ([a-z0-9*_-]+\.)+([a-z0-9_-]+)$} $string]
}

proc ircQUIT {prefix param args} {
    global toInfo    
    global activeChannels
    set nkinfo [mungPrefix $prefix]
    set nk [lindex $nkinfo 0]
    set lnk [lindex $nkinfo 2]
    if {[netsplit "$param"]} {
	if ![info exists Split($param)] {
	    optText SPLIT @info "*** Netsplit - $param"
	    global TSplit ; set TSplit($param) 600000
	    global Heal ; catch "unset Heal($param)"
	}
	foreach chan $activeChannels {
	    if [winfo exists .${chan}.users.userList.frame.${lnk}] {
		.${chan}.users.userList.frame.$lnk conf -state disabled
	    } {
		if {${chan} == ${lnk}} {
		    addText {} ${chan} "*** Netsplit - $param"
		    flagChannel ${lnk} disabled
		}
	    }
	}
	if [winfo exists .@friends.users.userList.frame.$lnk] {
	    .@friends.users.userList.frame.$lnk conf -state disabled
	}
	global Limbo ; set Limbo(${lnk}) {}
	global Split ; lappend Split($param) $lnk
    } {
	if {[set ti [expr {[lsearch $toInfo SIGNOFF] >=0}]]} {
	    addText @QUIT @info "*** Signoff: $nk ($param)"
	}
	foreach chan $activeChannels {
	    if {[winfo exists  .${chan}.users.userList.frame.$lnk]} {
		if {!$ti} {
		    optText QUIT ${chan} "*** Signoff: $nk ($param)"
		}
		handleOn QUIT [list ${prefix}]
		killUser ${chan} $lnk
	    } {
		if {${chan} == $lnk} {
		    addText @QUIT ${chan} "*** $nk has signed off!!!"
		}
	    }
	}
    }
}

proc chanUpdate {add chan field value} {
    global CInfo
    global channelInfo
    global confChange
    set chng 0
    set cif [expr {$field + 1}]
    if {[set cx [listmatch $channelInfo ${chan}]] >= 0} {
	set v [lindex [set cv [lindex $channelInfo $cx]] $cif]
	set x [lsearch $v $value]
	if $add {
	    if {$x < 0} { set chng 1 ; lappend v $value }
	} {
	    if {$x >= 0} { set chng 1 ; listdel v $x }
	}
	if $chng {
	    listupdate cv $cif $v
	    if [info exists CInfo(${chan})] {
		listupdate CInfo(${chan}) $field "$v"
	    }
	    listupdate channelInfo $cx $cv
	    set confChange 1
	}
    } { 
	if $add {
	    set chin "{} {} {} {} {} {} {} {} {}"
	    listupdate chin $field [list $value]
	    set CInfo(${chan}) $chin
	    lappend channelInfo [linsert $chin 0 ${chan}]
	    set chng 1
	    set confChange 1
	}
    }
    global new
    if [info exists new(channelInfo)] {
	while {[set cx [listmatch $new(channelInfo) ${chan}]] < 0} {
	    doCAC .@confChannels.chan.nels.list.values ${chan}
	}
	set ci [lindex $new(channelInfo) $cx]
	set v [lindex $ci $cif]
	set x [lsearch $v $value]
	if $add {
	    if {$x >=0} { return $chng }
	    lappend v $value
	} {
	    if {$x < 0} { return $chng }
	    listdel v $x
	}
	listupdate ci $cif $v
	listupdate new(channelInfo) $cx $ci
	global confCSel
	if {$confCSel == ${chan}} { setCCB ${chan} }
    }
    return $chng
}
#
# Topic handling procs
#
proc keepTopic {chan value} {
    if {$value == {}} return
    doTopic ${chan} $value
    if [chanUpdate 1 ${chan} 6 $value] {
	.${chan}.oFrm.topic.label.menu add command -label "[prune $value]" \
	  -command "doTopic ${chan} {$value}"
    }
}

proc getTopic chan {
    mkEntryBox .@topic "${chan} Topic" "Enter your new topic for ${chan}:" \
      {{Topic {}}} \
     "OK {doTopic ${chan}}" "Keep {keepTopic ${chan}}" {Cancel {}}
}

proc doTopic {chan val} {
    if {$val != {} } { sendIRC TOPIC ${chan} ${val}}
}

proc sendTopic win {
    if [normal $win] {
	global Name
	sendIRC TOPIC $Name([winfo toplevel ${win}]) [$win get 1.0 end]
    }
}

proc setTopic {chan string} {
    set state [lindex [.${chan}.oFrm.topic.entry conf -state] 4]
    .${chan}.oFrm.topic.entry conf -state normal
    .${chan}.oFrm.topic.entry delete 1.0 end
    doAddText {} .${chan}.oFrm.topic.entry $chan $string -noscroll
    .${chan}.oFrm.topic.entry conf -state $state
}

proc irc331 {prefix param args} {
    setTopic [string tolower [lindex "$args" 1]] {}
}

proc irc332 {prefix param args} {
    setTopic [string tolower [lindex "$args" 1]] $param
}

proc ircTOPIC {prefix param args} {
    set chan [string tolower [lindex "$args" 0]]
    setTopic $chan $param
    optText TOPIC $chan \
      "*** [lindex [mungPrefix $prefix] 0] has set the topic."
    handleOn TOPIC [list $chan $prefix $param]
}
#
# First message from the server....
#
proc irc001 {prefix param args} {
    global activeChannels
    global startup
    global nickname
    global invisible
    global wallops
    global srvmsg
    global server
    global ircport
    flagControl normal
    deIRCOp
    if {[set opStuff [lindex [serverData $server] 2]] != {}} {
	set nk [lindex $opStuff 0]
	sendIRC OPER [expr {$nk == {} ? $nickname : $nk}] [lindex $opStuff 1]
    }
    if $invisible { setMode ${nickname} "+i" }
    if $wallops	  { setMode ${nickname} "+w" }
    if $srvmsg	  { setMode ${nickname} "+s" }
    foreach ch $activeChannels {
	if {[channel ${ch}]} {
	    if {[winfo exists .${ch}]} {
		global lcNickname
		flagChannel ${ch} normal
		unmarkV ${ch} ${lcNickname}
		unmarkOp ${ch} ${lcNickname}
		sendIRC MODE ${ch}
	    }
	    channelJoin ${ch} {}
	} {
	    if {[winfo exists .${ch}]} { flagChannel ${ch} normal }
	}
    }
    addText {} @info "*** $param"
    setupTests
    handleOn STARTUP [list $server $ircport]
    set startup 0
}

proc irc004 {prefix param args} {
    global server
    global serverInfo
    set serverInfo [lrange $args 1 4]
    set server [lindex $serverInfo 0]
    addText {} @info "*** umodes available [lindex $serverInfo 2], channel modes\
 available [lindex $serverInfo 3]"
}

proc ircNUM {number prefix param args} {
    set txt {}
    foreach arg [lrange $args 1 end] {
	if {$arg != {}} { append txt " $arg" }
    }
    append txt " $param"
    case $number {
    [45]* { mkInfoBox ERROR .@err$number "Error $number" $txt {OK {}} }
    default { addText {} @info $txt }
    }
}

proc setNickname {nk} {
    global nickname
    global lcNickname
    global activeChannels
    entrySet .oFrm.nSFrm.nickname.entry $nk
    set lcn [string tolower $nk]
    foreach chan $activeChannels {
	if {[winfo exists .${chan}.users.userList.frame.@me]} {
	    .${chan}.users.userList.frame.@me conf -text $nk
	    global ${chan}Op
	    global ${chan}Spk
	    set op [set ${chan}Op($lcn) [set ${chan}Op($lcNickname)]]
	    set sp [set ${chan}Spk($lcn) [set ${chan}Spk($lcNickname)]]
	    destroy .${chan}.users.userList.frame.@me.menu
	    makeUserMenu ${chan} .${chan}.users.userList.frame.@me.menu \
	      $lcn $op $sp
	    unset ${chan}Spk($lcNickname)
	    unset ${chan}Op($lcNickname)
	}
    }
    set lcNickname $lcn
    set nickname $nk
}

proc irc301 {prefix param args} {
    global whois
    if [info exists whois(info)] {
	set whois(away) $param
    } {
	addText {} @info "*** [lindex "$args" 1] is away: $param"
    }
}

proc irc303 {prefix param args} {
    global ISON
    global Limbo
    global friendsOn
    set signons {}
    set signoffs {}
    set msg {}
    set lpar {}
    foreach who $param {
	lappend lpar [set lwho [string tolower $who]]
	if {![info exists ISON($lwho)] || [info exists Limbo($lwho)]} {
	    set ISON($lwho) 0
	    catch "unset Limbo($lwho)"
	    if {[winfo exists .@friends.users.userList.frame.$lwho] \
	      && ![normal .@friends.users.userList.frame.$lwho] } {
		doHeal ${lwho}
	    } {
		lappend signons $who
		if {$friendsOn && [winfo exists .@friends] && [friend $lwho]} {
		    doNewFriend $who
		}
		if [winfo exists .@friends.users.userList.frame.$lwho] {
		    markButton .@friends.users.userList.frame.$lwho ison
		    wm deiconify .@friends
		}
	    }
	}
    }
    if {$signons != {}} {
	set msg "Signon by $signons detected.\n"
    }
    foreach who [array names ISON] {
	if {![info exists Limbo($who)] && [lsearch $lpar $who] < 0} {
	    unset ISON($who)
	    lappend signoffs $who
	    if [winfo exists .@friends.users.userList.frame.$who] {
		if $friendsOn {
		    if [normal .@friends.users.userList.frame.$who] {
			destroy .@friends.users.userList.frame.$who
		    }
		} {
		    markButton .@friends.users.userList.frame.$who {}
		}
		wm deiconify .@friends
	    }
	}
    }
    if {$signoffs != {}} { set msg "${msg}Signoff by $signoffs detected.\n" }
    if {$msg != {} } { mkInfoBox ISON .@ison "Notify" $msg {OK {}} }
}

proc invert {button} {
    set fg [lindex [$button conf -foreground] 4]
    set bg [lindex [$button conf -background] 4]
    $button conf -foreground $bg -background $fg \
      -activeforeground $fg -activebackground $bg
}

proc irc305 {prefix param args} {
    global away
    if $away {invert .oFrm.bf1.away}
    set away 0
}

proc irc306 {prefix param args} {
    global away
    if !$away {invert .oFrm.bf1.away}
    set away 1
}

proc irc321 {prefix param args} {}

if [string match "7*" [info tclversion]] {
proc irc322 {prefix param args} {
    if ![winfo exists .@list] return
    global showList
    global allChannels
    global showPublic
    global showLocal
    global showPrivate
    global minMembers
    global topicOnly
    global listPattern
    global topicPattern
    set chan [lindex $args 1]
    if {[set listPattern [.@list.filter2.entry get]] == {}} {
	set listPattern {.*}
    }
    if {[set topicPattern [.@list.filter3.entry get]] == {}} {
	set topicPattern {.*}
    }
    if !$showList {
	switch -glob ${chan} {
	{\*}  { if !$showPrivate { return } {set chan Prv } }
	&*  { if !$showLocal   { return } }
	#*  { if !$showPublic  { return } }
	}
    }
    set memb [lindex $args 2]
    if {$showList  || (($param != {} || !$topicOnly) && \
      $memb >= $minMembers && [regexp -nocase "$listPattern" ${chan}] && \
      [regexp "$topicPattern" $param])} {
	lappend allChannels "${chan}";
	.@list.channels.list insert end \
	  "[format {%-9s %3d %s} [string range "${chan}" 0 8] $memb $param]"
    }
}
} {
proc irc322 {prefix param args} {
    if ![winfo exists .@list] return
    global showList
    global allChannels
    global showPublic
    global showLocal
    global showPrivate
    global minMembers
    global topicOnly
    global listPattern
    global topicPattern
    set chan [lindex $args 1]
    if {[set listPattern [.@list.filter2.entry get]] == {}} {
	set listPattern {.*}
    }
    if {[set topicPattern [.@list.filter3.entry get]] == {}} {
	set topicPattern {.*}
    }
    if !$showList {
	case ${chan} {
	\*  { if !$showPrivate { return } {set chan Prv } }
	&*  { if !$showLocal   { return } }
	#*  { if !$showPublic  { return } }
	}
    }
    set memb [lindex $args 2]
    if {$showList  || (($param != {} || !$topicOnly) && \
      $memb >= $minMembers && [regexp -nocase "$listPattern" ${chan}] && \
      [regexp "$topicPattern" $param])} {
	lappend allChannels "${chan}";
	.@list.channels.list insert end \
	  "[format {%-9s %3d %s} [string range "${chan}" 0 8] $memb $param]"
    }
}
}
proc irc323 {prefix param args} {global showList ; set showList 0}

proc irc324 {prefix param args} {
    handleMode [string tolower [lindex "$args" 1]] [lrange $args 2 end]
}

proc irc353 {prefix param args} {
    set chan [string tolower [lindex $args 2]]
    if {![active ${chan}]} {
	global namesTxt
	global namesChan
	if {$namesChan != ${chan}} {
	    set namesChan ${chan}
	    set namesTxt $param
	} {
	    append namesTxt "\n$param"
	}
	return
    }
    set names [split $param]
    foreach n $names {
	if {$n != {}} {
	    set op 0
	    set sp 0
	    while { [string match {[@+]*} $n] } {
		if {[string index "$n" 0] == "@"} { set op 1 } { set sp 1}
		set n [string range $n 1 end] ;
	    }
	    set nm $n ;
	    set n [string tolower $n] ;
	    if {[me $n]} {
		if {$op} { markOp $chan $n } { if {$sp} { markV $chan $n }}
	    } {
		makeUserButton $chan .${chan}.users.userList.frame $n \
		  $nm $op $sp
	    }
	}
    }
}

proc updateMon {chan names} {
    set w .@mon${chan}
    set win $w.users.userList
    set winf $win.frame
    set xist {}
    foreach n [winfo children $winf] {
	set l [split $n .]
	set x [expr {[llength $l] - 1}]
	lappend xist [lrange $l $x $x]
    }
    foreach n $names {
	if {$n == {}} continue
	set op 0
	set sp 0
	while { [string match {[@+]*} $n] } {
	    if {[string index "$n" 0] == "@"} { set op 1 } { set sp 1}
	    set n [string range $n 1 end] ;
	}
	set nm $n ;
	set n [string tolower $n] ;
	if ![winfo exists $winf.$n] {
	    menubutton $winf.$n -text $nm -menu $winf.$n.menu
	    makeUserMenu "" $winf.$n.menu $n 0 0
	    set wd [winfo reqwidth $winf.$n]
	    set sht [set ht [winfo reqheight $winf.$n]]
	    set ht [expr { $ht * [llength [winfo children $winf]]}]
	    $win conf \
	      -width $wd -scrollregion [list 0 0 $wd $ht] -scrollincrement $sht
	    $win conf -width $wd -height $ht
	    zpack $winf $n { }
	} {
	    if {[set x [lsearch $xist $n]] >= 0} {listdel xist $x}
	}
	if $op {
	    markButton $winf.$n operator
	} {
	    markButton $winf.$n [expr {$sp ? "speaker" : {} }]
	}
    }
    foreach n $xist { destroy $winf.$n }
}

proc makeMon {chan names} {
    set w .@mon${chan}
    toplevel $w -class Zircon
    wm title $w "${chan} Monitor"
    wm grid $w 10 10 10 10
    wm minsize $w 10 1

    set wu [frame $w.users -relief raised]
    frame $w.btns -relief raised
    zpack $w btns {bottom fillx}
    zpack $w users {top filly}
    scrollbar $wu.vscroller -command "$wu.userList yview" 
    set win [canvas $wu.userList -yscrollcommand "$wu.vscroller set"]
    frame $wu.userList.frame -border 0
    $wu.userList create window 0 0 -window $wu.userList.frame -anchor nw
    zpack $wu {userList vscroller} {left filly} 
    button $w.btns.cancel -text Cancel -command "
	destroy $w
	global monitor
	listdel monitor \[lsearch \$monitor {${chan}} \]
    "
    button $w.btns.join -text Join -command "
	channelJoin ${chan}
    "
    bind $wu <Destroy> "
	global monitor
	if {\[set x \[lsearch \$monitor {${chan}} \] \] >= 0} {
	    listdel monitor \[lsearch \$monitor {${chan}} \]
	}
    "
    zpack $w.btns {cancel join} {left fillx expand}
    set winf $wu.userList.frame
    foreach n $names {
	if {$n == {}} continue
	set op 0
	set sp 0
	while { [string match {[@+]*} $n] } {
	    if {[string index "$n" 0] == "@"} { set op 1 } { set sp 1}
	    set n [string range $n 1 end] ;
	}
	set nm $n ;
	set n [string tolower $n] ;
	menubutton $winf.$n -text $nm -menu $winf.$n.menu
	makeUserMenu "" $winf.$n.menu $n 0 0
	if $op {
	    markButton $winf.$n operator
	} {
	    if $sp { markButton $winf.$n speaker }
	}
	set wd [winfo reqwidth $winf.$n]
	set sht [set ht [winfo reqheight $winf.$n]]
	set ht [expr { $ht * [llength [winfo children $winf]]}]
	$win conf \
	  -width $wd -scrollregion [list 0 0 $wd $ht] -scrollincrement $sht
	$win conf -width $wd -height $ht
	zpack $winf $n { }
    }
}

proc irc366 {prefix param args} {
    set chan [string tolower [lindex $args 1]]
    global namesChan
    global namesTxt
    if {$namesChan == ${chan}} {
	global monitor
	if {[lsearch $monitor ${chan}] >= 0 } {
	    if [winfo exists .@mon${chan}] {
		updateMon ${namesChan} [split $namesTxt]
	    } {
		makeMon ${namesChan} [split $namesTxt]
	    }
	} {
	    mkInfoBox NAMES .@names${chan} "Names ${chan}" $namesTxt {OK {}}
	}
    }
    set namesChan {}
    set namesTxt {}
}

proc irc376 {prefix param args} {}

proc ircItems {state} {
    global activeChannels
    foreach chan $activeChannels {
	if [winfo exists .${chan}.users.userList.frame] {
	    foreach name [winfo children .${chan}.users.userList.frame] {
		setState $name.menu ircop $state
	    }
	}
    }
    setState .oFrm.bf2.servers.menu ircSrv $state
    setState .oFrm.bf2.users.menu ircop $state
    .oFrm.nSFrm.cr.ircop conf -state $state
}

proc deIRCOp {} { global nickname ; unmakeIRCOp 1 ; setMode $nickname -O }

proc irc381 {prefix param args} { makeIRCOp ; addText {} @info "*** $param" }
proc irc394 {prefix param args} {}

proc irc252 {prefix param args} {
    set nm [lindex "$args" 1]
    addText {} @info "*** There [expr {$nm == 1 ? {is 1 operator} :
      "are $nm operators"}] online."
}

proc irc253 {prefix param args} {
    set nm [lindex "$args" 1]
    addText {} @info "*** There [expr {$nm == 1 ? {is 1 unknown connection} :
      "are $nm connections"}]."
}

proc irc254 {prefix param args} {
    set nm [lindex "$args" 1]
    addText {} @info "*** There [expr {$nm == 1 ? {is 1 channel} :
      "are $nm channels"}] formed."
}

proc parseMsg {msg} {
    global server

    set sp [string first " " $msg]
    set prefix [string range $msg 0 [expr {$sp - 1}]]
    case $prefix {
    :* {
	set msg [string range $msg [expr {$sp + 1}] end]
    }
    PING { sendIRC PONG [string range $msg 5 end] ; return }
    default { set prefix :$server }
    }
    set cln [string first : $msg]
    if {$cln >= 0} {
	set param [string range $msg [expr {$cln + 1}] end]
	set msg [string range $msg 0 [expr {$cln - 1}]]
    } {
	set param {}
    }
    set cmdList [split $msg]
    set i 1
    foreach e $cmdList {
	if {$e != {}} {
	    set p${i} [lindex $cmdList $i]
	    incr i
	}
    }
    for {set j $i} {$j <= 15} {incr j} {
	set p${j} {}
    }
    set cmd [lindex $cmdList 0]
    if {[info procs "irc$cmd"] == {} && ![auto_load "irc$cmd"]} {
	ircNUM $cmd $prefix $param $p1 $p2 $p3 $p4 $p5 $p6 $p7 \
	  $p8 $p9 $p10 $p11 $p12 $p13 $p14 $p15
    } {
	irc$cmd $prefix $param $p1 $p2 $p3 $p4 $p5 $p6 $p7 \
	  $p8 $p9 $p10 $p11 $p12 $p13 $p14 $p15
    }
    update idletasks
}

proc closeIRC {conn} {
    global server
    catch "dp_filehandler $conn"
    catch "close $conn"
    global sock ; if {$sock == $conn} { set sock {} }
    flagControl disabled
    global activeChannels
    foreach w $activeChannels { flagChannel ${w} disabled }
    global away
    if $away { invert .oFrm.bf1.away }
    set away 0
    mkDialog {} .@close Shutdown \
      "Server $server has closed the connection." {} {OK {}}
}

proc ircInput {mode conn} {
    case $mode in {
    r   {
	    global monitorIn
	    if {[catch "gets $conn" buffer] || [eof $conn]} {
		closeIRC $conn
	    } {
		if {$monitorIn} {puts stdout <$buffer}
		if {[set cr [string first "\r" $buffer]] >= 0} {
		    incr cr -1
		    set buffer [string range $buffer 0 $cr]
		}
		parseMsg "$buffer"
	    }
	}
    e   { addText {} @info "*** Error on server connection" }
    }
}

proc changeIRCName {name} {
    global sock
    if {$sock != {}} {
	mkDialog {} .@warn Warning \
	  "Change will not take effect until next server change." {} "OK {}"
    }
    global ircname; set ircname $name
}

proc changeNickname {name} {
    global startup
    if {$startup} {
	setNickname $name
    } {
	entrySet .oFrm.nSFrm.nickname.entry "$name"
    }
    global sock ; if {$sock != {}} {sendIRC NICK $name}
}

proc doQuit msg {
    global sock
    if {$sock != {}} {
	sendIRC QUIT $msg
	catch "dp_shutdown $sock all"
	catch "close $sock"
	set sock {}
    }
    global confChange
    if $confChange {
	mkDialog SAVECONF .@save "Save Configuration" \
	  "You have made changes to your configuration. Do you wish to \
save them?" {} {No exit} {Yes {confSave all ; exit}}
	while 1 update
    }
    exit
}

proc quitZircon {} {
    global signoff
    mkDialog QUIT .@quit "Quit IRC" \
      "Really quit?" "{Message {$signoff}}" {OK doQuit} {Cancel {}}
}

proc startIRC {server port} {
    global sock
    global nickname
    global ircname
    global host
    global user
    if {$server == {}} {
	addText {} @info "*** You must specify a server" -nos
	return 0
    }
    addText {} @info "*** Connecting to port $port of server $server" -nos
    update idletasks
    if {$port == {}} {
	if [catch "dp_connect $server" val] {
	    addText {} @info \
	"*** Cannot connect to UNIX domain server $server ($val)" -nos
	    return 0
	}
    } {
	if [catch "dp_connect $server $port" val] {
	    addText {} @info "*** Cannot connect to server $server ($val)" -nos
	    return 0
	}
    }
    set sock [lindex $val 0]
    sendIRC USER $user $host $server $ircname
    sendIRC NICK $nickname
    dp_filehandler $sock re ircInput
    global noRefresh ; if !$noRefresh { channelList { } }
    global ircport ; set ircport $port
    global Icon
    wm title .@info "Zircon Information Window - $server"
    wm iconname .@info [set Icon(.@info) "Info $server"]
    wm iconname . "Control $server"
    return 1
}

proc envCheck {arg evar gvar dflt} {
    global env
    global $gvar
    set lst [set $gvar]
    if {$arg != {} || [info exists env($evar)]} {
	set v [expr {$arg != {} ? $arg : $env($evar)}]
	if {[set x [lsearch $lst $v]] > 0} { listdel $gvar $x }
	if {$x != 0} { set $gvar [linsert $lst 0 $v] }
    } {
	if {$lst == {}} { set $gvar $dflt }
    }
}
proc setupTests {} {
    global closeTime
    global testTime
    global notifyLeft
    global notifyInterval
    set notifyLeft $notifyInterval
    global monitorLeft
    global monitorTime
    set monitorLeft $monitorTime
    set testTime $notifyInterval
    if {$closeTime > 0 && $closeTime < $notifyInterval} {
	set testTime $closeTime
    }
    sendISON
    after $testTime ircTests
}

proc setupUsers {} {
    global friendsOn
    global userInfo
    global notify
    global TellMe
    global ISON
    foreach nn $userInfo {
	if {$friendsOn || [expr {[lsearch [lindex $nn 1] "notify"] >= 0}]} {
	    set lwho [string tolower [set who [lindex $nn 0]]]
	    set TellMe($lwho) 1
	    if {[lsearch $notify $lwho] < 0} { lappend notify $lwho }
	    if {[winfo exists .@friends] && \
	      (!$friendsOn || [info exists ISON($lwho)])} {
		doNewFriend $who
	    }
	}
   }
}

proc sendISON {} {
    global notify
    global sock
    if {$notify != {} && $sock != {}} { sendIRC ISON [join $notify " "] }
}


proc cleanSplit h {
    global Split
    global Heal
    if [info exists Split($h)] {
	global activeChannels
	foreach user $Split($h) {
	    if [active $user] { flagChannel $user normal }
	    foreach chan $activeChannels {
		if {[winfo exist .${chan}.users.userList.frame.$user] &&
		  ![normal .${chan}.users.userList.frame.$user]} {
		    killUser ${chan} ${user}
		}
	    }
	}
	if {[winfo exist .@friends.users.userList.frame.$user] &&
	  ![normal .@friends.users.userList.frame.$user]} {
	    global friendsOn
	    if $friendsOn {
		destroy .@friends.users.userList.frame.$user
	    } {
		markbutton .@friends.users.userList.frame.$user {}
	    }
	}
	unset Split($h)
	global TSplit ; unset TSplit($h)
    }
    catch "unset Heal($h)"
}

proc ircTests {} {
    global testTime
    global closeTime
    if {$closeTime > 0} inactiveTest
    global notifyLeft
    if {[incr notifyLeft -$testTime] <= 0} {
	sendISON
	global notifyInterval ; set notifyLeft $notifyInterval
    }
    global TSplit
    global Heal
    foreach h [array names Heal] {
	if {[incr Heal($h) -$testTime] <= 0} { cleanSplit $h }
    }
    foreach h [array names TSplit] {
	if {[incr TSplit($h) -$testTime] <= 0} { cleanSplit $h }
    }
    global zircon ; incr zircon(idle) [expr {$testTime / 1000}]
    global monitorLeft
    if {[incr monitorLeft -$testTime] <= 0} {
	global monitor
	if {$monitor != {}} {
	    set ch [lindex $monitor 0]
	    foreach m [lrange $monitor 1 end ] {
		append ch ",$m"
	    }
	    sendIRC NAMES $ch
	}
	global monitorTime
	set monitorLeft $monitorTime
    }
    global MkOp
    foreach h [array names MkOp] {
	if [operator $h] {
	    set flag +
	    set who {}
	    foreach n $MkOp($h) {
	        global ${h}Op
	        if {[info exists ${h}Op($n)] && ![set ${h}Op($n)]} {
		    append flag o
		    lappend who $n
		}
	    }
	    if {$who != {}} {
		eval sendIRC MODE $h $flag $who
	    }
	}
	unset MkOp($h)
    }
    after $testTime ircTests
}
