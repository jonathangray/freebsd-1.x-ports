#
proc away {args} {
    global aways
    lappend aways $args
}
#
proc nick {args} {
    global nicks
    lappend nicks [join $args]
}
#
proc ircname {args} {
    global ircnames
    lappend ircnames [join $args]
}
#
proc action {args} {
    global actions
    lappend actions $args
}
#
proc zbind {chan sequence action} {
    if [string match {} ${chan}] {
	global bindings
	lappend bindings [list $sequence $action]
    } {
	set chn [Channel :: make ${chan}]
	set b [$chn bindings]
	lappend b [list $sequence $action]
	$chn configure -bindings $b
    }
}
#
proc InitGlobals {} {
    global env user host argv zircon
#    if [file exist $zircon(lib)/zircon.ad] {
#	option readfile $zircon(lib)/zircon.ad startupFile
#    }
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
    set zircon(envnick) IRCNICK
    set zircon(envname) IRCNAME
    set zircon(envserver) IRCSERVER
    set zircon(register) [file exist ~/.zirconreg]
    set zircon(command) 0
    set zircon(style) {original}
    set zircon(language) {english}
    set zircon(autos) {join open close menu draw jump quiet}
    foreach arg { nameCount idle j o z } { set zircon($arg) 0 }
    foreach arg { host i N s p} { set zircon($arg) {} }
    getOption showFriends 0
    getOption friendsStyle window
    getOption smiley {:-)}
    getOption scowl {:-(}
    getOption wink {;-)}
    getOption listPattern {.*}
    getOption topicPattern {.*}
    getOption minMembers 3
    foreach arg {noPopup popInfo \
      invisible wallops srvmsg showPrivate topicOnly DEBUG monitorIn \
      monitorOut verboseCTCP } {
	getOption $arg 0
    }
    foreach arg { noRefresh friendsOn killPath showLocal \
      showPublic} {
	getOption $arg 1
    }
    foreach arg { ons bindings nicks ircnames actions noConfirm aways \
      toInfo ignores} {
	getOption $arg {}
    }
    global monitor ; set monitor {}
# control panel
    global namesTxt namesChan monitorTime notifyInterval \
      testTime trust notify
    set notify {}
    set namesTxt {}
    set namesChan {}
    getOption monitorTime 60
    set monitorTime [expr $monitorTime * 1000]
    set notifyInterval 30000
    set testTime $notifyInterval
# Messages
    getOption signoff {I run Zircon - so should you}
    getOption helpService {Help_UK}
    set trust(eval) {}
    set trust(draw) {.+}
    Service NickServ -host service.de -ops \
      {ACCESS BUG HELP IDENTIFY INFO INTRO LIST NEWS OPERWHO \
      PASSWD REGISTER STATS TIMEZONE UNREG WHOIS}
    Service NoteServ -host service.de \
      -ops {AWAY DELETE IDENTIFY INFO INTRO LIST MSG QUERY READ WAITFOR}
    global defChan defChat defMsg defNotice
    set defChan [Channel *default* -height 24 -width 80]
    set defChat [Chat *default* -height 10 -width 80]
    set defMsg [Message *default* -height 10 -width 80]
    set defNotice [Notice *default* -height 10 -width 80]
#
# Array variables
#
    makeArray TFg TBg TAF TAB TSplit Heal Split MkOp MTO NTO \
      OnCode Host Nick STO AChat Chat ASend Send Get Shost CTO CHTO
# Process args
    set opts {}
    foreach arg $argv {
	if [string match {-*} $arg] {
	    foreach bit [split [string range $arg 1 end]] {
		switch $bit {
		j -
		o -
		z { set zircon($bit) 1 }
		i -
		N -
		s -
		p { append opts $bit}
		default { puts stderr "Unknown option -$bit" }
		}
	    }
	} {
	    set opt [lindex $opts 0]
	    set opts [lrange $opts 1 end]
	    switch $opt {
	    {i N s p} { set zircon(arg) $arg }
	    default { }
	    }
	}
    }
#
# Source the system and then the user's rc file if they exist. The -z
# flag turns off reading rc files.
#
    if !$zircon(z) {
	if [file exist $zircon(lib)/rc] {
	    if [catch {uplevel #0 source $zircon(lib)/rc} msg] {
		puts stderr "**** Error in the system rc file!!! - $msg"
		exit
	    }
	    global defaultServer defaultPort
	    if {[info exists defaultServer] || [info exists defaultPort]} {
		Server default -host $defaultServer -port $defaultPort
		Server :: select $defaultServer
		upgradeWarn
	    }
	}
	if [file exist ~/.zirconrc] {
	    if [catch {uplevel #0 source ~/.zirconrc} msg] {
		puts stderr "**** Error in your .zirconrc file!!! - $msg"
		exit
	    }
	}
    }
    global hostIPAddress nicks myid
    set host \
      [expr {[info exists env(HOSTNAME)] ? $env(HOSTNAME) : [exec hostname]}]
    set a1 [dp_address create $host 0]
    set hostIPAddress [lindex [dp_address info $a1] 0]
    dp_address delete $a1

    set user [expr {[info exists env(USER)] ? $env(USER) : [exec whoami]}]
    envCheck $zircon(N) $zircon(envnick) nicks $user
    envCheck $zircon(i) $zircon(envname) ircnames $user@$host
    if {![string match {} $zircon(s)] || \
      [info exists env($zircon(envserver))]} {
	set v [expr \
	  {![info exists env($zircon(envserver))] ? $zircon(s) : \
	    $env($zircon(envserver))}]
	Server :: make $v
	Server :: select $v
    }
    if [string match {} $zircon(host)] { Server :: select default }
    if {![string match {} $zircon(p)] || [info exists env(IRCPORT)]} {
	set v [expr ![string match {} $zircon(p)] ? $zircon(p) : $env(IRCPORT)]
    }
#
# Re-Initialise things in case they were set in the rc file....
#
    set zircon(version) {1.15.T}
    set zircon(windows) 0
#
# Configuration panel stuff
#
    global cVars
    set cVars(IRC) {nicks ircnames}
    set cVars(Channels) {}
    set cVars(People) {ignores showFriends friendsOn}
    set cVars(Info) {showLocal showPublic showPrivate topicOnly minMembers \
      signoff noConfirm toInfo popInfo verboseCTCP helpService \
      noPopup noRefresh killPath listPattern topicPattern }
    set cVars(Others) {aways actions invisible wallops srvmsg}
#
    global confData friendsOn
    set confData(channel) {{{Auto Join} join} {{Pop Up} open} \
      {{Pop Down} close} {{On Menu} menu} {Draw draw} \
      {Jump jump} {Quiet quiet}}
    set confData(single) {showLocal showPublic showPrivate \
      topicOnly minMembers signoff popInfo verboseCTCP helpService \
      invisible wallops srvmsg noRefresh noPopup friendsOn killPath \
      listPattern topicPattern showFriends }
    set confData(msg) {Join Kick Kill Leave Mode Quit Topic}
    set confData(info) {Ctcp Signoff Who Whois Whowas Error Ison Info}
    set confData(nconf) {Quit Leave Kill SaveConf}
#
# Check to see if upgrade needed
#
    upgradeTest
#
#
#
    global away busy sock startup allChannels confChange closeTime
    set closeTime [expr [$defChan closetime] * 1000]
    set away 0
    set busy 0
    set sock {}
    set startup 1
    set allChannels {}
    set confChange 0
#
#	1 if user is an IRC operator
#
    set zircon(ircop) 0
#
# Some command information
#
    global Ops
    set Ops(server) {Links Version Motd Time Trace Admin Lusers Info Stats \
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
# Create the User object for me!!
#
    set myid [User [lindex $nicks 0]]
    $myid ref
#
# Flag channels that are created in the rc file. This makes sure they
# dont get thrown away when the channel is closed
#
    foreach chn [Channel :: list] { $chn configure -keep 1 }
    foreach chn [Message :: list] { $chn configure -keep 1 }
    setupUsers
    processOns
}
#
proc notIdle {win} {
    if ![string match {} $win] {
	if ![string match {nil} [set chn [channel $win]]] {
	    $chn extendTime
	}
    }
    global zircon
    set zircon(idle) 0
}
#
proc doScroll {win where} { $win yview $where }
#
proc setScroll {txt win total window first last} {
    global Chistory
    set id [channel $win]
    if {$total > $Chistory($id)} {
	incr total -$Chistory($id)
	$txt delete 1.0 "1.0 + $total lines"
	set total $Chistory($id)
    }
    $win set $total $window $first $last
}
#
# alter menu item states - used for oping and ircoping
#
proc setState {name pick state} {
    if ![string match {none} [$name index last]] {
	global Ops
	foreach cmd $Ops($pick) {
	    $name entryconfigure [$name index $cmd] -state $state
	}
    }
}
#
proc makeMB {win text} {
    menubutton $win -text $text -menu $win.menu
    return [menu $win.menu]
}
#
proc keepAway {value} {
    doAway "$value"
    .oFrm.bf1.away.menu add command -label "[prune $value 15]" \
      -command "doAway {$value}"
    global aways confChange
    lappend aways $value
    set confChange 1
}
#
proc getAway {} {
    mkEntryBox .@away {Away Message} {Enter your away message:} \
      {{Away {}}} \
      {OK doAway} {Keep keepAway} {Back {doAway {}}} {Cancel {}}
}
#
proc getOValue {win opt lc uc} {
    if [string match {} [set x [option get ${win} $lc $uc]]] {
	set x [lindex [${win} conf -$opt] 4]
    }
    return $x
}
#
proc getTValue {win win2 opt lc uc} {
    if [string match {} [set x [option get ${win} $lc $uc]]] {
	set x [lindex [${win2} conf -$opt] 4]
    }
    return $x
}
#
proc doHelp {topic service} {
    if ![string match {} $service] {sendIRC PRIVMSG $service $topic}
}
#
proc getHelp {} {
    global helpService
    mkEntryBox .@help "Help" "Enter topic on which you need help:" \
      "{Topic {zircon ?}} {Service $helpService}" \
      {OK {doHelp}} {Cancel {}}
}
#
proc setFlag {flag} {
    global $flag myid
    $myid mode [expr {[set $flag] ? {+} : {-}}][string index $flag 0]
}
#
proc pickvar {val v1 v2} { return [expr {$val != {} ? $v1 : $v2}] }
#
proc setTags {this nk} {
    global TFn TFa TFg TBg TAF TAB
    set w [$this text]
    set x "${this},${nk}"
    set ch [$this tagWindow]
    set TFn($x) [getTValue $ch $w font ${nk}Font Font]
    set TFg($x) [getTValue $ch $w foreground ${nk}Foreground Foreground]
    set TBg($x) [getTValue $ch $w background ${nk}Background Background]
    set TFa($x) [getTValue $ch $w font ${nk}ActionFont Font]
    set TAF($x) [getTValue $ch $w foreground ${nk}ActionForeground Foreground]
    set TAB($x) [getTValue $ch $w background ${nk}ActionBackground Background]
}
#
proc insertText {this name text tagInfo} {
    set pattern *\[\002\026\037\007\]*
    if ![string match $pattern $text] {
	$name insert end $text
	set end [$name index end]
    } {
	set tag [lindex $tagInfo 0]
	set bold {}
	set boldEnd {}
	set uline {}
	set ulEnd {}
	set invert {}
	set invEnd {}
	set ends {}
	foreach ch [split $text {}] {
	    case $ch {
	    \002 { set [pickvar $bold boldEnd bold] [$name index end] }
	    \017 {
		    set en [$name index end]		
		    if ![string match {} $bold] { set boldEnd $en }
		    if ![string match {} $invert] { set invEnd $en }
		    if ![string match {} $uline] { set ulEnd $en }
		}
	    \026 { set [pickvar $invert invEnd  invert] [$name index end] }
	    \037 { set [pickvar $uline ulEnd uline] [$name index end] }
	    \007 {
		    global Bg
		    if ![$this quiet] {
			puts -nonewline stdout "\007" ; flush stdout
		    }
		    $name insert end { }
		    set bp [$name index end]
		    $name insert end BEEP
		    set bpe [$name index end]
		    $name insert end { }
		    set bg [expr {[string match {} $tag] ? \
		      $Bg($this) : [lindex $tagInfo 2]}]
		    $name tag add @beep${tag} $bp $bpe
		    $name tag configure @beep${tag} -background $bg \
		      -borderwidth 2 -relief raised
		}
	    default {
		    $name insert end "$ch"
		    if ![string match {} $boldEnd] {
			global BF
			if ![string match {} $BF($this)] {
			    $name tag add @b${tag} $bold $boldEnd
			    $name tag configure @b${tag} -font $BF($this)
			}
			set bold {}
			set boldEnd {}
		    }
		    if ![string match {} $ulEnd] {
			$name tag add @u${tag} $uline $ulEnd
			$name tag configure @u${tag} -underline 1
			set uline {}
			set ulEnd {}
		    }
		    if ![string match {} $invEnd] {
			global Fg Bg
			set fg [expr {[string match {} $tag] ? \
			  $Fg($this) : [lindex $tagInfo 1] }]
			set bg [expr {[string match {} $tag] ? \
			  $Bg($this) : [lindex $tagInfo 2]}]
			$name tag add @v${tag} $invert $invEnd
			$name tag configure @v${tag} \
			  -foreground $bg -background $fg
			set invert {}
			set invEnd {}
		    }
		}
	    }
	}
	set end [$name index end]
	if ![string match {} $bold] {
	    global BF
	    if ![string match {} $BF($this)] {
		$name insert end { }
		$name tag add @b${tag} $bold $end
		$name tag configure @b${tag} -font $BF($this)
	    }
	}
	if ![string match {} $uline] {
	    $name insert end { }
	    $name tag add @u${tag} $uline $end
	    $name tag configure @u${tag} -underline 1
	}
	if ![string match {} $invert] {
	    global Fg Bg
	    set fg [expr \
	      {[string match {} $tag] ? $Fg($this) : [lindex $tagInfo 1]}]
	    set bg [expr \
	      {[string match {} $tag] ? $Bg($this) : [lindex $tagInfo 2]}]
	    $name insert end { }
	    $name tag add @v${tag} $invert $end
	    $name tag configure @v${tag} -foreground $bg -background $fg
	}
    }
    return $end
}
#
# Send strings to the server
#
proc sendIRC {op args} {
    global monitorOut sock
    if ![string match {} $sock] {
	set msg $op
	set last {}
	foreach arg $args  {
	    if ![string match {} $arg] {
		if ![string match {} $last] { append msg " $last" }
		set last $arg
	    }
	}
	if ![string match {} $last] { append msg " :$last" }
	if [catch [list puts $sock "$msg\r\n"]] {
	    closeIRC $sock
	} \
	elseif $monitorOut { puts stdout >$msg }
    }
}
#
proc setMode {chan mode args} { sendIRC MODE $chan $mode [lindex $args 0] }
#
#	Sets or clears the Away message at the server
#
proc doAway {args} { sendIRC AWAY [join $args] }
#
proc doBRB {args} {
    global away
    if {$away == 0} {
	.oFrm.bf1.brb conf -text Back
	foreach id [Channel :: list] {
	    if [$id active] { $id send brb -nopop }
	}
	doAway {Back soon.}
	set away 1
    } {
	.oFrm.bf1.brb conf -text BRB
	foreach id [Channel :: list] {
	    if [$id active] { $id send back -nopop }
	}
	doAway {}
	set away 0
    }
    invert .oFrm.bf1.away
}
#
#	channel Operations
#
proc channelInvite {chan args} {
    if ![string match {} ${chan}] { userInvite ${chan} }
}
#
proc doNotice {chan string} {
    if ![string match {} $string] {
	if {[string match {nil} [set cn [Channel :: find ${chan}]]] ||
	    ![$cn active]} {
	    net0 display @me "${chan}>- $string"
	} {
	    $cn addText @me "- $string"
	}
	sendIRC NOTICE ${chan} $string
    }
}
#
proc channelNotice {chan args} {
    if ![string match {} ${chan}] {
	mkEntryBox .@[newName wn] "Notice to ${chan}" \
	  {Enter your notice text:} \
	  {{Notice {}}} "OK {doNotice {${chan}}}" {Cancel {}}
    }
}	
#
proc channelJoin {chan args} {
    [Channel :: make ${chan}] sendJoin [lindex $args 0]
}
#
proc channelMonitor chan {
    if {![string match {} ${chan}]} {
	set chid [Channel :: make ${chan}]
	if ![$chid active] {
	    global monitor
	    set chan [$chid lname]
	    if {[lsearch $monitor ${chan}] < 0} { lappend monitor ${chan} }
	    $chid configure -monitor 1
	    sendIRC NAMES ${chan}
	}
    }
}
#
proc channelWho chan {
    if ![string match {} $chan] { sendIRC WHO ${chan} }
}
#
proc channelNames {chan} { sendIRC NAMES ${chan} }
#
proc channelList doit {
    global allChannels showList
    set allChannels {}
    if ![winfo exists .@list] {
	global showLocal showPublic showPrivate minMembers topicOnly
	toplevel .@list -class Zircon
	wm title .@list {IRC Channel List}
	wm iconname .@list {IRC Channel List}
	wm minsize .@list 10 1
	set w .@list
	frame $w.filter -relief raised
	checkbutton $w.filter.public -variable showPublic -text Public
	checkbutton $w.filter.local -variable showLocal -text Local
	checkbutton $w.filter.private -variable showPrivate -text Private
	checkbutton $w.filter.topic -variable topicOnly -text {With Topic}

	set tmp $minMembers
	scale $w.filter.members \
	  -from 1 -to 25 -label {Minimum Number of Members} \
	  -showvalue 1 -orient horizontal \
	  -command {global minMembers ; set minMembers }

	$w.filter.members set $tmp

	pack $w.filter.members -fill x
	pack $w.filter.public $w.filter.local $w.filter.private \
	  $w.filter.topic -side left -fill x
	global listPattern topicPattern
	labelEntry 0 $w.filter2 {-text Channel} $listPattern {}
	labelEntry 0 $w.filter3 {-text Topic} $listPattern {}

	makeLB $w.chn -geometry 20x8 -setgrid 1
	frame $w.btn
	button $w.btn.ok -text OK -command {destroy .@list} -relief raised
	button $w.btn.clear -text Clear -relief raised \
	  -command {.@list.chn.l delete 0 end ; set allChannels {}}
	button $w.btn.list -text List -relief raised \
	  -command {
	    .@list.chn.l delete 0 end
	    sendIRC LIST ;
	    set allChannels {}
	  }
	pack $w.btn.list $w.btn.clear $w.btn.ok -side left -expand 1 -fill x
	pack $w.filter $w.filter2 $w.filter3 -fill x
	pack $w.chn -expand 1 -fill both
	pack $w.btn -fill x
	bind $w.chn.l <Double-Button-1> {
	    channelJoin [lindex $allChannels [%W nearest %y]] {}
	}
	bind $w.chn.l <Double-Button-2> {
	    whoAction [lindex $allChannels [%W nearest %y]]
	}
	bind $w.chn.l <Button-1> {
	    entrySet .oFrm.cmdLine.channel \
	      [lindex $allChannels [%W nearest %y]]
	}
    } {
	popup .@list
	if ![string match {} $doit] {.@list.chn.l delete 0 end}
    }
    set showList 0
    if ![string match {} $doit] { sendIRC LIST $doit ; set showList 1 }
}
#
proc popup {win} { wm deiconify $win ; raise $win }
#
proc mungNotice {msg} {
    if [string match {*Received KILL message*} $msg] {
	regsub {Path:.*\(} $msg {Path: ... (} msg
    }
    return $msg
}
#
proc doWhois {nk where} {
    if ![string match {} ${nk}] { sendIRC WHOIS $where ${nk} }
}
#
proc markButton {name which} {
    if ![winfo exists $name] { return }
    foreach opt {font foreground background activeForeground \
      activeBackground} {
	set uopt [capitalise $opt]
	set fopt ${which}[expr {$which != {} ? $uopt : $opt}]
	set lopt [string tolower $opt]
	if {[set cl [option get $name $fopt $uopt]] != {}} {
	    $name conf -$lopt $cl
	} \
	elseif [string match {} $which] {
	    if ![string match {} [set cl [lindex [$name conf -$lopt] 3]]] {
		$name conf -$lopt $cl
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
#
proc markEntry {name index which} {
    if ![winfo exists $name] { return }
    foreach opt {font background activeBackground} {
	set uopt [capitalise $opt]
	set fopt ${which}[expr {$which != {} ? $uopt : $opt}]
	set lopt [string tolower $opt]
	if {[set cl [option get $name $fopt $uopt]] != {}} {
	    $name entryconfigure $index -$lopt $cl
	} \
	elseif [string match {} $which] {
	    if ![string match {} [set cl [lindex [$name conf -$lopt] 3]]] {
		$name entryconfigure $index -$lopt $cl
	    }
	}
    }
#    set af [lindex [$name conf -activeforeground] 4]
#    set fg [lindex [$name conf -foreground] 4]
#    if {$af == $fg} {
#	set bg [lindex [$name conf -background] 4]
#	$name entryconfigure $index -activeforeground $bg -activebackground $fg
#    }
}
#
proc unsetUser {md} {
    case $md {
    [oO] { unmakeIRCOp 0 }
    w	 { global wallops ; set wallops 0 }
    s	 { global srvmsg ; set srvmsg 0 }
    i	 { global invisible ; set invisible 0 }
    }
}
#
proc setUser {md} {
    case $md {
    [Oo] { makeIRCOp }
    w	 { global wallops ; set wallops 1 }
    s	 { global srvmsg ; set srvmsg 1 }
    i	 { global invisible ; set invisible 1 }
    }
}
#
# First message from the server....
#
proc irc001 {net prefix param pargs} {
    global startup invisible wallops srvmsg myid zircon defMsg defChan
    flagControl normal
    deIRCOp
    set me [$myid name]
    set opStuff [$zircon(host) oper]
    if ![string match {} [set nk [lindex $opStuff 0]]] {
	if [string match {} [set pw [lindex $opStuff 1]]] {
	    mkEntryBox .@operpw {IRC Op Password} \
	      {Enter your operator password:} {{Password {}}} \
	      "OK { doOper $nk }" {Cancel {}}
	} {
	    sendIRC OPER $nk $pw
	}
    }
    if $invisible { $myid mode +i }
    if $wallops	  { $myid mode +w }
    if $srvmsg	  { $myid mode +s }
    if !$zircon(j) {
	foreach id [Channel :: list] {
	    if {$id == $defChan} { continue }
	    if {[$id join] || [$id active]} {
		$id show
		$id flag normal
		$id unmarkV $myid
		$id unmarkOp $myid
		set ch [$id name]
		$id sendJoin {}
		sendIRC MODE $ch
	    } \
	    elseif [$id monitor] { channelMonitor [$id name] }
	}
	foreach id [Message :: list] {
	    if {$id == $defMsg} { continue }
	    $id show
	    $id flag normal
	}
    }
    $net display {} "*** $param"
    setupTests
    if $zircon(register) {
	sendIRC PRIVMSG ZirconBot "!zstartup $zircon(version)"
	set zircon(register) 0
    }
    handleOn STARTUP [list [$zircon(host) host] [$zircon(host) port]]
    set startup 0
}
#
proc irc004 {net prefix param pargs} {
    global serverInfo
    set serverInfo [lrange $pargs 1 4]
    $net display {} \
      "[string range $prefix 1 end]: umodes available [lindex $serverInfo 2],\
channel modes available [lindex $serverInfo 3]"
}
#
proc setNickname {nk} {
    global myid
    set nk [string range $nk 0 8]
    if {[$myid name] != $nk} {
	entrySet .oFrm.nSFrm.nickname.entry $nk
	foreach id [Channel :: list] { $id nickChange $myid $nk }
	$myid rename $nk
    }
}
#
proc deIRCOp {} { global myid ; unmakeIRCOp 1 ; $myid mode -O }
#
proc irc381 {net prefix param pargs} {
    makeIRCOp
    [$net info] addText {} "*** $param"
}
#
proc irc301 {net prefix param pargs} {
    global whois
    if [info exists whois(info)] {
	set whois(away) $param
    } {
	switch [set x [Message :: find [set who [lindex $pargs 1]]]] {
	nil { set chn [$net info] }
	default { set chn $x }
	}
	$chn addText {} "*** $who is away: $param"
    }
}
#
proc irc303 {net prefix param pargs} {
    global friendsOn
    set signons {}
    set signoffs {}
    set msg {}
    set lpar {}
    foreach who $param {
	set usr [User :: make $who]
	lappend lpar $usr
	if {![$usr ison] || [$usr inLimbo]} {
	    $usr on
	    if [friends absent $usr] {
		$usr heal
	    } {
		lappend signons $who
		if {$friendsOn && [$usr isFriend]} { friends add $usr }
		friends mark $usr ison
	    }
	}
    }
    if ![string match {} $signons] {
	set msg "Signon by $signons detected.\n"
    }
    foreach usr [User :: list] {
	if {[$usr ison] && ![$usr inLimbo] && [lsearch $lpar $usr] < 0} {
	    $usr off
	    lappend signoffs [$usr name]
	}
    }
    if ![string match {} $signoffs] {
	set msg "${msg}Signoff by $signoffs detected.\n"
    }
    if ![string match {} $msg] {
	mkInfoBox ISON .@ison Notify "[exec date] :\n$msg" {OK {}}
    }
}
#
proc irc305 {net prefix param pargs} {
    global away
    if $away {invert .oFrm.bf1.away}
    set away 0
}
#
proc irc306 {net prefix param pargs} {
    global away
    if !$away {invert .oFrm.bf1.away}
    set away 1
}
#
proc irc321 {net prefix param pargs} {}

proc irc322 {net prefix param pargs} {
    if ![winfo exists .@list] return
    global showList allChannels showPublic showLocal showPrivate \
      minMembers topicOnly listPattern topicPattern
    set chan [lindex $pargs 1]
    if [string match {} [set listPattern [.@list.filter2.entry get]]] {
	set listPattern {.*}
    }
    if [string match {} [set topicPattern [.@list.filter3.entry get]]] {
	set topicPattern {.*}
    }
    if !$showList {
	switch -glob ${chan} {
	{\*}  { if !$showPrivate { return } {set chan Prv } }
	&*  { if !$showLocal   { return } }
	#*  { if !$showPublic  { return } }
	}
    }
    set memb [lindex $pargs 2]
    if {$showList  || ((![string match {} $param] || !$topicOnly) && \
      $memb >= $minMembers && [regexp -nocase $listPattern ${chan}] && \
      [regexp $topicPattern $param])} {
	lappend allChannels ${chan}
	.@list.chn.l insert end \
	  "[format {%-9s %3d %s} [string range ${chan} 0 8] $memb $param]"
    }
}
#
proc irc323 {net prefix param pargs} {global showList ; set showList 0}
#
proc irc324 {net prefix param pargs} {
    [Channel :: find [lindex $pargs 1]] mode [lrange $pargs 2 end]
}
#
proc irc353 {net prefix param pargs} {
    global myid
    set chan [string tolower [lindex $pargs 2]]
    set chid [Channel :: make ${chan}]
    if ![$chid active] {
	global namesTxt namesChan
	if {${namesChan} != ${chid}} {
	    set namesChan ${chid}
	    set namesTxt $param
	} {
	    append namesTxt "\n$param"
	}
	return
    }
    foreach n $param {
	set op 0
	set sp 0
	while {[string match {[@+]*} $n]} {
	    if [string match {@} [string index "$n" 0]] \
	      { set op 1 } { set sp 1}
	    set n [string range $n 1 end] ;
	}
	set usr [User :: make $n]
	if {$usr == $myid} {
	    if $op { $chid markOp $usr } \
	    elseif $sp { $chid markV $usr }
	} {
	    $chid addUser $usr $op $sp
	}
    }
}
#
proc updateMon {chid names} {
    set w .@mon${chid}
    if ![winfo exists $w] {
	makeMon ${chid} $names
	return
    }
    set win $w.users.userList
    set winf $win.frame
    set xist {}
    foreach n [winfo children $winf] {
	set l [split $n .]
	set x [expr {[llength $l] - 1}]
	lappend xist [lrange $l $x $x]
    }
    foreach n $names {
	if [string match {} $n] continue
	set op 0
	set sp 0
	while { [string match {[@+]*} $n] } {
	    if {[string index "$n" 0] == {@}} { set op 1 } { set sp 1}
	    set n [string range $n 1 end] ;
	}
	set nm $n
	set n [string tolower $n]
	if ![winfo exists ${winf}.$n] {
	    menubutton $winf.$n -text $nm -menu $winf.$n.menu
	    makeUserMenu nil $winf.$n.menu $n
	    set wd [winfo reqwidth $winf.$n]
	    set sht [set ht [winfo reqheight $winf.$n]]
	    set ht [expr { $ht * [llength [winfo children $winf]]}]
	    $win conf \
	      -width $wd -scrollregion [list 0 0 $wd $ht] -scrollincrement $sht
	    $win conf -width $wd -height $ht
	    pack ${winf}.$n
	} \
	elseif {[set x [lsearch $xist $n]] >= 0} {listdel xist $x}
	if $op {
	    markButton $winf.$n operator
	} {
	    markButton $winf.$n [expr {$sp ? "speaker" : {} }]
	}
    }
    foreach n $xist { destroy $winf.$n }
}
#
proc makeMon {chid names} {
    set w .@mon${chid}
    toplevel $w -class Zircon
    set chan [$chid name]
    wm title $w "${chan} Monitor"
    wm grid $w 10 10 10 10
    wm minsize $w 10 1

    pack [frame $w.btns -relief raised] -side bottom -fill x
    pack [set wu [frame $w.users -relief raised]] -fill y
    scrollbar $wu.vscroller -command "$wu.userList yview" 
    set win [canvas $wu.userList -yscrollcommand "$wu.vscroller set"]
    frame $wu.userList.frame -border 0
    $wu.userList create window 0 0 -window $wu.userList.frame -anchor nw
    pack $wu.userList $wu.vscroller -side left -fill y
    button $w.btns.cancel -text Cancel -command " deMonitor $w ${chan} "
    button $w.btns.join -text Join -command " $chid sendJoin {} "
    bind $wu <Destroy> "deMonitor {} ${chan}"
    pack $w.btns.cancel $w.btns.join -side left -fill x -expand 1
    set winf $wu.userList.frame
    foreach n $names {
	if [string match {} $n] { continue }
	set op 0
	set sp 0
	while { [string match {[@+]*} $n] } {
	    if {[string index $n 0] == {@}} { set op 1 } { set sp 1}
	    set n [string range $n 1 end]
	}
	set usr [User :: make $n]
	set lnm [$usr lname]
	menubutton $winf.$lnm -text $n -menu $winf.$lnm.menu
	makeUserMenu nil $winf.$lnm.menu $usr
	if $op { markButton $winf.$lnm operator	} \
	elseif $sp { markButton $winf.$lnm speaker }
	set wd [winfo reqwidth $winf.$lnm]
	set sht [set ht [winfo reqheight $winf.$lnm]]
	set ht [expr { $ht * [llength [winfo children $winf]]}]
	$win conf \
	  -width $wd -scrollregion [list 0 0 $wd $ht] -scrollincrement $sht
	$win conf -width $wd -height $ht
	pack $winf.$lnm
    }
}
#
proc irc366 {net prefix param pargs} {
    set chan [string tolower [lindex $pargs 1]]
    set chid [Channel :: make ${chan}]
    global namesChan namesTxt
    if {${namesChan} == ${chid}} {
	global monitor
	if {[lsearch $monitor [$chid lname]] >= 0 } {
	    updateMon ${chid} [split $namesTxt]
	} {
	    mkInfoBox NAMES .@names${chid} "Names ${chan}" $namesTxt {OK {}}
	}
    }
    set namesChan {}
    set namesTxt {}
}
#
proc irc376 {net prefix param pargs} {}
#
proc irc394 {net prefix param pargs} {}
#
proc irc251 {net prefix param pargs} {
    $net display {} "[string range $prefix 1 end]: $param]"
}
#
proc irc252 {net prefix param pargs} {
    set nm [lindex "$pargs" 1]
    $net display {} \
      "[string range $prefix 1 end]: There [expr {$nm == 1 ? {is 1 operator} :
      "are $nm operators"}] online."
}
#
proc irc253 {net prefix param pargs} {
    set nm [lindex "$pargs" 1]
    $net display {} "[string range $prefix 1 end]: There\
[expr {$nm == 1 ? {is 1 unknown connection} : "are $nm connections"}]."
}
#
proc irc254 {net prefix param pargs} {
    set nm [lindex "$pargs" 1]
    $net display {} \
      "[string range $prefix 1 end]: There [expr {$nm == 1 ? {is 1 channel} :
      "are $nm channels"}] formed."
}
#
proc irc255 {net prefix param pargs} {
    $net display {} "[string range $prefix 1 end]: $param"
}
#
proc parseMsg {net msg} {
    if ![regexp {^([^ ]*) ([^ ]*)(( ([^:][^ ]*))*)( :(.*))?$} $msg \
      match prefix cmd b c d e param] {
	if ![regexp {^([^ ]*) ([^ ]*)(.*)$} $msg  match prefix cmd b] {
	    return
	}
	set param {}
    }
    switch -glob $prefix {
    :*		{ }
    PING	{ sendIRC PONG [string range $msg 5 end] ; return }
    default	{ global zircon ; set prefix :[$zircon(host) host] }
    }
    set msg [string range $b 1 end]
    if {[string match {} [info procs "irc$cmd"]] && ![auto_load "irc$cmd"]} {
	ircNUM $net $cmd $prefix $param $msg
    } {
	irc$cmd $net $prefix $param $msg
    }
    update idletasks
}
#
proc closeIRC {conn} {
    global sock away zircon
    catch "dp_filehandler $conn"
    catch "close $conn"
    if {$sock == $conn} { set sock {} }
    flagControl disabled
    foreach id [Channel :: list] { $id flag disabled }
    if $away { invert .oFrm.bf1.away }
    set away 0
    mkDialog {} .@close Shutdown \
      "Server [$zircon(host) host] has closed the connection." {} {OK {}}
}
#
proc ircInput {mode conn} {
    case $mode in {
    r   {
	    global monitorIn Net
	    if {[catch {gets $conn} buffer] || \
	      ([string match {} $buffer] && [eof $conn])} {
		closeIRC $conn
	    } {
		if $monitorIn { puts stdout <$buffer }
		regexp "^\[^\r\]*" $buffer buffer
		parseMsg $Net($conn) $buffer
	    }
	}
    e   {
	    global mainInfo
	    $mainInfo addText {} "*** Error on server connection"
	}
    }
}
#
proc changeIRCName {name} {
    global sock ircname
    if {$sock != {}} {
	mkDialog {} .@warn Warning \
	  "Change will not take effect until next server change." {} "OK {}"
    }
    set ircname $name
}
#
proc changeNickname {name} {
    global startup sock
    if $startup {
	setNickname $name
    } {
	entrySet .oFrm.nSFrm.nickname.entry $name
    }
    if ![string match {} $sock] { sendIRC NICK $name }
}
#
proc doQuit {msg} {
    global sock confChange
    if ![string match {} $sock] {
	sendIRC QUIT $msg
	catch {dp_shutdown $sock all}
	catch {close $sock}
	set sock {}
    }
    if $confChange {
	set w .@[newName Save]
	mkDialog SAVECONF $w {Save Configuration} \
	  {You have made changes to your configuration. Do you wish to \
save them?} {} {No exit} {Yes {saverc}}
	tkwait window $w
    }
    exit
}
#
proc quitZircon {} {
    global signoff
    mkDialog QUIT .@quit "Quit IRC" \
      {Really quit?} "{Message {$signoff}}" {OK doQuit} {Cancel {}}
}
#
proc startIRC {srv} {
    if [string match {} $srv] { return 0 }
    global sock myid ircname host user noRefresh Icon Net
    set port [$srv port]
    set server [$srv host]
    net0 display {} "*** Connecting to port $port of server $server"
    update idletasks
    if [string match {} $port] {
	if [catch {dp_connect $server} val] {
	    net0 display {} \
	"*** Cannot connect to UNIX domain server $server ($val)"
	    return 0
	}
    } \
    elseif [catch {dp_connect $server $port} val] {
	net0 display {} "*** Cannot connect to server $server ($val)"
	return 0
    }
    set sock [lindex $val 0]
    set Net($sock) net0
    foreach ln [$srv script] { puts $sock "$ln\n" }
    dp_filehandler $sock re ircInput
    dp_socketOption $sock recvBuffer 8192
    sendIRC USER $user $host $server $ircname
    sendIRC NICK [$myid name]
    if !$noRefresh { channelList { } }
    set ircport $port
    set w [[net0 info] window]
    wm title $w "Zircon Information Window - $server" 
    wm iconname $w [set Icon($w) "Info $server"]
    wm iconname . "Control $server"
    return 1
}
#
proc envCheck {arg evar gvar dflt} {
    global env $gvar
    set lst [set $gvar]
    if {![string match {} $arg] || [info exists env($evar)]} {
	set v [expr {[string match {} $arg] ? $env($evar) : $arg}]
	if {[set x [lsearch $lst $v]] > 0} { listdel $gvar $x }
	if {$x != 0} { set $gvar [linsert $lst 0 $v] }
    } \
    elseif [string match {} $lst] { set $gvar $dflt }
}
#
proc setupTests {} {
    global closeTime testTime notifyLeft notifyInterval monitorLeft monitorTime
    set notifyLeft $notifyInterval
    set monitorLeft $monitorTime
    set testTime $notifyInterval
    if {$closeTime > 0 && $closeTime < $notifyInterval} {
	set testTime $closeTime
    }
    sendISON
    after $testTime ircTests
}
#
proc setupUsers {} {
    global friendsOn
    foreach usr [User :: friends] {
	if $friendsOn { $usr notify 1 }
	if {!$friendsOn || [$usr ison]} { friends add $usr }
   }
}
#
proc sendISON {} {
    global notify sock
    if {$notify != {} && $sock != {}} { sendIRC ISON [join $notify " "] }
}
#
proc cleanSplit h {
    global Split Heal
    if [info exists Split($h)] {
	global TSplit
	foreach user $Split($h) {
	    if ![string match {nil} [set msg [Message :: find $user]]] {
		$msg flag normal
	    }
	    foreach id [Channel :: list] {
		if {[winfo exist [$id window].cFrm.uFrm.userBtn.frame.$user] &&
		  ![normal [$id window].cFrm.uFrm.userBtn.frame.$user]} {
		    $id killUser ${user}
		}
	    }
	    friends remove $user
	}
	unset Split($h) TSplit($h)
    }
    catch {unset Heal($h)}
}
#
proc ircTests {} {
    global testTime closeTime notifyLeft TSplit Heal zircon monitorLeft MkOp
    if {$closeTime > 0} {
	foreach id [Channel :: list] { $id inactive }
	foreach id [Chat :: list] { $id inactive }
	foreach id [Message :: list] { $id inactive }
	foreach id [Notice :: list] { $id inactive }
	foreach id [Info :: list] { $id inactive }
    }
    if {[incr notifyLeft -$testTime] <= 0} {
	sendISON
	global notifyInterval ; set notifyLeft $notifyInterval
    }
    foreach h [array names Heal] {
	if {[incr Heal($h) -$testTime] <= 0} { cleanSplit $h }
    }
    foreach h [array names TSplit] {
	if {[incr TSplit($h) -$testTime] <= 0} { cleanSplit $h }
    }
    incr zircon(idle) [expr {$testTime / 1000}]
    if {[incr monitorLeft -$testTime] <= 0} {
	global monitor monitorTime
	if ![string match {} $monitor] {
	    set ch [join [split $monitor] ,]
	    sendIRC NAMES $ch
	}
	set monitorLeft $monitorTime
    }
    foreach id [array names MkOp] {
	if [$id operator] {
	    set flag +
	    set who {}
	    global ${id}Op
	    foreach n $MkOp($id) {
		if {[info exists ${id}Op($n)] && ![set ${id}Op($n)]} {
		    append flag o
		    lappend who [$n name]
		}
	    }
	    if ![string match {} $who] { sendIRC MODE [$id name] $flag $who }
	}
	unset MkOp($id)
    }
    after $testTime ircTests
}
#
proc deMonitor {w chan} {
    if ![string match {} $w] {catch {destroy $w}}
    global monitor
    if {[set x [lsearch $monitor ${chan} ]] >= 0} {
	listdel monitor $x
    }
    [Channel :: find ${chan}] configure -monitor 0
}
#
proc object_list {name} {
    global $name
    set l {}
    foreach v [array names $name] { lappend l [set ${name}($v)] }
    return $l
}
#
proc Message_list {} { object_list MTO }
#
proc Chat_list {} { object_list CHTO }
#
proc Notice_list {} { object_list NTO }
#
proc Channel_list {} { object_list CTO }
#
proc Info_list {} { global mainInfo ; return [list $mainInfo] }
#
proc find {name} {
    foreach class {Channel Message Notice} {
	switch [set handle [$class :: find $name]] {
	nil { }
	default { return $handle }
	}
    }
    return nil
}
