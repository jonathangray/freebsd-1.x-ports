#
proc newName {class} {
    global zircon OType
    set n [string tolower ${class}][incr zircon(nameCount)]
    set OType($n) $class
    return $n
}
#
proc makeArray {args} {
    foreach x $args { global $x ; set ${x}(1) 1 ; unset ${x}(1) }
}
#
proc channel {w} {
    global Name
    set win [winfo toplevel $w]
    return [expr {[info exists Name($win)] ? $Name($win) : {nil}}]
}
#
proc window {chan} { return [[find $chan] window] }
#
proc normal {w} {
    return [string match {normal} [lindex [${w} conf -state] 4]]
}
#
proc capitalise {str} {
    return [string toupper [string index $str 0]][string range $str 1 end]
}
#
proc getOption {var dflt} {
    global $var
    if [string match {} [set $var [option get . $var [capitalise $var]]]] {
	set $var $dflt
    }
}
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
#
proc listdel {v item} {
    set cmd \
      "if {\[llength \$$v\] > $item} { set $v \[lreplace \$$v $item $item\] }"
    uplevel 1 $cmd
}
#
proc listupdate {list item val} {
    uplevel 1 "while { \[llength \$${list}\] <= $item} {lappend ${list} {} }"
    uplevel 1 "set $list \[lreplace \$$list $item $item {$val} \]"
}
#
proc listmove {list from to val} {
    set cmd "set $list \[linsert \[lreplace \$$list $from $from\] $to $val \]"
    uplevel 1 $cmd
}
#
# Procedure used to shorten menu labels to 10 characters. Used
# when adding user provided items to menus
#
proc prune {name lng} {
    regsub -all "\[\002\017\026\037\]" $name {} name
    return [expr {[string length $name] > $lng ? \
      "[string range $name 0 [expr {$lng - 3}]]..." : $name}]
}
#
proc killWindow {win} {
    global Icon IconBM
    catch "unset Icon(${win})"
    catch "unset IconBM(${win})"
    catch "destroy ${win}"
}
#
#	proc isChannel : Returns true if name is a channel name
#
proc isChannel {name} { return [string match {[#&]*} ${name}] }
#
#	proc mungPrefix : breaks up the prefix to an IRC message
#	returns : {nick, me?, lowercase nick, name, ignores}
#
proc mungPrefix {prefix} {
    global myid
    if ![regexp {:([^!]*)!(.*)} $prefix m1 nk nm] {
	set nk [string range $prefix 1 end]
	set nm {}
    }
    set usr [User :: make ${nk}]
    return [list $usr [expr {$myid == $usr}] $nm [z_ignore $usr $nm]]
}
#
#	proc me : Returns true if nk is this user
#		  Assumes that nk is in lower case!!!
#
proc me {nk} {
    global myid
    return [expr {[string tolower $nk] == [$myid lname]}]
}
#
# proc active : returns true if name is an active channel
#
proc active {chan} {
    return [expr ![string match {} [[find ${chan}] window]]]
}
#
proc addCTCPMenu {name usr} {
    $name add cascade -label CTCP -menu $name.ctcp
    menu $name.ctcp
    foreach cmd \
      {Clientinfo Echo Errmsg Finger Pid Ping
       Source Time Version Userinfo Zircon Other} {
	if {$usr == {{}}} {
	    set prc "usersCTCP [string toupper $cmd]"
	} {
	    set prc "doCtcp [string toupper $cmd] \[$usr name \]"
	}
	$name.ctcp add command -label $cmd -command $prc
    }
}
#
proc addDCCMenu {name usr} {
    $name add cascade -label DCC -menu $name.dcc
    menu $name.dcc
    foreach cmd {Send Chat} {
	$name.dcc add command -label $cmd \
	  -command "doDCC [string toupper $cmd] \[$usr name\]"
    }
}
#
proc makeUserMenu {chid win usr} {
    if [winfo exists $win] {
	return $win
    } {
	return [menu $win -postcommand "postUM $chid $win $usr"]
    }
}
#
proc postUM {chid win usr} {
    global zircon
    set w [winfo parent $win]
    $win add command -label Whois -command "doWhois \[$usr lname\] {}"
    $win add command -label Msg -command "Message :: make \[$usr name\]"
    $win add command -label Notice -command "channelNotice \[$usr lname\]"
    $win add command -label Time -command "sendIRC TIME \[$usr name\]"
    addCTCPMenu $win $usr
    addDCCMenu $win $usr
    $win add checkbutton -label Notify -variable Unotify($usr) \
      -command "$usr doNotify"
    if ![string match {nil} $chid] {
	addIgnoreMenu $win $usr
	$win add command -label Finger -command "$usr finger"
        set st [expr {[$chid operator] ? "normal" : "disabled"}]
	$win add checkbutton -label Speak -variable ${chid}Spk($usr) \
	  -command "$chid userMode $usr v" -state $st
	$win add checkbutton -label ChanOp -variable ${chid}Op($usr) \
	  -command "$chid userMode $usr o" -state $st
	$win add command -label Kick -command "$chid kick $usr" \
	  -state $st
	$win add command -label Ban+Kick \
	  -command "$chid banKick $usr"  -state $st
	$win add command -label Kill -command "$usr kill" \
	  -state [expr {$zircon(ircop) ? "normal" : "disabled"}]
    }
    $win configure -postcommand {}
}
#
proc invert {button} {
    set fg [lindex [$button conf -foreground] 4]
    set bg [lindex [$button conf -background] 4]
    $button conf -foreground $bg -background $fg \
      -activeforeground $fg -activebackground $bg
}
#
proc makeLB {win args} {
    frame $win -relief raised
    frame $win.f -borderwidth 0
    scrollbar $win.v -command "$win.l yview"
    eval listbox $win.l -xscrollcommand "{$win.h set}" \
      -yscrollcommand "{$win.v set}" $args
    pack $win.l -side left -expand 1 -fill both -in $win.f
    pack $win.v -side left -fill y -in $win.f

    frame $win.g -borderwidth 0
    scrollbar $win.h -command "$win.l xview" -orient horizontal

    frame $win.g.p
    pack $win.h -side left -expand 1 -fill x -in $win.g
    pack $win.g.p -side right -padx 5
    pack $win.g -fill x -side bottom
    pack $win.f -expand 1 -fill both -side top
    return $win
}
#
proc rebind {txt} {
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
