global defChan
set defChan {}
#
proc nil {op args} {
    switch $op {
    window { return {} }
    operator -
    isa { return 0 }
    opText -
    addText { global mainInfo ; eval $mainInfo addText $args}
    default { error "***** \007\007 nil called : $op $args" }
    }	
}
#
proc Channel {name args} {
    if [string match {::} $name] {
	return [eval Channel_[lindex $args 0] [lrange $args 1 end] ]
    }
    if [string match {nil} [set id [Channel :: find $name]]] {
	set id [makeChannel $name]
    }
    if ![string match {} $args] { eval $id configure $args }
    return $id
}
#
proc cleanup {name} {
    set pattern "\[ ,\n\r\]"
    regsub -all "$pattern" $name {} name
    return $name
}
#
proc oneLiner {win this} {
    notIdle $win
    set x [split [$win get]]
    set who [lindex $x 0]
    set msg [join [lrange $x 1 end]]
    sendIRC PRIVMSG $who $msg
    $this addText {} ">>$who : $msg"
    $win delete 0 end
}
#
proc channel_makePalette {this} {
    set w .@palette
    if [winfo exists $w] { popup $w ; return }
    global Ft
    killWindow $w
    toplevel $w -class Zircon
    wm title $w {Zircon Palette}
    for {set i 2} {$i < 16} {incr i} {
	frame $w.f$i
	for {set j 0} {$j < 16} {incr j} {
	    set c [format "%c" [expr {$i * 16 + $j}]]
	    button $w.f$i.l$j -text $c -relief raised -width 1 \
	      -command "\[focus\] insert insert $c" -font $Ft($this)
	    pack $w.f$i.l$j -side left
	}
	pack $w.f$i -side top
    }
    button $w.ok -text OK -command "destroy $w"
    pack $w.ok -fill x
}
#
proc setClose {this val} {
    if [regexp {^[0-9]+$} $val] { $this configure -closetime $val }
}
#
proc channel_set {this flag} {
    switch $flag {
    close {
	    global Cclose
	    if $Cclose($this) {
		set v [$this closetime]
		if [string match {0} $v] { set v {} }
		mkEntryBox .@cl$this {Close Time} \
		  "Enter the close time for [$this name]:" \
		  "{Time $v}" "OK {setClose $this}" \
		  {Cancel {}}
	        tkwait window .@cl$this
	    }
	}
    }
}
#
proc setCrypt {this key} {
    $this configure -crypt $key
    if [$this isa Message] {
	[User :: find [$this name]] configure -crypt $key
    }
}
#
proc clearCrypt {this args} { setCrypt $this {} }
#
proc channel_getCrypt {this} {
    mkEntryBox .@cr$this {Encryption Key} \
      "Enter the encryption key for [$this name]:" \
      "{Key [$this crypt]}" "OK {setCrypt $this}" "Clear {clearCrypt $this}"\
      {Cancel {}}
    tkwait window .@cr$this
}
#
proc channel_drawSet {this} {
    set mn [$this window].channel.menu
    $mn entryconfigure [$mn index {Draw}] \
      -state [expr {[$this draw] ? {normal} : {disabled}}]
}
#
proc makeChannel {chan} {
    set this [newName Channel]
    global Clname CTO $this Cname defChan
    proc $this {args} " eval channel_call $this \$args "
    set lchan [string tolower $chan]
    set Cname($this) ${chan}
    set Clname($this) $lchan
    $this configure -hpos 0 -hbuff {} -window {} -actions 0 -patterns {} \
      -logfile {} -log {} -menu 0 -join 0 -ops {} -bindings {} \
      -icon {} -topics {} -keep 0 -monitor 0 -crypt {}
    if [string match {} $defChan] {
	$this configure -open 0 -close 0 -jump 1 -quiet 0 -draw 1 \
	  -history 50 -closetime 0 -msg {} -key {} -buttons 1
    } {
	$this configure -open [$defChan open] -close [$defChan close] \
	  -jump [$defChan jump] -quiet [$defChan quiet] -draw [$defChan draw] \
	  -history [$defChan history] -closetime [$defChan closetime] \
	  -msg [$defChan msg] -key {} -buttons [$defChan buttons]
    }
    set CTO($lchan) $this
    foreach m {p m s i t n} { set ${this}($m) 0 }
    return $this
}
#
proc channel_show {this} {
    if ![$this active] {
	global zircon
	switch $zircon(style) {
	hicaffiene	-
	diet	-
	original	{ $this original }
	}
    }
}
#
proc channel_operator {this} {
    global ${this}Op myid
    return [set ${this}Op($myid)]
}
#
proc channel_call {this op args} {
   switch $op {
   isa { global OType ; return [expr {$OType($this) == [lindex $args 0]}] }
   name { global Cname ; return $Cname($this) }
   lname { global Clname ; return $Clname($this) }
   text { global Ctext ; return $Ctext($this) }
   logfile { global Clogfile ; return $Clogfile($this) }
   patterns { global Cpatterns ; return $Cpatterns($this) }
   menu { global Cmenu ; return $Cmenu($this) }
   close { global Cclose ; return $Cclose($this) }
   open { global Copen ; return $Copen($this) }
   jump { global Cjump ; return $Cjump($this) }
   draw { global Cdraw ; return $Cdraw($this) }
   quiet { global Cquiet ; return $Cquiet($this) }
   icon { global Cicon ; return $Cicon($this) }
   join { global Cjoin ; return $Cjoin($this) }
   msg { global Cmsg ; return $Cmsg($this) }
   key { global Ckey ; return $Ckey($this) }
   bindings { global Cbindings ; return $Cbindings($this) }
   closetime { global Cclosetime ; return [expr $Cclosetime($this) / 1000]}
   history { global Chistory ; return $Chistory($this) }
   crypt { global Ccrypt ; return $Ccrypt($this) }
   buttons { global Cbuttons ; return $Cbuttons($this) }
   ops { global Cops ; return $Cops($this) }
   topics { global Ctopics ; return $Ctopics($this) }
   window {
	    global Cwindow
	    if [string match {} $Cwindow($this)] { return {} }
	    return [$Cwindow($this) name]
	}
   active { global Cwindow ; return [expr ![string match {} $Cwindow($this)]] }
   keep { global Ckeep ; return $Ckeep($this) }
   monitor { global Cmonitor ; return $Cmonitor($this) }
   default { eval channel_$op $this $args }
   }
}
#
proc channel_setTopic {this string} {
    set w [$this window].topic.entry
    set state [lindex [$w conf -state] 4]
    $w configure -state normal
    $w delete 1.0 end
    insertText $this $w $string {}
    $w configure -state $state
}
#
proc channel_changeMode {this mode} {
    global $this
    setMode [$this name] [expr {[set ${this}(${mode})] ? {+} : {-}}]${mode}
}
#
proc channel_optText {this name string} {
    if [$this wantMessage $name] { $this addText @$name $string }
}
#
proc channel_addText {this tag text} {
    $this doPopUp
    $this doAddText $tag $text
}
#
proc channel_tagInfo {this tag} {
    if [string match {} $tag] {
	return {}
    } {
	global TFn TFg TBg
	set tag [string tolower $tag]
	set indx "${this},$tag"
	if ![info exists TFn($indx)] { setTags $this $tag }
	return [list $tag $TFg($indx) $TBg($indx) $TFn($indx)] 
    }
}
#
proc channel_send {this string args} {
    notIdle {}
    if ![string match {} $string] {
	global Secure
	set lc [$this lname]
	set rchan [expr \
	  {[info exists Secure($lc)] ? $Secure($lc) : [$this name]}]
	sendIRC PRIVMSG ${rchan} [encrypt $string [$this crypt]]
	if [string match {} $args] {
	    $this addText @me "> $string"
	} {
	    $this doAddText @me "> $string"
	}
    }
}
#
proc channel_isOp {this usr} {
    global ${this}Op
    return [set ${this}Op($usr)]
}
#
proc channel_isSpeaker {this usr} {
    global ${this}Spk
    return [set ${this}Spk($usr)]
}
#
proc channel_killUser {this usr} {
    global ${this}Op ${this}Spk myid
    set w [$this window]
    set win $w.cFrm.uFrm.userBtn.frame
    catch {destroy $win.$usr}
    set wd [winfo reqwidth $win.$myid]
    set ht [winfo reqheight $win.$myid]
    set ht [expr { $ht * [llength [winfo children ${win}]]}]
    $w.cFrm.uFrm.userBtn conf -scrollregion [list 0 0 $wd $ht]
    catch {unset ${this}Op($usr)${this}Spk($usr)}
    set w $w.users.menu
    catch {$w delete [indexHack $w [$usr name] 2]}
    destroy $w.$usr
    $usr leave $this
}
#
proc channel_isJoined {this usr} {
    return [winfo exists [$this window].cFrm.uFrm.userBtn.frame.$usr]
}
#
proc channel_sendJoin {this args} {
    if ![string match {} [lindex $args 0]] {
	$this configure -key [lindex $args 0]
    }
    sendIRC JOIN [$this name] [$this key]
}
#
proc channel_doJoin {this usr nm prefix} {
    $this show
    $usr join $this
    if ![$this isJoined $usr] {
	$this optText JOIN \
	  "*** [$usr name] ($nm) has joined channel [$this name]"
	$this addUser $usr 0 0
	$this setOps $prefix $usr
    } {
	set w [$this window]
	if {[set x [indexHack $w.users.menu [$usr name] 2]] >=0} {
	    $w.users.menu entryconfigure $x -state normal
	}
	$w.cFrm.uFrm.userBtn.frame.$usr conf -state normal
	$usr heal
    }
}
#
proc channel_inactive {this} {
    if {[$this close] && [$this active]} {
	global testTime Cclosecount Cclosetime
	if {[incr Cclosecount($this) -$testTime] <= 0} {
	    wm iconify [$this window]
	    if [winfo exists .@zd$this] { wm iconify .@zd$this }
	    set Cclosecount($this) $Cclosetime($this)
	}
    }
}
#
proc channel_extendTime {this} {
    global Cclosecount Cclosetime
    set Cclosecount($this) $Cclosetime($this)
}
#
proc setGlob {array element value} {
    global $array
    set ${array}($element) $value
}
#
proc setOption {this sub opt val} {
    if {[$this lname] == {*default*}} {
	set id *cFrm*${sub}[capitalise $opt]
    } {
	set id *$this*${sub}$opt
    }
    option add $id $val
}
#
proc channel_configure {this args} {
    while {![string match {} $args]} {
	set val [lindex $args 1]
	set name [lindex $args 0]
	set opt [string range $name 1 end]
	switch -glob -- $name {
	-foreground -
	-background -
	-font {
		setGlob "C$opt" $this $val
		setOption $this {} $opt $val
	    }
	-geometry {
		setGlob "C$opt" $this $val
		setOption $this {cFrm.} $opt $val
	    }
	-height -
	-width  {
		setGlob "C$opt" $this $val
		if [$this isa Channel] { set type text } \
		elseif [$this isa Message] { set type msg } \
		elseif [$this isa Notice] { set type note } \
		elseif [$this isa Chat ] { set type chat } \
		elseif [$this isa Info ] { set type info }
		setOption $this ${type}. $opt $val
	    }
	-boldfont {
		setGlob "C$opt" $this $val
		if [$this isa Channel] { set type text } \
		elseif [$this isa Message] { set type msg } \
		elseif [$this isa Notice] { set type note } \
		elseif [$this isa Chat ] { set type chat } \
		elseif [$this isa Info ] { set type info }
		setOption $this ${type}. boldFont $val
	    }
	-topic {
		if ![string match {} $val] {
		    sendIRC TOPIC [$this name] $val
		}
	    }
	-closetime {
		global testTime Cclosetime Cclosecount
		set ct [expr {$val * 1000}]
		if {$ct > 0 && $ct < $testTime} { set testTime $ct }
		set Cclosetime($this) $ct
		set Cclosecount($this) $ct
	    }
	-buttons {
		global Cbuttons
		set Cbuttons($this) $val
		if [winfo exists [$this window].users.menu] {
		    [$this window].users.menu entryconfigure 0 -label \
		      [expr {[$this buttons] ? {No Buttons} : {Buttons}}]
		}
	    }
	-name {
		global Cname Clname
		set Cname($this) $val
		set Clname($this) [string tolower $val]
	    }
	-window {
		global Cwindow
		if {[info exists Cwindow($this)] && \
		  ![string match {} [set w $Cwindow($this)]]} {
		    $w delete
		}
		set Cwindow($this) $val
	    }
	-*  { setGlob "C$opt" $this $val }
	}
	set args [lrange $args 2 end]
    }
}
#
proc channel_log {this val} {
    global Clog
    if ![string match {} $Clog($this)] { puts $Clog($this) $val }
    $this extendTime
}
#
proc channel_popup {this} { popup [$this window] ; return $this }
#
proc channel_popdown {this} { wm iconify [$this window] ; return $this }
#
proc channel_tagWindow {this} {
   global OType
   set w [$this lname]
   set type [string tolower $OType($this)]
   if ![winfo exists .$type.$w] {
        if ![winfo exists .$type] { frame .$type }
	set path $type
	set r $w
	while {[regexp {([^.]*)\.(.*)} $r match f r]} {	
	    append path ".$f"
	    if ![winfo exists .$path] { frame .$path }
	}
	append path ".$r"
	if ![winfo exists .$path] { frame .$path }
   }
   return .$type.$w
}
#
proc channel_original {this} {
    global Name startup myid
    set rchan [$this name]
    set chan [$this lname]
    set realChan [$this isa Channel]
    set wndw [Window .$this]
    set w [$wndw name]
    set Name($w) $this
    $this configure -window $wndw
    if [$this isa Channel] {
	deMonitor .@mon${this} ${chan}
	set lp {IRC Channel} ; set wname ${rchan}
	$this configure -text $w.cFrm.text
    } \
    elseif [$this isa Notice] {
	set lp {IRC Notice from} ; set wname ${rchan}
	$this configure -text $w.cFrm.note
    } \
    elseif [$this isa Chat] {
	set lp {DCC Chat with} ; set wname "Chat ${rchan}"
	$this configure -text $w.cFrm.chat
    } \
    elseif [$this isa Message] {
	set lp {IRC Conversation with} ; set wname ${rchan}
	$this configure -text $w.cFrm.msg
    }
    $wndw configure -title "$lp $rchan" -minsize {40 1}
    $wndw setIcon $this $wname

    set oFrm $w

    if $realChan {
	frame $w.topic
	menubutton $w.topic.label -text Topic -relief raised
	$w.topic.label conf -menu $w.topic.label.menu
	set om [menu $w.topic.label.menu]
	$om add command -label New -command "getTopic \[channel $w\]"
	$om add separator
	foreach nn [$this topics] {
	    $om add command -label "[prune $nn 15]" \
	      -command "$this configure -topic {${nn}}"
	}
	emacsTEntry $w.topic.entry -relief raised
	pack $w.topic.label -side left
	pack $w.topic.entry -side left -expand 1 -fill x
	bind $w.topic.entry <Return> {sendTopic %W ; notIdle %W}
    }

    frame $w.cmds -borderwidth 0
    frame $w.cmds.cmds0
    set om [makeMB $w.mode Mode]
    $om add checkbutton -label {Pop Up} -variable Copen($this) \
	-command "$this set open"
    $om add checkbutton -label {Pop Down} -variable Cclose($this) \
	-command "$this set close"
    $om add checkbutton -label Draw -variable Cdraw($this) \
      -command "$this drawSet"
    if [$this isa Chat] { $om entryconfigure last -state disabled }
    $om add checkbutton -label Jump -variable Cjump($this) \
	-command "$this set jump"
    $om add checkbutton -label Quiet -variable Cquiet($this) \
	-command "$this set quiet"
    $om add checkbutton -label Actions -variable Cactions($this) \
      -command "$this flipActions"
    if [$this isa Chat] { $om entryconfigure last -state disabled }
    if $realChan {
	global $this
	$om add command -command "$this setBan" -label Ban
	foreach vl {{p Private} {m Moderated} {s Secret} {i {Invite Only}} \
	  {t Topic} {n {No Msg}}} {
	    set v [lindex $vl 0]
	    set ${this}($v) 0
	    $om add checkbutton -command "$this changeMode $v" \
	      -variable ${this}(${v}) -state disabled -label [lindex $vl 1]
	}	
	$om add command -command "$this setLimit" -state disabled -label Limit
	$om add command -command "setKey {${chan}}" -state disabled -label Key

	set om [makeMB $w.channel Channel]
	$om add command -command "sendIRC WHO {${rchan}}" -label Who
	$om add command -command "channelInvite {${chan}}" -label Invite
	$om add command -command "channelNotice {${chan}}" -label Notice
	$om add command -command "$this makeZDraw" -label Draw
	if ![$this draw] { $om entryconfigure 3 -state disabled }
	addCTCPMenu $om $this
    } {
	set om [makeMB $w.channel User]
	$om add command -command "sendIRC WHOIS ${rchan}" -label Whois
	$om add command -label Invite -state disabled
	$om add command  -command "channelNotice ${chan}" -label Notice
	$om add command -command "$this makeZDraw" -label Draw
	if {![$this draw] || [$this isa Chat]} {
	    $om entryconfigure last -state disabled
	}
	addCTCPMenu $om $this
	addDCCMenu $om $this
    }
    $om add cascade -label Log -menu $om.log
    menu $om.log
    foreach cmd {Close Open Flush Empty} {
	$om.log add command -label $cmd -command "$this doLog $cmd" \
	  -state disabled
    }
    if ![string match {} [set lf [$this logfile]]] {
	if [catch {open $lf a} ld] {
	    mkDialog ERROR .@le$this {Log Error} \
	      "*** Cannot open log file $lf for channel ${chan} : $ld" \
	      {OK {}}
	    $om.log entryconfigure 1 -state normal
	} {
	    $om.log entryconfigure 0 -state normal
	    $om.log entryconfigure 2 -state normal
	    $om.log entryconfigure 3 -state normal
	    $this configure -log $ld
	}
    } {
	$om.log entryconfigure 1 -state normal
    } 
    $om add command -label Crypt -command "$this getCrypt"
    if [$this isa Chat] { $om entryconfigure last -state disabled }
    set om [makeMB $w.action Action]
    if ![$this isa Chat] {
	$om add command -label New -command "$this getAction"
	$om add separator

	global actions
	foreach act $actions {
	    $om add command -label "[prune $act 15]" \
	      -command "$this action {$act}"
	}
    } {
	$w.action configure -state disabled
    }
    $this buildUsersMenu [makeMB $w.users Names]
    frame $w.cmds.cmds1
    button $w.quit -command "$this leave" -text Leave
    bind $w.quit <Shift-1> {tk_butDown %W}
    bind $w.quit <Shift-ButtonRelease-1> {
	set sc [lindex [%W configure -command] 4]
	%W configure -command quitZircon
	tk_butUp %W
	%W configure -command $sc
    }
    if $realChan {
	bind $w.quit <Control-1> {tk_butDown %W}
	bind $w.quit <Control-ButtonRelease-1> "
	    tk_butUp %W
	    $this configure -monitor 1
	"
    }

    button $w.clear -command "$this clear 0" -text Clear
    bind $w.clear <Shift-1> {tk_butDown %W}
    bind $w.clear <Shift-ButtonRelease-1> "
	set sc \[lindex \[%W configure -command \] 4 \]
	%W configure -command {$this clear 1}
	tk_butUp %W
	%W configure -command \$sc
    "
    pack $w.mode $w.channel $w.action $w.users \
      -in $w.cmds.cmds0 -side left -expand 1 -fill x
#    tk_menuBar $w.cmds.cmds0 $w.mode $w.channel $w.action $w.users

    pack $w.quit $w.clear -in $w.cmds.cmds1 -side left -expand 1 -fill x
    pack $w.cmds.cmds0 $w.cmds.cmds1 -side left -expand 1 -fill x
    frame $w.cFrm -borderwidth 0
    $this buildUsers $w
    $this addUser $myid 0 0
    frame $w.cFrm.textFrm -relief raised
    set ot [$this text]
    scrollbar $w.cFrm.vscroller -command "doScroll $ot"
    text $ot -yscrollcommand "setScroll $ot $w.cFrm.vscroller"
    rebind $ot
    set tgw [$this tagWindow]
    foreach tag {foreground background font selectForegound
      selectBackground width height} { 
	if ![string match {} [set v [option get $tgw $tag [capitalise $tag]]]] {
	    $ot configure -[string tolower $tag] $v
	}
    }
    global BF Fg Bg Ft
    set BF($this) [getOValue $ot font boldFont Font]
    set Ft($this) [getOValue $ot font font Font]
    set Fg($this) [getOValue $ot foreground foreground Foreground]
    set Bg($this) [getOValue $ot background background Background]
    $ot conf -selectforeground $Bg($this) -selectbackground $Fg($this)
    pack $w.cFrm.vscroller -side right -fill y -in $w.cFrm.textFrm
    pack $ot -side left -expand 1 -fill both -in $w.cFrm.textFrm
    set om [frame $w.cmdLine -relief raised]
    scrollbar $om.cscroller -orient horizontal -command "$om.commandLine view"
    emacsEntry $om.commandLine -scrollcommand "$om.cscroller set"
    pack $om.commandLine $om.cscroller -expand 1 -fill x

    if {$realChan} { pack $w.topic -fill x }

    pack $w.cmds -side top -fill x
    pack $w.cmdLine -side bottom -fill x
    pack $w.cFrm.textFrm -expand 1 -fill both
    pack $w.cFrm -expand 1 -fill both
    set occ $w.cmdLine.commandLine
    doBindings $occ $this $chan
    focus $occ
    bind $ot <Enter> "focus $occ ; notIdle %W"
    bind $ot <Configure> {%W yview -pickplace end ; notIdle %W}
    bind $w.cFrm <Enter> "focus $occ ; notIdle %W"
    bind $w.cFrm <Destroy> "$this doLeave"
    bind $w.cFrm <Visibility> {
	set win [winfo toplevel %W]
	global Icon IconBM
	if [info exists Icon($win)] {wm iconname $win $Icon($win)}
	if [info exists IconBM($win)] {
	    wm iconbitmap $win [lindex $IconBM($win) 0]
	}
	notIdle %W
    }
    tkwait visibility $w

    if {[$this open] && [$this join]} {	$wndw iconify }
    if $realChan { sendIRC MODE ${chan} }
}
#
proc channel_buildUsers {this w} {
    set f [frame $w.cFrm.uFrm -relief raised]
    scrollbar $f.vscroller -command "$f.userBtn yview" 
    canvas $f.userBtn -yscrollcommand "$f.vscroller set"
    frame $f.userBtn.frame -border 0
    $f.userBtn create window 0 0 -window $f.userBtn.frame -anchor nw
    pack $f.vscroller -side right -fill y
    pack $f.userBtn -side left -fill y
    if [$this isa Channel] { pack $f -side right -fill y }
}
#
proc channel_buildUsersMenu {this w} {
    global ${this}Spk ${this}Op myid
    set ${this}Op($myid) 0
    set ${this}Spk($myid) 0
    $w add command -label [expr {[$this buttons] ? {No Buttons} : {Buttons}}] \
      -command "$this toggleUsers"
    $w add separator
}
#
proc channel_toggleUsers {this} {
    set w [$this window]
    if [winfo ismapped $w.cFrm.uFrm] {
	pack forget $w.cFrm.uFrm
	$w.users.menu entryconfigure 0 -label Buttons
    } {
	pack $w.cFrm.uFrm -side right -fill y -before $w.cFrm.textFrm
	$w.users.menu entryconfigure 0 -label {No Buttons}
    }
}
#
proc channel_delete {this} { mcnDelete $this CTO }
#
proc mcnDelete {this nvar} {
    global Clname Name Clog ${nvar} $this
    if ![string match {} $Clog($this)] { close $Clog($this) }
    set win [$this window]
    $this configure -window {}
    if {![$this keep] && ![$this monitor]} {
	set chan $Clname($this)
	unset Clname($this) Clog($this) ${nvar}(${chan})
	catch {unset Name($win)}
	foreach v {Copen Cclose Chistory Cdraw BF Bg Fg Cjump Cquiet Chpos \
	  Chbuff Cactions Clogfile Cclosetime Ccrypt Cbuttons \
	  Cclosecount Cwindow OType Cpatterns Cname Cmenu Cjoin Cops \
	  Cmsg Cbindings Cicon Ctopics Ckeep Cmonitor Ckey Ctext \
	  Cforeground Cbackground Cfont Cgeometry Cheight Cwidth \
	  Cboldfont } {
	    global $v ; catch {unset ${v}($this)}
	}
	rename $this {}
    } \
    elseif [$this monitor] { channelMonitor [$this name] }
    foreach v {Op Spk} {
	global ${this}${v}
	catch {unset ${this}${v}}
    }
    catch {unset $this}
}
#
proc channel_getPrev {id} {
    global Chpos Chbuff
    if [string match {} [set line [lindex $Chbuff($id) $Chpos($id)]]] {
	set Chpos($id) 0
	set line [lindex $Chbuff($id) 0]
    } {
	incr Chpos($id)
    }
    return $line
}
#
proc channel_getNext {id} {
    global Chpos Chbuff
    if [string match {} [set line [lindex $Chbuff($id) $Chpos($id)]]] {
	set Chpos($id) 0
	set line [lindex $Chbuff($id) 0]
    } {
	incr Chpos($id) -1
    }
    return $line
}
#
proc channel_addToHist {this txt} {
    $this configure -hpos 0
    if ![string match {} $txt] {
	global Chbuff ; set Chbuff($this) [linsert $Chbuff($this) 0 $txt]
	set Chbuff($this) [lrange $Chbuff($this) 0 9]
    }
    return $txt
}
#
proc channel_flipActions {this} {
    set win [$this window].cmdLine.commandLine
    set ret [bind $win <Return>]
    set sret [bind $win <Shift-Return>]
    bind $win <Return> $sret
    bind $win <Shift-Return> $ret
}
#
# Leaving channels :
#	doLeave sends the PART message
#	leaveChannel does the are you sure dialog
#
proc channel_doLeave {this} {
    global sock
    if {![string match {} $sock] && [$this isa Channel]} {
	sendIRC PART [$this name]
    } {
	$this delete
    }
}
#
proc channel_leave {this} {
    set chan [$this name]
    mkDialog LEAVE .@$this "Leaving ${chan}" \
      "Really leave channel ${chan}?" {} "OK {$this doLeave}" {Cancel {}}
}
#
proc channel_doAddText {this tag text} {
    global Cjump
    $this log $text
    set tagInfo [$this tagInfo $tag]
    set name [$this text]
    set start [$name index end]
    set end [insertText $this $name $text $tagInfo]
    $name insert end "\n"
    if ![string match {} $tag] {
	set tag [lindex $tagInfo 0]
	$name tag add $tag $start $end
	$name tag lower $tag sel
	global Fg Bg
	set fg [lindex $tagInfo 1]
	set bg [lindex $tagInfo 2]
	if {$fg != $Fg($this)} { $name tag configure $tag -foreground $fg }
	if {$bg != $Bg($this)} { $name tag configure $tag -background $bg }
	$name tag configure $tag -font [lindex $tagInfo 3]
	if ![string match {@*} $tag] {
	    $name tag bind $tag <Double-Button-1> "Message :: make $tag"
	    $name tag bind $tag <Shift-Double-Button-1> "sendIRC WHOIS $tag $tag"
	    $name tag bind $tag <Control-Double-Button-1> "finger $tag"
	}
    }
    update idletasks
    if $Cjump($this) { $name yview -pickplace end }
}
#
proc channel_doPopUp {this} {
    global Copen
    if [string match {} [set win [$this window]]] { return }
    if {$Copen($this) && ![winfo ismapped $win]} {
	global noPopup
	if $noPopup {
	    global Icon IconBM
	    wm iconname $win "*$Icon($win)*"
	    if {[info exists IconBM($win)] && \
		![string match {} [set icn [lindex $IconBM($win) 1]]]} {
		wm iconbitmap $win $icn
	    }
	} {
	    wm deiconify $win
	    $this extendTime
	}
    }
}
#
proc channel_keepAction {this value} {
    $this action $value
    [$this window].action.menu add command -label "[prune $value 15]" \
      -command "$this action {$value}"
    global actions confChange
    lappend actions $value
    set confChange 1
}
#
proc channel_getAction {this} {
    mkEntryBox .@${this}action "Action" "Enter your action:" \
      {{Action {}}}\
      "OK {$this action}" "Keep {$this keepAction}" {Cancel {}}
}
#
# Send string to channel as an ACTION and echo to channel window
#
proc channel_action {this string} {
    notIdle {}
    if ![string match {} $string] {
	global myid
	sendIRC PRIVMSG [$this name] "\001ACTION $string\001"
	$this addText @me "* [$myid name] $string"
    }
}
#
proc channel_clear {this hist} {
    set t [$this text]
    if $hist { $t delete 1.0 end }
    $t yview end
}
#
proc channel_insert {this text} {
    set ent [$this window].cmdLine.commandLine
    if ![string match {} $text] {
	while {[regexp "(\[^\012\]*)\012(.*)" $text dummy line text]} {
	    $ent insert insert $line
	    tk_entrySeeCaret $ent
	    $this send [$ent get]
	    $ent delete 0 end
	}
	if ![string match {} $text] {
	    $ent insert insert $text
	    tk_entrySeeCaret $ent
	}
    }
}
#
proc channel_insertSelect {this} {
    if ![catch {selection get} bf] { $this insert $bf }
}
#
proc channel_markOp {this usr} {
    global ${this}Op myid
    set ${this}Op($usr) 1
    if {$usr == $myid} { $this opItems normal }
    set w [$this window]
    markButton $w.cFrm.uFrm.userBtn.frame.$usr operator
    markEntry $w.users.menu [indexHack $w.users.menu [$usr name] 2] operator
}
#
proc channel_unmarkOp {this usr} {
    global ${this}Op ${this}Spk myid
    set ${this}Op($usr) 0
    set par [expr  {[set ${this}Spk($usr)] ? "speaker" : {}}]
    if {$usr == $myid} { $this opItems disabled }
    set w [$this window]
    markButton $w.cFrm.uFrm.userBtn.frame.$usr $par
    markEntry $w.users.menu [indexHack $w.users.menu [$usr name] 2] $par
}
#
proc channel_markV {this usr} {
    global ${this}Spk
    set ${this}Spk($usr) 1
    if ![$this isOp $usr] {
	set w [$this window]
	markButton $w.cFrm.uFrm.userBtn.frame.$usr speaker
	markEntry $w.users.menu [indexHack $w.users.menu [$usr name] 2] speaker
    }
}
#
proc channel_unmarkV {this usr} {
    global ${this}Spk
    set ${this}Spk($usr) 0
    if ![$this isOp $usr] {
	set w [$this window]
	markButton $w.cFrm.uFrm.userBtn.frame.$usr {}
	markEntry $w.users.menu [indexHack $w.users.menu [$usr name] 2] {}
    }
}
#
proc channel_flag {this state} {
    set win [$this window]
    foreach w {mode channel action} {
	catch "$win.$w conf -state $state"
    }
    if [winfo exists $win.topic] {
	$win.topic.label conf -state disabled
	$win.topic.entry conf -state disabled
	foreach w [winfo children $win.cFrm.uFrm.userBtn.frame] {
	    $w conf -state $state
	}
	if [string match {normal} $state] {
	    global Split
	    foreach w [array names Split] {
		foreach n $Split($w) {
		    if [$this isJoined $n] {
			$win.cFrm.uFrm.userBtn.frame.$n conf -state disabled
		    }
		}
	    }
	}
    }	
}
#
proc channel_nickChange {this usr nnk} {
    global ${this}Op ${this}Spk myid
    if {[set w [$this window]] == {}} { return }

    if [$this isJoined $usr] {
	$w.cFrm.uFrm.userBtn.frame.$usr configure -text $nnk
	$this optText NICK "*** [$usr name] is now known as $nnk"
    }
    if {[set x [indexHack $w.users.menu [$usr name] 2]] >=0} {
	$w.users.menu entryconfigure $x -label $nnk
    }
    if {[set n [$this lname]] == [$usr lname]} {
	wm title $w "Conversation with $nnk"
	set var [expr [$this isa Notice] ? {NTO} : {MTO}]
	global Clname $var Cname
	set ln [string tolower $nnk]
	set Clname($this) $ln
	set Cname($this) $nnk
	set ${var}($ln) $this
	unset ${var}($n)
    }
}
#
proc channel_addUser {this usr op sp} {
    global ${this}Op ${this}Spk myid
    set ${this}Op($usr) $op
    set ${this}Spk($usr) $sp
    set w [$this window]
    if [winfo exists [set win $w.cFrm.uFrm.userBtn.frame]] {
	set wnk $win.$usr
	if [winfo exists $wnk] {return}
	menubutton $wnk -text [$usr name]
	$wnk conf -menu [makeUserMenu $this $wnk.menu $usr]
	if $op { markButton $wnk operator } \
	elseif $sp { markButton $wnk speaker }
	pack $wnk
	set wd [winfo reqwidth $wnk]
	set ht [winfo reqheight $wnk]
	$w.cFrm.uFrm.userBtn conf -width $wd  -scrollincrement $ht
	set ht [expr { $ht * [llength [winfo children $win]]}]
	$w.cFrm.uFrm.userBtn conf -scrollregion [list 0 0 $wd $ht]
    }
    set w $w.users.menu
    if {[catch {set x [indexHack $w [$usr name] 2]}] || $x < 0} {
	$w add cascade -label [$usr name] -menu $w.$usr
	set x last
    }
    if $op { markEntry $w $x operator } \
    elseif $sp { markEntry $w $x speaker }
    makeUserMenu $this $w.$usr $usr
}
#
proc channel_kick {this usr} {
    set chan [$this name]
    set who [$usr name]
    mkDialog {} .@kick {Kick} "Really kick $who from channel $chan?" \
      {{Message {}}} "OK {sendIRC KICK {${chan}} {${who}}}" {Cancel {}}
}
#
proc channel_banKick {this usr} {
    global banInfo
    set banInfo [list $usr $this]
    sendIRC USERHOST [$usr name]
}
#
proc channel_ircOp {this state} {
    if ![string match {} [set w [$this window]]] {
	if [winfo exists $w.cFrm.uFrm.userBtn.frame] {
	    foreach name [winfo children $w.cFrm.uFrm.userBtn.frame] {
		setState $name.menu ircop $state
	    }
	}
	foreach name [winfo children $w.users.menu] {
	    setState $name ircop $state
	}
    }
}
#
proc channel_userMode {this usr mode} {
    case $mode {
    o { global ${this}Op ; set val [set ${this}Op($usr)] }
    v { global ${this}Spk ; set val [set ${this}Spk($usr)] }
    }
    setMode [$this name] [expr {$val ? {+} : {-}}]$mode [$usr name]
}
#
proc channel_setOps {this prefix usr} {
    global MkOp
    foreach n [$this ops]] {
	if [regexp -nocase $n $prefix] {
	    lappend MkOp($this) $usr
	    break
	}
    }
}
#
proc channel_mode {this vals} {
    global ${this}
    set nxt {}
    set flag {}
    foreach par $vals {
	if ![string match {} $nxt] {
	    set flag [string index $nxt 0]
	    set m [string index $nxt 1]
	    set nxt [string range $nxt 2 end]
	    case $m {
	    o   {
		    $this [expr {[string match {+} $flag] ? \
		      "markOp" : "unmarkOp"}] [User :: make $par]
		}
	    v   {
		    $this [expr {[string match {+} $flag] ? \
		      "markV" : "unmarkV"}] [User :: make $par]
		}
	    k   {
		    global Key
		    set Key($this) [expr {[string match {+} $flag] ? $par : {}}]
		}
	    }
	    handleOn MODE [list [$this name] ${flag}${m} $par]
	} {
	    set nxt {}
	    foreach m [split $par {}] {
		case $m {
		[+-] { set flag $m }
		[kovlb] { append nxt ${flag}${m} }
		[psinm] {
			set ${this}($m) [string match {+} $flag]
			handleOn MODE [list [$this name] ${flag}${m}]
		    }
		t   {
			set x [string match {+} $flag]
			set ${this}(t) $x
			if ![$this operator] {
			    set st [expr {$x ? "disabled" : "normal"}]
			    set w [$this window]
			    $w.topic.label conf -state $st
			    $w.topic.entry conf -state $st
			}
			handleOn MODE [list [$this name] ${flag}t]
		    }
		}
	    }
	}
    }
}
#
proc channel_opItems {this state} {
    set win [$this window]
    foreach name [winfo children $win.cFrm.uFrm.userBtn.frame] {
	setState $name.menu chanop $state
    }
    foreach name [winfo children $win.users.menu] {
	setState $name chanop $state
    }
    set mn $win.mode.menu
    set vl [$mn index {Ban}]
    incr vl
    set last [$mn index last]
    while {$vl <= $last} {
	$mn entryconfigure $vl -state $state
	incr vl
    }
    global $this
    if {[string match {normal} $state] || [set ${this}(t)]} {
	$win.topic.entry conf -state $state
	$win.topic.label conf -state $state
    }
}
#
proc Channel_make {name args} {
    global CTO
    set ln [string tolower $name]
    set id [expr {[info exists CTO($ln)] ? $CTO($ln) : [Channel $name]}]
    if ![string match {} $args] { eval $id configure $args }
    return $id
}
#
proc Channel_find {name} {
    global CTO
    set ln [string tolower $name]
    return [expr {[info exists CTO($ln)] ? $CTO($ln) : {nil}}]
}
#
# proc to determine if a message is wanted
#
proc channel_wantMessage {this msg} {
    set cm [$this msg]
    return [expr {[lsearch $cm "!$msg"] >= 0 || [lsearch $cm $msg] < 0}]
}
#
proc mncSave {this desc def} {
    global OType
    set ln "$OType($this) [$this name]"
    if {$this == $def} {
	foreach x {open close jump quiet history buttons} {
	    append ln " -$x [$this $x]"
	}
    } {
	foreach x {open close jump quiet history buttons} {
	    if {[$this $x] != [$def $x]} {
		append ln " -$x [$this $x]"
	    }
	}
    }
    if [$this close] {
	append ln " -closetime [$this closetime]"
    }
    foreach xv {foreground background font geometry height width boldfont} {
	global C$xv
	if [info exists C${xv}($this)] {
	    append ln " -$xv {[set C${xv}($this)]}"
	}
    }
    return $ln
}
#
proc channel_save {this desc} {
    global defChan
    set ln [mncSave $this $desc $defChan]
    foreach op {draw menu join} {
	if [$this $op] { append ln " -$op 1" }
    }
    foreach op {msg ops patterns topics logfile icon} {
	if ![string match {} [set v [$this $op]]] {
	    append ln " -$op {$v}"
	}
    }
    if ![string match {} [$this key]] {
	append ln " -key [$this key]"
    }
    puts $desc $ln
    foreach  b [$this bindings] {
	puts $desc "zbind [$this name] [lindex $b 0] {[lindex $b 1]}"
    }
}
#
proc Channel_save {desc} {
    global defChan
    $defChan save $desc
    foreach ch [Channel :: list] {
	if {$ch != $defChan && [$ch keep]} { $ch save $desc }
    }
}
#
proc Channel_pack {where} {
    foreach ch [Channel :: list] {
	$ch pack $where
    }
}
#
proc channel_pack {this where} {
    if ![$this isa Channel] { return }
    foreach prop {open close jump join draw quiet menu msg \
      history closetime key icon logfile name lname} {
	global ${where}C${prop}
	set ${where}C${prop}($this) [$this $prop]
    }
}
#
proc channel_unpack {this where} {
    foreach prop {name open close jump join draw quiet menu msg \
      history closetime key icon logfile} {
	global ${where}C${prop}
	$this configure -$prop [set ${where}C${prop}($this)]
	unset ${where}C${prop}($this)
    }
}
#
proc Channel_cleanup {where} {
    foreach prop {name open close jump join draw quiet menu msg \
      history closetime key icon logfile} {
	global ${where}C${prop}
	unset ${where}C${prop}
    }
}
#
proc indexHack {w nk start} {
#
# Work round a tk bug
#
    if [regexp {.*\[.*} $nk] {
	set l $start
	$w add command -label {########}
	set lst [$w entryconfigure last -label]
	while {[set val [$w entryconfigure $l -label]] != $lst} {
	    if {[lindex $val 4] == $nk} {
		$w delete last
		return $l
	    }
	    incr l
	}
	$w delete last
	return -1
    } {
	return [$w index $nk]
    }
}
