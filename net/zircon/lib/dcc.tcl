global dccInfo
set dccInfo {}
#
proc acceptChat {mode conn} {
    global monitorIn
    switch $mode {
    r   {
	    if ![catch  {dp_accept $conn} cls] {
		global AChat Chat Cwho Cobj
		set usr $Cwho($conn)
		set cht [Chat [$usr name]]
		$cht show
		$cht addUser $usr 0 0
		set newc [set Chat($cht) [lindex $cls 0]]
		set Cwho($newc) $usr
		set Cobj($newc) $cht
		dp_filehandler $newc re dccChat
		unset Cwho($conn) AChat($usr)
		if $monitorIn { puts stderr "Chat Accept : [$usr name]" }
	    } {
		if $monitorIn { puts stderr "Error on Accept : $conn" }
	    }
	    catch {dp_filehandler $conn}
	    catch {close $conn}
	}
    e   { net0 display ERROR {*** Error on DCC Chat accept}}
    }
}
#
proc DCCSend {usr file} {
    if [string match {} $file] { return }
    if [file exists $file] {
	if ![file readable $file] {
	    mkDialog ERROR .@fe {File error} "Cannot read file $file." \
	      {} {OK {}}
	    return
	}
	set file [glob $file]
	set xfile [file tail $file]
	global zircon hostIPAddress ASend
	set port [split [exec $zircon(lib)/dccsend $file [setInfo] $usr]]
	sendCtcp DCC [$usr name] "SEND $xfile [ipPack $hostIPAddress] [lindex $port 0]"
	lappend ASend($usr) [list [lindex $port 1] $file]
	if [winfo exists .@dcclist] buildDCCList
    } {
	mkDialog ERROR .@fe {File error} "File $file does not exist." \
	  {} {OK {}}
    }
}
#
proc doDCC {cmd nk} {
    if {![string match {} $nk] && ![string match {[#&]*} $nk]} {
	[User :: make $nk] dcc $cmd
    }
}
#
proc closeChat {cht who conn} {
    dp_filehandler $conn
    close $conn
    global Chat Cwho Cobj
    unset Cwho($conn) Cobj($conn) Chat($cht)
    if ![string match {} [info proc $cht]] {
	$cht addText $who "*** $who has closed the connection"
    }
}
#
proc dccChat {mode conn} {
    global Cwho Cobj monitorIn
    set who [$Cwho($conn) name]
    set cht $Cobj($conn)
    switch $mode {
    r   {
	    if {[catch {gets $conn} buffer] || \
	      ([string match {} $buffer] && [eof $conn])} {
		closeChat $cht $who $conn
	    } {
		$cht addText $who "=$who= $buffer"
		if $monitorIn { puts stderr "<= $buffer" }
	    }
	}
    e   {  info0 addText {} "*** Error on DCC Chat connection with $who" }
    }
}

proc handleInfo {mode conn} {
    switch $mode {
    r   {
	    if {[catch {gets $conn} msg] || \
	      ([string match {} $msg] && [eof $conn])} {
	 	catch {dp_shutdown $conn all}
		catch {close $conn}
	    } {
		global ASend Send Get
		set sp [split $msg]
		set who [lindex $sp 5]
		set pid [lindex $sp 0]
		set msg [join [lreplace [lrange $sp 1 end] 4 4 [$who name]]]
		switch -glob -- $msg {
		{DCC Get conn*} -
		{DCC Send acc*} { return }
		{DCC Send conn*} {
			set x [lsearch $ASend($who) "$pid*"]
			lappend Send($who) [lindex $ASend($who) $x]
			listdel ASend($who) $x
			if [string match {} $ASend($who)] {
			    unset ASend($who)
			}
			if [winfo exists .@dcclist] buildDCCList
		    }
		{DCC Send*} {
			set x [lsearch $Send($who) "$pid*"]
			listdel Send($who) $x
			if [string match {} $Send($who)] {
			    unset Send($who)
			}
			if [winfo exists .@dcclist] buildDCCList
		    }
		default {
			set x [lsearch $Get($who) "$pid*"]
			listdel Get($who) $x
			if [string match {} $Get($who)] {
			    unset Get($who)
			}
			if [winfo exists .@dcclist] buildDCCList
		    }
		}
		mkInfoBox DCCINFO .@dcc$conn {DCC Info} $msg {OK {}}
	    }
	}
    e   {  info0 addText {} "*** Error on DCC Info connection." }
    }
}

proc acceptInfo {mode conn} {
    global monitorIn
    case $mode {
    r   {
	    set sk [lindex [dp_accept $conn] 0]
	    dp_filehandler $sk re handleInfo
	    if $monitorIn { puts stderr "Info Accept" }
	}
    e   {  info0 addText {} "*** Error on DCC Info connection (accept)." }
    }
}

proc setInfo {} {
    global dccInfo
    if [string match {} $dccInfo] {
	if [catch {dp_connect -server 0} dccInfo] {
	    info0 addText {} "*** Cannot set up info socket - $dccInfo"
	    return {}
	}
	dp_filehandler [lindex $dccInfo 0] re acceptInfo
    }
    return [lindex $dccInfo 1]
}
#
proc tkerror {args} {
    global errorCode errorInfo
    puts stderr "$errorCode $errorInfo"
}
#
proc doGetDCC {wh usr addr port args} {
    set host [dectonet $addr]
    if [string match {Chat} $wh] {
	if [catch {dp_connect $host $port} val] {
	    info0 addText {} "*** Cannot connect to host $host ($val)"
	    return 0
	}
	set sok [lindex $val 0]
	global Cwho Cobj Chat 
	set Cwho($sok) $usr
	set Cobj($sok) [set this [Chat [$usr name]]]
	$this show
	$this addUser $Cwho($sok) 0 0
	set Chat($this) $sok
	dp_filehandler $sok re dcc${wh}
    } {
	set file [lindex $args 0]
	if [file exists $file] {
	    if ![file writable $file] {
		mkInfoBox {} .@fe {File error} \
		  "Cannot write file $file." {OK {}}
		return
	    }
	}
	global zircon Get
	set file [file dirname $file]/[file tail $file]
	set pid [exec $zircon(lib)/dccget $host $port $file [setInfo] $usr]
	lappend Get($usr) [list $pid $file]
    }
}

proc handleDCC {usr param} {
#    global DCCReq
    set pars [split $param]
    case [lindex $pars 1] {
    SEND {
	    if [string match ".*" [set fln [lindex $pars 2]]] {
		set fln _[string range $fln 1 end]
	    }
	    set addr [lindex $pars 3]
	    set port [lindex $pars 4]
#	    lappend DCCReq [list Send $usr $fln $addr $port]
	    set msg "DCC Send request ($fln) received from [$usr name]"
	    mkFileBox .@dcc "DCC Send $fln" $msg $fln \
	      "Accept {doGetDCC Get $usr $addr $port}" {Cancel {}}
	}  
    CHAT {
	    set addr [lindex $pars 3]
	    set port [lindex $pars 4]
#	    lappend DCCReq [list Chat $usr $addr $port]
	    set msg "DCC Chat request received from [$usr name]"
	    mkDialog {} .@dcc "DCC Chat Request" $msg {} \
	      "Accept {doGetDCC Chat $usr $addr $port}" {Cancel {}}
	}
    }
}
#
proc ipPack {ip} {
    set val 0
    foreach x [split $ip "."] {
	set val [expr {($val << 8) + $x}]
    }
    return [format %u $val]
}
#
proc dectonet {dec} {
    if {[string length $dec] == 10 && [set first [string index $dec 0]] > 1} {
	case $first {
	    2 {set overflow "0 148 53 119"}
	    3 {set overflow "0 94 208 178"}
	    4 {set overflow "0 40 107 238"}
	}
	set dec [string range $dec 1 end]
    } else {
	set overflow {0 0 0 0}
    }   

    scan [format "%08x" $dec] "%2x%2x%2x%2x" net(3) net(2) net(1) net(0)

    for {set part 0; set carry 0} {$part < 4} {incr part} {
	set sum [expr {$net($part) + [lindex $overflow $part] + $carry}]
	set internet($part) [expr {$sum % 256}]
	set carry [expr {$sum / 256}]
    }

    return "$internet(3).$internet(2).$internet(1).$internet(0)"
}
#
proc killDel {arr usr file} {
    global $arr
    set i 0
    foreach p [set ${arr}($usr)] {
	if {[lindex $p 1] == $file} {
	    catch {exec kill [lindex $p 0]}
	    listdel ${arr}($usr) $i
	    if [string match {} [set ${arr}($usr)]] { unset ${arr}($usr) }
	    return
	}
	incr i
    }
}
#
proc dccClose {win} {
    foreach t [$win curselection] {
	set x [split [$win get $t]]
	set usr [User :: find [set who [lindex $x 2]]]
	set file [lindex $x 4]
	switch -glob -- [lindex $x 0] {
	{Call to*} {$usr unChat }
	{Call from*} { }
	Chat* {[Chat :: find $who] leave}
	Offer* { killDel ASend $usr $file }
	Request* { }
	Send* { killDel Send $usr $file }
	Get* { killDel Get $usr $file }
	}
    }
    foreach t [$win curselection] { $win delete $t }
}
#
proc buildDCCList {args} {
    set w .@dcclist
    if [winfo exists $w] {
	popup $w
	if [string match {} $args] { $oFrm.dcc.l delete 0 end }
    } {
	toplevel $w -class Zircon -relief raised -borderwidth 2
	wm title $w {DCC Connections}
	wm minsize $w 10 1
	makeLB $w.dcc -setgrid 1
	frame $w.btn
	button $w.btn.ok -text OK -command {destroy .@dcclist} -relief raised
	button $w.btn.clear -text Close -relief raised \
	  -command { dccClose .@dcclist.dcc.l }
	pack $w.btn.ok $w.btn.clear -side left -expand 1 -fill x
	pack $w.dcc -fill both
	pack $w.btn -fill x
    }
    global AChat Chat ASend Send Get
    foreach nn [array names AChat] {
	$w.dcc.l insert end "Call to [$nn name]"
    }
    foreach nn [array names Chat] {
	$w.dcc.l insert end "Chat to [$nn name]"
    }
    foreach nn [array names ASend] {
	foreach fl $ASend($nn) {
	    $w.dcc.l insert end "Offer to [$nn name] : [lindex $fl 1]"
	}
    }
    foreach nn [array names Send] {
	foreach fl $Send($nn) {
	    $w.dcc.l insert end "Send to [$nn name] : [lindex $fl 1]"
	}
    }
    foreach nn [array names Get] {
	foreach fl $Get($nn) {
	    $w.dcc.l insert end "Get from [$nn name] : [lindex $fl 1]"
	}
    }
}
#
proc usersDCC {cmd} {
    switch $cmd {
    List -
    Close { buildDCCList }
    default {
	    mkEntryBox .@$cmd $cmd "Enter user name for DCC $cmd:" \
	      {{User {}}} "OK {doDCC [string toupper $cmd]}" {Cancel {}}
	}
    }
}
