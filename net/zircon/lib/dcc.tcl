global dccInfo
set dccInfo {}

proc acceptChat {mode conn} {
    global Chat
    case $mode {
    r {
	    global ${conn}Who
	    set nk [set ${conn}Who]
	    set Chat($nk) [lindex [dp_accept $conn] 0]
	    global $Chat($nk)Who ; set $Chat($nk)Who $nk
	    catch "dp_filehandler $conn"
	    catch "close $conn"
	    unset ${conn}Who
	    makeChannel $nk D
	    dp_filehandler $Chat($nk) re dccChat
	    global AChat ; unset AChat($nk);
	}
    e { addText ERROR @info {*** Error on DCC Chat accept}}
    }
}

proc insertDCCSelect {chan ent} {
    if {[catch {set bf [selection get]}] == 0} {
	while {[set nl [string first "\012" $bf]] >= 0} {
	    $ent insert insert [string range $bf 0 [incr nl -1]]
	    tk_entrySeeCaret $ent
	    sendToDCC $chan [$ent get]
	    $ent delete 0 end
	    set bf [string range $bf [incr nl 2] end]
	}
	if {$bf != ""} { $ent insert insert $bf ; tk_entrySeeCaret $ent }
    }
}
proc sendToDCC {chan string args} {
    if {$string != ""} {
	global Chat
	if [info exists Chat($chan)] {
	    if [catch [list puts $Chat($chan) $string] err] {
		addText {} ~${chan} "*** Error : $err"
	    } {
		flush $Chat($chan)
		addText @me ~${chan} "= $string"
	    }
	} {
	    addText {} ~${chan} {*** Connection is closed!!!!}
	}
    }
}

proc doDCCLeave {chan} {
    killChannel ~${chan}
    global Chat
    if [info exists Chat($chan)] {
	set sock $Chat($chan)
	catch "dp_filehandler $sock"
	catch "close $sock"
	global $Chat($chan)Who ; unset $Chat($chan)Who
	unset Chat($chan)
    }
}

proc leaveDCC {chan} {
    mkDialog LEAVE .@${chan} "Leave ${chan}" \
      "Really leave DCC chat with ${chan}?" {} \
      "OK {doDCCLeave ${chan}}" {Cancel {}}
}

proc DCCSend {nk file} {
    if {$file == {}} return
    if [file exists $file] {
	if ![file readable $file] {
	    mkDialog ERROR .@fe {File error} "Cannot read file $file." \
	      {} {OK {}}
	    return
	}
	set file [glob $file]
	set xfile [file tail $file]
	global zircon
	global hostIPAddress
	set port [split [exec $zircon(lib)/dccsend $file [setInfo] $nk]]
	sendCtcp DCC $nk "SEND $xfile [ipPack $hostIPAddress] [lindex $port 0]"
	global ASend ; lappend ASend($nk) [list [lindex $port 1] $file]
	if [winfo exists .@dcclist] buildDCCList
    } {
	mkDialog ERROR .@fe {File error} "File $file does not exist." \
	  {} {OK {}}
    }
}

proc doDCC {cmd nk} {
    if {$nk == {}} return
    set lnk [string tolower $nk]
    case $cmd {
    SEND {
	    mkFileBox .@dccSend$nk "Send $nk" "File to send to $nk" {}\
	      "Send {DCCSend $lnk}" {Cancel {}}
	}
    CHAT {
	    global AChat;
	    if [info exist AChat($lnk)] {
		mkDialog {} .@chat$nk "Chat" \
		  "You already have a chat request open to $nk."  \
		  {} \
		  "Close {unChat $lnk}" {Keep {}}
	    } {
		global hostIPAddress
		if ![catch {dp_connect -server {} 0} sk] {
		    set sock [lindex $sk 0]
		    global ${sock}Who ; set ${sock}Who $lnk
		    global AChat ; set AChat($lnk) $sock
		    dp_filehandler $sock re acceptChat
		    sendCtcp DCC $lnk \
		      "CHAT chat [ipPack $hostIPAddress] [lindex $sk 1]"
		} {
		    addText ERROR @info "*** $host : $sk"
		}
	    }
	}
    }
}

proc closeChat {who conn} {
    if [winfo exists .~$who] {
	addText $who ~$who "*** $who has closed the connection"
    }
    catch "dp_filehandler $conn"
    catch "close $conn"
    global ${conn}Who ; catch "unset ${conn}Who"
    global Chat ; catch "unset Chat($who)"
}

proc dccChat {mode conn} {
    global ${conn}Who
    set who [set ${conn}Who]
    case $mode in {
    r   {
	    if {[catch "gets $conn" buffer] || [eof $conn]} {
		closeChat $who $conn
	    } {
		addText ${who} ~${who} "=$who= $buffer"
	    }
	}
    e   {  addText {} @info "*** Error on DCC Chat connection with $who" }
    }
}

proc handleInfo {mode conn} {
    case $mode {
    r   {
	    if {[catch "gets $conn" msg] || [eof $conn]} {
	 	catch "dp_shutdown $conn all"
		catch "close $conn"
	    } {
		global ASend
		global Send
		global Get
		set sp [split $msg]
		set who [lindex $sp 5]
		set pid [lindex $sp 0]
		set msg [join [lrange $sp 1 end]]
		case $msg {
		{{DCC Send acc*}} { return }
		{{DCC Send conn*}} {
			set x [lsearch $ASend($who) "$pid*"]
			lappend Send($who) [lindex $ASend($who) $x]
			listdel ASend($who) $x
			if {$ASend($who) == {}} {
			    unset ASend($who)
			}
			if [winfo exists .@dcclist] buildDCCList
		    }
		{{DCC Get conn*}} { return }
		{{DCC Send*}} {
			set x [lsearch $Send($who) "$pid*"]
			listdel Send($who) $x
			if {$Send($who) == {}} {
			    unset Send($who)
			}
			if [winfo exists .@dcclist] buildDCCList
		    }
		default {
			set x [lsearch $Get($who) "$pid*"]
			listdel Get($who) $x
			if {$Get($who) == {}} {
			    unset Get($who)
			}
			if [winfo exists .@dcclist] buildDCCList
		    }
		}
		mkInfoBox DCCINFO .@dcc$conn {DCC Info} $msg {OK {}}
	    }
	}
    e   {  addText {} @info "*** Error on DCC Info connection." }
    }
}

proc acceptInfo {mode conn} {
    case $mode {
    r   {
	    set sk [lindex [dp_accept $conn] 0]
	    dp_filehandler $sk re handleInfo
	}
    e   {  addText {} @info "*** Error on DCC Info connection (accept)." }
    }
}

proc setInfo {} {
    global dccInfo
    if {$dccInfo == {}} {
	if [catch {dp_connect -server {} 0} dccInfo] {
	    addText {} @info "*** Cannot set up info socket - $dccInfo"
	    return {}
	}
	dp_filehandler [lindex $dccInfo 0] re acceptInfo
    }
    return [lindex $dccInfo 1]
}

proc doGetDCC {wh lnk addr port args} {
    set host [dectonet $addr]
    if {$wh == "Chat"} {
	if [catch "dp_connect $host $port" val] {
	    addText {} @info "*** Cannot connect to host $host ($val)"
	    return 0
	}
	set sok [lindex $val 0]
	global ${sok}Who ; set ${sok}Who $lnk
	makeChannel $lnk D
	global Chat ; set Chat($lnk) $sok
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
	global zircon
	set file [file dirname $file]/[file tail $file]
	set pid [exec $zircon(lib)/dccget $host $port $file [setInfo] $lnk]
	global Get ; lappend Get($lnk) [list $pid $file]
    }
}

proc handleDCC {nk lnk param} {
    set pars [split $param]
    case [lindex $pars 1] {
    SEND {
	    if [string match ".*" [set fln [lindex $pars 2]]] {
		set fln _[string range $fln 1 end]
	    }
	    set addr [lindex $pars 3]
	    set port [lindex $pars 4]
	    set msg "DCC Send request ($fln) received from $nk"
	    mkFileBox .@dcc "DCC Get $fln" "$msg" $fln \
	      "Accept {doGetDCC Get $lnk $addr $port}" {Cancel {}}
	}  
    CHAT {
	    set addr [lindex $pars 3]
	    set port [lindex $pars 4]
	    set msg "DCC Chat request ([lindex $pars 2]) received from $nk"
	    mkDialog {} .@dcc "DCC Chat Request" "$msg" {} \
	      "Accept {doGetDCC Chat $lnk $addr $port}" {Cancel {}}
	}
    }
}

proc ipPack {ip} {
    set val 0
    foreach x [split $ip "."] {
	set val [expr {($val << 8) + $x}]
    }
    return [format %u $val]
}

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

proc killDel {arr who file} {
    global $arr
    set i 0
    foreach p [set ${arr}($who)] {
	if {[lindex $p 1] == $file} {
	    catch "exec kill [lindex $p 0]"
	    listdel ${arr}($who) $i
	    if {[set ${arr}($who)] == {}} { unset ${arr}($who) }
	    return
	}
	incr i
    }
}

proc unChat who {
    global AChat
    global $AChat($who)Who
    unset $AChat($who)Who
    catch "dp_filehandler $AChat($who)"
    catch "close $AChat($who)"
    unset AChat($who)
}

proc dccClose win {
    foreach t [$win curselection] {
	set x [split [$win get $t]]
	set who [lindex $x 2]
	set file [lindex $x 4]
	case [lindex $x 0] {
	Call* {unChat $who }
	Chat* {doDCCLeave $who}
	Offer* { killDel ASend $who $file }
	Send* { killDel Send $who $file }
	Get* { killDel Get $who $file }
	}
    }
    foreach t [$win curselection] { $win delete $t }
}
