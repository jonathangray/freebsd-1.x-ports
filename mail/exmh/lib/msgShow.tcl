# msgShow.tcl
#
# Message display.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Msg_Setup { t } {
    global msg
    foreach tagname $msg(tagnames) {
	# Set up looks for different header lines
	set rval [option get . m_$tagname {}]
	if {$rval != {}} {
	    set msg(tag,$tagname) $rval
	    if [catch {
		eval {$t tag configure $tagname} $rval
	    } err] {
		Exmh_Debug tag configure $tagname $rval: $err
		unset msg(tag,$tagname)
	    }
	}
    }
    $t tag raise sel
}

proc MsgShow { msgid } {
    # Display the current message in a text widget
    global msg exwin exmh mhProfile msgHdr

    if {$msg(dpy) == $msgid} {
	return
    }
    Label_Message $exmh(folder):$msgid
    if [MsgShowInText $exwin(mtext) $mhProfile(path)/$exmh(folder)/$msgid] {
	MsgSeen $msgid
	set msg(dpy) $msgid
	set msg(curclear) 0
	update idletasks	;# Faces display can be slow
	Face_Show [MsgParseFrom $msgHdr(from)] $msgHdr(x-face)
    }
}
proc MsgShowInText { win file } {
    global mhProfile msgHdr msgHdrs msg
    $win configure -state normal
    $win delete 0.0 end

    if [info exists mhProfile(exmhshowproc)] {
	Exmh_Debug MsgShowInText $mhProfile(exmhshowproc) $file
	set filen [concat "|" $mhProfile(exmhshowproc) $file]
    } else {
	set filen $file
    }
    if [catch {open $filen "r"} showcmd] {
	$win insert end "Cannot open $file: $showcmd"
	return 0
    }
    catch {unset msgHdr}
    catch {unset msgHdrs}
    set msgHdr(cur) {}
    set msgHdr(from) {}
    set msgHdr(x-face) {}
    set hideMark 1.0
    while {1} {
	set numBytes [gets $showcmd input]
	if {$numBytes < 0} {
	    break
	}
	if {$numBytes == 0} {
	    $win insert end "\n"
	    break
	}
	if [regexp -- {^-*$} $input] {
	    $win insert end $input\n
	    break
	}
	set continue [regexp {^[ 	]} $input]
	if {! $continue} {
	    	    if [regexp -indices {^([^:]+):} $input match hdr] {
		set msgHdr(cur) [string tolower \
		    [eval {string range $input} $hdr]]
		append msgHdr($msgHdr(cur)) \
		    [string trim \
			[string range $input [expr [lindex $match 1]+1] end]]
		lappend msgHdrs $msgHdr(cur)
	    }
	    set show 1
	    foreach item $mhProfile(header-suppress) {
		if [regexp -nocase ^${item}\$ $msgHdr(cur)] {
		    set show 0
		    break
		}
	    }
	    foreach item $mhProfile(header-display) {
		if [regexp -nocase ^${item}\$ $msgHdr(cur)] {
		    set show 1
		    break
		}
	    }
	} else {
	    if [regexp -indices {^[ 	]+} $input match] {
		append msgHdr($msgHdr(cur)) \
		    [string range $input [expr [lindex $match 1]+1] end]
	    }
	}
	if $show {
	    $win insert end $input\n
	    set look end
	    set default general
	} else {
	    $win insert $hideMark $input\n
	    set hideMark [$win index "$hideMark + 1 lines"]
	    set look $hideMark
	    set default hidden
	}
	foreach key [list $msgHdr(cur) $default] {
	    if [info exists msg(tag,$key)] {
		$win tag add $key "$look -1lines" "$look -1 lines lineend"
		break
	    }
	}
    }
    $win yview $hideMark
    if {![info exists msgHdr(mime-version)] || ![Mime_Enabled]} {
	$win insert end [read $showcmd]
	Exmh_Status text/plain
    } else {
	global exmh msg mhProfile
	if ![info exists msgHdr(content-type)] {
	    set msgHdr(content-type) text/plain
	    set type text/plain
	} else {
	    set params [split $msgHdr(content-type) \;]
	    set type [string tolower [string trim [lindex $params 0]]]
	}
	if [info exists msgHdr(content-transfer-encoding)] {
	    set encoding [string trim [string tolower \
			    $msgHdr(content-transfer-encoding)] \ \" ]
	} else {
	    set encoding 7bit
	}
	# Hack to optimize text/plain
	if {$type == "text/plain" &&
	    ($encoding == "7bit" || $encoding == "8bit")} {
	    $win insert end [read $showcmd]
	    Exmh_Status text/plain
	} else {
	    Mime_Cleanup	;# tmp files from last message.
	    set fileName [Mime_TempFile /tmp/exmh.body]
	    if [catch {open $fileName w} out] {
		Exmh_Status "Cannot open $fileName"
		$win insert end [read $showcmd]
	    } else {
		puts -nonewline $out [read $showcmd]
		close $out
		busy Mime_ShowBody $win $fileName $msgHdr(content-type) $encoding
	    }
	}
    }
    close $showcmd
    $win configure -state disabled
    return 1
}

proc MsgParseFrom { fromline } {
    set result {}
    set line [string trim $fromline]
    if [regsub {\(.*\)} $line {} newline] {
	set line $newline
    }
    if [regexp {[^ 	"]*@[^ 	"]*} $line token] {
	set token [string trim $token <>]
    } else {
	if [regexp {<.*>} $line token] {
	    set token [string trim $token <>]
	} else {
	    if [catch {lindex $line 0} token] {
		set token {}
		Exmh_Debug MsgParseFrom failed on: $fromline
	    }
	}
    }
    return $token
}


