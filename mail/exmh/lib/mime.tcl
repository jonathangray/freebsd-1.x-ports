# mime.tcl
#
# MIME message display.
#
# Thanks to Chris Garrigues who tested and improved this code.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Mime_Init {} {
    global mime env
    if [info exists mime(init)] {
	return
    }
    # Make sure Metamail is on the path
    set hit 0
    foreach dir [split $env(PATH) :] {
	if {[string compare $dir $mime(dir)] == 0} {
	    set hit 1
	    break
	}
    }
    if {! $hit} {
	set env(PATH) $mime(dir):$env(PATH)
    }

    set mime(init) 1
    set mime(seed) 1
    set mime(junkfiles) {}

    set types [concat [option get . mimeTypes {}] [option get . mimeUTypes {}]]
    Exmh_Debug MimeTypes $types
    set mime(showproc,default) 			Mime_ShowDefault
    if {[llength $types] == 0} {
	set mime(showproc,text) 			Mime_ShowText
	set mime(showproc,multipart) 			Mime_ShowMultipart
	set mime(showproc,application/octet-stream) 	Mime_ShowApplicationOctet
	set mime(showproc,application/postscript)   	Mime_ShowPostscript
	set mime(showproc,message/external-body)	Mime_ShowMessageExternal
	set mime(showproc,message/rfc822)		Mime_ShowRfc822
	set mime(showproc,image) 			Mime_ShowDirect
	set mime(showproc,audio) 			Mime_ShowAudio
    } else {
	foreach type $types {
	    set func [option get . mime_$type {}]
	    if {$func != {}} {
		set mime(showproc,$type) $func
	    }
	}
    }

    Preferences_Add "MIME" \
"MIME is the Multipurpose Internet Mail Extension that allows a variety of message types to be transfered and displayed for you." {
	{mime(enabled) mimeEnabled	ON {Enable MIME display}
"This controls whether or not MIME-format messages are parsed
and displayed.  If it is disabled, then the messages are
displayed as plain text."}
	{mime(showType) mimeShowType	ON {Show MIME types}
"This controls whether or not the MIME type information for each
part of a multi-part message is displayed.  The type is prefixed
with -- to offset it from the rest of the information."}
	{mime(showPrelude) mimeShowPrelude	OFF {Show MIME prelude}
"This controls whether or not the information between the mail headers
and the official start of a multipart message is displayed.  Sometimes
this has useful information, while other times it has warnings about
the rest of the message being in MIME format."}
	{mime(ftpMethod) ftpMethod	{CHOICE expect ftp {ftp -n} metamail}
	    {FTP access method}
"Sometimes the automatic FTP transfer fails because of
problems logging into the remote host.  This option lets
you try a few different approachs to find the one that
works for you:
expect - use the ftp.expect script to download the file.
ftp - use ftp and feed user and password to it via a pipe.
ftp -n - use the ftp no-auto-login feature.
metamail - pass control to metamail and let it try."}
	{mime(ftpCommand) ftpCommand	ftp {FTP command name}
"You may need to run a different command than \"ftp\" in
order to get out onto the internet from inside your company
network.  The command will be invoked with the site name as
the only argument, possibly with the -n flag, which depends on
your choice of the FTP access method.  If you use the expect
script, you'll have to tweak that by hand."}
	{mime(isoFont) isoFont	-*-lucida-*-*-*-*-*-*-*-*-*-*-iso8859-* {ISO-8859 font}
"This font is used to display MIME text that is in the
ISO-8859-1 character set.  This kicks in regardless of your
font setting for the message display window.  Override this
feature by clearing out the font name."}
	{mime(psViewer) psViewer ghostview {Postscript Viewer}
"This program is used to display postscript on your
display (as opposed to printing it).  Options include
pageview (SunOS-specific) and ghostview."}
    }
}
proc Mime_Enabled {} {
    global mime
    return $mime(enabled)
}
proc Mime_Cleanup {} {
    global mime
    set cmd rm
    foreach f $mime(junkfiles) {
	if [file exists $f] {
	    lappend cmd $f
	}
    }
    if {$cmd != "rm"} {
	eval exec $cmd
    }
    set mime(junkfiles) {}
}

proc Mime_ShowBody { tkw fileName contentType encoding} {
    global mime mimeHdr
    catch {unset mimeHdr}
    Exmh_Status "Formatting MIME message ..." red
    set mimeHdr(0,0,file) $fileName
    MimeHeader 0 0 $contentType $encoding

    set start [$tkw index insert]
    $tkw tag add 0 $start

    set type [lindex [split $contentType \;] 0]
    set menu [MimeMakeMenu $tkw 0 0 0 "Message" $type]

    MimeShow $tkw 0 0 0 $menu

    set end [$tkw index insert]
    $menu add command -label "Dismiss" -command "$menu unpost"
    $tkw tag add 0 $start $end
    $tkw tag lower 0
    Exmh_Status $type
}
proc MimeShow {tkw depth part justDoIt menu {color default}} {
    global mime mimeHdr
    set color [MimeColor $tkw $color]
    set fileName $mimeHdr($depth,$part,file)
    set type $mimeHdr($depth,$part,type)
    foreach t [list $type [file dirname $type] default] {
	if [info exists mime(showproc,$t)] {
	    if [$mime(showproc,$t) $tkw $depth $part $justDoIt $menu $color] {
		Exmh_Debug "MimeShow should remove $fileName"
#		exec rm $fileName
	    }
	    return
	}
    }
}
proc MimeColor { tkw color } {
    if [regexp default $color] {
	set color [lindex [$tkw configure -background] 4]
    }
    set rgb [winfo rgb $tkw $color]
    return [format #%04x%04x%04x \
	[lindex $rgb 0] [lindex $rgb 1] [lindex $rgb 2]]
}

proc MimeHeader { depth part contentType encoding } {
    global mimeHdr
    # mimeHdr contains state about nested body parts:
    # mimeHdr($depth,$part,type)	Content-Type
    # mimeHdr($depth,$part,encoding)	Content-Transfer-Encoding
    # mimeHdr($depth,$part,file)	Tmp file containing body
    # mimeHdr($depth,$part,params)	Parameter names (e.g., boundary)
    # mimeHdr($depth,$part,param,$key)	Parameter value
    # mimeHdr($depth,$part,hdrs)	List of subpart mail headers
    # mimeHdr($depth,$part,hdr,$key)	Subpart mail header value

    set params [split $contentType \;]
    set type [string tolower [string trim [lindex $params 0]]]
    if {$type == "text"} {
	set type text/plain
    }
    set mimeHdr($depth,$part,hdr,content-type) $contentType
    set mimeHdr($depth,$part,type) $type
    set mimeHdr($depth,$part,encoding) $encoding
    set mimeHdr($depth,$part,params) {}
    foreach sub [lrange $params 1 end] {
	set sub [string trim $sub]
	if [regexp {([^=]+)=(.+)} $sub match key val] {
	    set key [string trim [string tolower $key]]
	    set val [string trim $val \ \"]
	    lappend mimeHdr($depth,$part,params) $key
	    set mimeHdr($depth,$part,param,$key) $val
	}
    }
    return $type
}

proc Mime_ShowDefault { tkw depth part justDoIt menu color args} {
    global mimeHdr

    set type $mimeHdr($depth,$part,type)
    $tkw insert insert "You have received a $type\n"

    MimeInsertInfo $tkw $depth $part

    if $justDoIt {
	MimeMetaMail $mimeHdr($depth,$part,hdr,content-type) \
		     $mimeHdr($depth,$part,encoding) \
		     $mimeHdr($depth,$part,file)
    } else {
	$tkw insert insert "(Invoke menu with right button.)\n"
    }
    return 0
}
proc Mime_ShowDirect { tkw depth part justDoIt menu color} {
    return [Mime_ShowDefault $tkw $depth $part $justDoIt $menu $color -e -d]
}
proc MimeInsertInfo { tkw depth part } {
    global mimeHdr
#    if [info exists mimeHdr($depth,$part,hdr,content-description)] {
#	    $tkw insert insert \
#		"Description: $mimeHdr($depth,$part,hdr,content-description) "
#    }
#    $tkw insert insert "($mimeHdr($depth,$part,type))\n"
    foreach key $mimeHdr($depth,$part,params) {
	$tkw insert insert "\t$key = $mimeHdr($depth,$part,param,$key)\n"
    }
}

proc MimeMetaMail { contentType encoding fileName args } {
    global mimeHdr msgHdr
    if [catch {
	Exmh_Status "metamail $fileName -c $contentType ..."
	set mcmd [list metamail -b\
		    -c $contentType \
		    -E $encoding \
		    -f [MsgParseFrom $msgHdr(from)] \
		    -m exmh ]
	if [regexp -nocase {text} $contentType] {
	    lappend args -p
	} else {
	    lappend args -B
	}
	# recall that eval concats its arguments, thus exploding things for us
	Exmh_Debug [concat exec $mcmd $args $fileName]
	eval exec $mcmd $args $fileName < /dev/null > /dev/null &
    } err] {
	 Exmh_Status "$err"
    }
}

proc Mime_ShowText { tkw depth part justDoIt menu color} {
    global mimeHdr mime
    set charset us-ascii
    if [info exists mimeHdr($depth,$part,param,charset)] {
	set charset $mimeHdr($depth,$part,param,charset)
	switch -regexp $charset {
	    (us-ascii|US-ASCII) { set charset us-ascii }
	    (iso|ISO)-8859-1 { set charset iso-8859-1 }
	    default { $tkw insert insert "(Warning: unknown charset <$charset>)\n\n" }
	}
    }
    set subtype [file tail $mimeHdr($depth,$part,type)]
    set encoding $mimeHdr($depth,$part,encoding)
    set fileName $mimeHdr($depth,$part,file)
    if ![regexp {[78]bit} $encoding] {
	set newFile [Mime_TempFile /tmp/exmh.decode]
	if ![MimeDecode $mimeHdr($depth,$part,file) $newFile $encoding] {
	    $tkw insert insert "(Decode failed - raw text follows)\n\n"
	    set subtype plain
	} else {
	    set fileName $newFile
	}
    }
#    Exmh_Status text/$subtype
    if [catch {open $fileName r} fileIO] {
	Exmh_Status "Cannot open body $fileName: $fileIO"
	return 1
    }
    case $subtype {
	default {
	    if {$subtype != "plain"} {
		$tkw insert insert "(Unknown text subtype of '$subtype'; metamail may be able to do more.
  Invoke menu with right button.)\n"
	    }
	    set start [$tkw index insert]
	    $tkw insert insert [read $fileIO]
	    if {$charset == "iso-8859-1"} {
		$tkw tag add ISO $start [$tkw index insert]
		if [catch {$tkw tag configure ISO -font $mime(isoFont)} err] {
		    Exmh_Debug Mime_ShowText $err
		}
	    }
	}
	{enriched richtext} { Rich_Display $tkw $fileIO $subtype}
    }
    close $fileIO
    if [info exists newFile] {
	exec rm $newFile
    }
    return 1
}
proc Mime_ShowRfc822 { tkw depth part justDoIt menu color} {
    global mimeHdr
    Exmh_Debug Mime_ShowRfc822 $depth $part
    set fileName $mimeHdr($depth,$part,file)
    if [catch {open $fileName r} fileIO] {
	Exmh_Status "Cannot open body $fileName: $fileIO"
	return 1
    }

    # "Chop" up the file, which will only be a single message,
    # but this fills out the mimeHdr data structure and our
    # MimeShowPart below will handle
    # the possible recursion if the message is itself a multi-part.

    set np [MimeMultiChop $tkw $depth $part $fileIO {} text/plain]
    close $fileIO
    Exmh_Debug RFC822 MimeMultiChop got $np parts
    for {set p 1} {$p <= $np} {incr p} {
	if [info exists mimeHdr($depth-$part,$p,hdrs)] {
	    foreach hdr $mimeHdr($depth-$part,$p,hdrs) {
		$tkw insert insert "$hdr: $mimeHdr($depth-$part,$p,hdr,$hdr)\n"
	    }
	}
#	$tkw insert insert \n
	MimeShowPart $tkw $depth-$part $p $justDoIt $color 1
    }
    return 1
}
proc MimeDecode { fileName name encoding } {
    set ok 1
    if [catch {
	case $encoding {
	    default {
		Exmh_Status "cat > $name"
		exec cat < $fileName > $name
	    }
	    {8bit 7bit} {
		Exmh_Status "cat > $name"
		exec cat < $fileName > $name
	    }
	    base64 {
		Exmh_Status "mimencode -u -b -p > $name"
		exec mimencode < $fileName -u -b -p > $name
	    }
	    quoted-printable {
		Exmh_Status "mimencode -u -q > $name"
		exec mimencode < $fileName -u -q > $name
	    }
	}
    } err] {
	Exmh_Status "Decode failed: $err"
	set ok 0
    }
    return $ok
}

proc Mime_ShowApplicationOctet { tkw depth part justDoIt menu color} {
    global mimeHdr

    $tkw insert insert "You have received an encoded file.\n"
    MimeInsertInfo $tkw $depth $part
    if [info exists mimeHdr($depth,$part,param,name)] {
	set name [string trim $mimeHdr($depth,$part,param,name) \ \" ]
    } else {
	set name {}
    }
    $menu add command \
	-label "Open file transfer dialog..." \
	-command [list MimeFileTransfer $mimeHdr($depth,$part,file) $name \
				$mimeHdr($depth,$part,encoding)]
    return 0
}
proc Mime_ShowPostscript { tkw depth part justDoIt menu color} {
    global mimeHdr

    $tkw insert insert "You have received a postscript file.\n"
    MimeInsertInfo $tkw $depth $part
    if [info exists mimeHdr($depth,$part,param,name)] {
	set name [string trim $mimeHdr($depth,$part,param,name) \ \" ]
    } else {
	set name {}
    }
    $tkw insert insert "(Invoke menu with right button.)\n"

    $menu add command \
	-label "Start Postscript viewer ..." \
	-command [list MimePsView $mimeHdr($depth,$part,file) \
			$mimeHdr($depth,$part,encoding)]
    return 0
}

proc MimePsView { fileName encoding } {
    global mime
    set name [Mime_TempFile /tmp/ps ]
    if {$name != {}} {
	if [MimeDecode $fileName $name $encoding] {
	    Exmh_Status "$mime(psViewer) $name"
	    exec /bin/sh -c "$mime(psViewer) $name ; rm $name" &
	}
    }
}

proc MimeFileTransfer { fileName name encoding } {
    set name [FSBox "Select the name of the downloaded file:" $name ]
    if {$name != {}} {
	if [MimeDecode $fileName $name $encoding] {
	    Exmh_Status "Conversion ok"
	}
    }
}
proc Mime_ShowAudio { tkw depth part justDoIt menu color} {
    global mimeHdr

    $tkw insert insert "You have received an audio file\n"
    $tkw insert insert "(Invoke menu with right button.)\n"
    MimeInsertInfo $tkw $depth $part

    $menu add command \
	-label "Play audio..." \
	-command [list MimeDoAudio $mimeHdr($depth,$part,file) \
			  $mimeHdr($depth,$part,encoding) $depth $part]

    if $justDoIt {
	MimeDoAudio $mimeHdr($depth,$part,file) \
	  $mimeHdr($depth,$part,encoding) $depth $part
    }
    return 0
}
proc MimeDoAudio { fileName encoding depth part } {
    global mimeHdr
    set msg [Exmh_OldStatus]
    if [info exists mimeHdr($depth,$part,audio)] {
	Exmh_Status "Playing audio..."
	exec showaudio < $mimeHdr($depth,$part,audio)
	Exmh_Status $msg
    } else {
	set name [Mime_TempFile /tmp/exmh.audio]
	if [MimeDecode $fileName $name $encoding] {
	    Exmh_Status "Playing audio..."
	    exec showaudio < $name
	    set mimeHdr($depth,$part,audio) $name
	    Exmh_Status $msg
	}
    }
}
proc Mime_ShowMessageExternal { tkw depth part justDoIt menu color} {
    global mimeHdr mime

    if [catch {set mimeHdr($depth,$part,param,access-type)} atype] {
	return [Mime_ShowDefault $tkw $depth $part $JustDoIt $menu $color]
    }
    if {$atype != "anon-ftp"} {
	return [Mime_ShowDefault $tkw $depth $part $justDoIt $menu $color]
    }
    MimeInsertInfo $tkw $depth $part
    $tkw insert insert \n

    $menu add command \
	-label "Open FTP transfer dialog..." \
	-command [list MimeFTPTransfer $tkw $depth $part ]

    return 0
}
proc MimeFTPTransfer { tkw depth part } {
    global mime mimeHdr





    set site $mimeHdr($depth,$part,param,site)
    set directory $mimeHdr($depth,$part,param,directory)
    set theirname $mimeHdr($depth,$part,param,name)
    if {$mime(ftpMethod)  == "metamail"} {
	set myname foo
    } else {
	set myname [FSBox "Select the name of the downloaded file:" $theirname ]
    }
    if {$myname != {}} {
	if [catch {
	    case $mime(ftpMethod) {
		expect {
		    Exmh_Status "ftp.expect $site ..."
		    busy exec ftp.expect $site $directory $theirname $myname
		}
		ftp* {
		    Exmh_Status "$mime(ftpCommand) -n $site ..."
		    busy MimeFTPInner $site $directory $theirname $myname
		}
		metamail {
		    MimeMetaMail $mimeHdr($depth,$part,hdr,content-type) \
			   $mimeHdr($depth,$part,encoding) \
			   $mimeHdr($depth,$part,file) \
			   -d
		}
		default {
		    error "Unknown ftpMethod $mime(ftpMethod)"
		}
	    }
	} err] {
	    if [Exwin_Toplevel .ftpmsg "FTP error"] {
		Widget_Message .ftpmsg msg -aspect 1500 -relief raised
	    }
	    .ftpmsg.msg configure -text \
"Messages generated during FTP transfer:

$err
"
	} else {
	    Exmh_Status "FTP transfer complete"
	}
    }
}
proc MimeFTPInner {site directory theirname myname} {
    global env mime
    if {$mime(ftpMethod) == "ftp -n"} {
	set pipe [open "|$mime(ftpCommand) -n $site " w]
	puts $pipe "user anonymous $env(USER)@"
    } else {
	set pipe [open "|$mime(ftpCommand) $site" w]
	puts $pipe anonymous
	puts $pipe $env(USER)@
    }
    puts $pipe "cd $directory"
    puts $pipe "get $theirname $myname"
    puts $pipe "quit"
    close $pipe
}

proc Mime_ShowMultipart { tkw depth part justDoIt menu color} {
    global mimeHdr

    if ![info exists mimeHdr($depth,$part,param,boundary)] {
	$tkw insert insert "No <boundary> parameter for multipart message\n"
	$tkw insert insert "Raw content follows...\n\n"
	return [Mime_ShowText $tkw $depth $part $justDoIt $menu $color]
    }
    set fileName $mimeHdr($depth,$part,file)
    set boundary $mimeHdr($depth,$part,param,boundary)
    set type $mimeHdr($depth,$part,type)

    if [catch {open $fileName r} fileIO] {
	$tkw insert insert "Mime_ShowMultipart $fileName: $fileIO\n"
	return 0
    }
    Exmh_Status "Mime_ShowMultipart $depth $part $type"
    set mimeHdr($depth,$part,numParts) \
	[MimeMultiChop $tkw $depth $part $fileIO $boundary \
	    [expr {($type == "multipart/digest") ? \
		"message/rfc822" : "text/plain"}]]
    close $fileIO

    # Display it

    MimeShowPart $tkw $depth $part $justDoIt $color 0

    return 1
}
proc MimeMultiChop {tkw depth part fileIO boundary {defType text/plain} } {
    global mimeHdr mime

    set depth $depth-$part
    set part 0
    if {$boundary == {}} {	;# Hack for message/RFC822
	set state rfc822
    } else {
	set state prolog	;# prolog > header > body [ > header > body ]
    }

    # Read and parse multipart, diverting subparts to files

    while {1} {
	set numBytes [gets $fileIO line]
	if {$numBytes < 0} {
	    catch {close $tmpFile}
	    return $part
	}
	if {($numBytes == 0) && ($state == "header")} {
	    set state body
	    if [catch {set mimeHdr($depth,$part,hdr,content-type)} contentType] {
		set contentType $defType
	    }
	    if [catch {set mimeHdr($depth,$part,hdr,content-transfer-encoding)} encoding] {
		set encoding 7bit
	    }
	    set encoding [string trim [string tolower $encoding] \ \" ]
	    set type [MimeHeader $depth $part $contentType $encoding]
	    Exmh_Status "Saving $type ..."
	    if {[file dirname $type] == "multipart"} {
		if ![catch {set mimeHdr($depth,$part,param,boundary)} bndy] {
		    set mimeHdr($depth,$part,numParts) \
			[MimeMultiChop $tkw $depth $part $fileIO $bndy]
		}
	    }
	    continue
	}
	if [regexp ^-- $line] {
	    #Could be a boundary line
	    set trailer [string range $line 2 end]
	    if {($boundary != {}) && [string compare $boundary $trailer] == 0} {
		if {$state != "prolog"} {
		    close $tmpFile
		}
		incr part
		set state header
		set mimeHdr($depth,$part,file) \
		    [Mime_TempFile /tmp/exmh.$depth.$part]
		set tmpFile [open $mimeHdr($depth,$part,file) w]
		Exmh_Status "MimeMultiChop $depth $part $mimeHdr($depth,$part,file)"
		continue
	    } else {
		if {[string compare $boundary-- $trailer] == 0} {
		    if {$state != "prolog"} {
			close $tmpFile
		    }
		    return $part
		}
	    }
	}
	if {($state == "rfc822") && ($numBytes > 0)} {
	    incr part
	    set mimeHdr($depth,$part,file) \
		    [Mime_TempFile /tmp/exmh.$depth.$part]
	    set tmpFile [open $mimeHdr($depth,$part,file) w]
	    Exmh_Debug "MimeMultiChop $depth $part $mimeHdr($depth,$part,file)"
	    #
	    # Hack to deal with header-less content
	    #
	    if [regexp {^([^: ]+):} $line] {
		set state header
	    } else {
		Exmh_Status "Warning - no nested RFC 822 headers" warn
		set mimeHdr($depth,$part,type) text/plain
		set mimeHdr($depth,$part,encoding) 8bit
		set state body
	    }
	}
	case $state {
	    header {
		set continue [regexp {^[ 	]} $line]
		if {! $continue} {
		    if [regexp -indices {^([^:]+):} $line match hdr] {
			set cur [string tolower \
			    [eval {string range $line} $hdr]]
			set mimeHdr($depth,$part,hdr,$cur) \
			    [string trim \
				[string range $line \
				    [expr [lindex $match 1]+1] end]]
			lappend mimeHdr($depth,$part,hdrs) $cur
		    }
		} else {
		    if [regexp -indices {^[ 	]+} $line match] {
			append mimeHdr($depth,$part,hdr,$cur) \
			    [string range $line \
				    [expr [lindex $match 1]+1] end]
		    }
		}
	    }
	    body {
		# Divert sub-body to a temp file.
		puts $tmpFile $line
	    }
	    prolog {
		if {$mime(showPrelude)} {
		    $tkw insert insert $line\n
		}
	    }
	}
    }
}
proc MimeShowPart { tkw depth part justDoIt color only {defType text/plain} } {
    global mimeHdr mime
    if [catch {set mimeHdr($depth,$part,type)} type] {
	set type $defType
    }

    set newDepth $depth-$part

    set len [expr ([string length $color]-1)/3]
    scan $color "#%${len}x%${len}x%${len}x" red green blue
    set color [format "#%${len}x%${len}x%${len}x" \
	[expr int($red * .95)] [expr int($green *.95)] \
	[expr int($blue * .95)]]
    set start [$tkw index insert]
    $tkw tag add $newDepth $start
    $tkw tag raise $newDepth

    set titlestart [$tkw index insert]

    # Slight tweak because of TCL 7.0 regsub bug
    set depthPart [string range "$depth-$part." 4 end]
    regsub -all -- - $depthPart . depthPart

    if {(!$only) || \
	([info exists mimeHdr($depth,$part,hdr,content-description)])}  {
	$tkw insert insert "$depthPart\t"
    }

    if [info exists mimeHdr($depth,$part,hdr,content-description)] {
	set descstart [$tkw index insert]
	$tkw insert insert "$mimeHdr($depth,$part,hdr,content-description)"
	set descend [$tkw index insert]
	$tkw insert insert " "
	$tkw tag add $newDepth-desc $descstart $descend
	$tkw tag configure $newDepth-desc -underline 1
        set menu [MimeMakeMenu $tkw $newDepth $depth $part \
	    "$depthPart $mimeHdr($depth,$part,hdr,content-description)" $type]
    } else {
#	$tkw insert insert "Untitled"
        set menu [MimeMakeMenu $tkw $newDepth $depth $part \
	    "$depthPart" $type]
    }
    set titleend [$tkw index insert]
    set typestart [$tkw index insert]
    if {$mime(showType)} {
	$tkw insert insert "($type)"
    }
    set typeend [$tkw index insert]
    $tkw insert insert \n

    $tkw tag add $newDepth-title $titlestart $titleend
    $tkw tag configure $newDepth-title -font *-Times-Bold-R-Normal-*-120-*
    $tkw tag raise $newDepth-title

    $tkw tag add $newDepth-type $typestart $typeend
    $tkw tag configure $newDepth-type -font *-Times-Medium-R-Normal-*-120-*
    $tkw tag raise $newDepth-type

    if ![regexp multipart $type] {
	MimeShow $tkw $depth $part $justDoIt $menu $color
    } else {
        set numParts $mimeHdr($depth,$part,numParts)
	
	set subtype [file tail $type]
        case $subtype {
            default {
		if {$subtype != "mixed"} {
		    $tkw insert insert "(Unknown multipart subtype of '$subtype'; displaying as mixed.)\n";
		}
                for {set p 1} {$p <= $numParts} {incr p} {
                    MimeShowPart $tkw $newDepth $p $justDoIt $color 0
                }
            }
            parallel {
                for {set p 1} {$p <= $numParts} {incr p} {
                    MimeShowPart $tkw $newDepth $p 1 $color 0
                }
            }
            digest {
                Exmh_Debug DIGEST with $numParts parts
                for {set p 1} {$p <= $numParts} {incr p} {
                    Exmh_Debug digest $newDepth $p 
                    MimeShowPart $tkw $newDepth $p $justDoIt $color 0 message/rfc822
                }
            }
            alternative {
                global mime
                set chosenPart {}
                for {set p 1} {$p <= $numParts} {incr p} {
                    set type $mimeHdr($newDepth,$p,type)
                    foreach t [list $type [file dirname $type]] {
                        if [info exists mime(showproc,$t)] {
                            set chosenPart $p
                            break
                        }
                    }
                }
                if {$chosenPart == {}} {
                    # Can't display anything.  Unroll the whole thing
                    # so the user can choose what to send to metamail.
                    # This is mainly to work around the fact that the
                    # sub-parts are not collected into a single file for me.
                    $tkw insert insert "You have to pick an alternative...\n"
    
                    for {set p 1} {$p <= $numParts} {incr p} {
                        MimeShowPart $tkw $newDepth $p $justDoIt $color 0
                    }
                } else {
                    MimeShowPart $tkw $newDepth $chosenPart $justDoIt $color 1
                }
            }
        }
    }

    set end [$tkw index insert]
    $tkw insert insert \n

    $menu add command -label "Dismiss" -command "$menu unpost"
    $tkw tag add $newDepth $start $end
    $tkw tag configure $newDepth -background $color
}
proc MimeMakeMenu {tkw tag depth part menuLabel {type "message/rfc822"}} {
    global mimeHdr

    set menu .$tag-menu
    if [catch {menu $menu}] {
	$menu delete 0 999
    }

    if [catch {set descr $mimeHdr($depth,$part,hdr,content-description)}] {
	set descr $type
    }
    $menu configure -disabledforeground Black
    $menu add command \
	-label $menuLabel \
	-state disabled \
	-font *-Times-Bold-R-Normal-*-140-*
    $menu add separator
    $menu add command \
	-label "Save $descr..." \
	-command [list Mime_SavePiece $mimeHdr($depth,$part,file) \
				      $mimeHdr($depth,$part,encoding) \
			              $depth $part $type]
    $menu add command \
	-label "Pass $type to metamail..." \
	-command [list MimeMetaMail $type \
			   $mimeHdr($depth,$part,encoding) \
			   $mimeHdr($depth,$part,file)]
    $tkw tag bind $tag <Button-3> "$menu post %X %Y"
    return $menu
}
proc Mime_SavePiece { fileName encoding depth part type} {
    global mimeHdr
    if [catch {set default $mimeHdr($depth,$part,param,name)}] {
	set default ""
    }
    Exmh_Status "Saving $fileName"
    set name [FSBox "Save $type to:" $default]
    if {$name != {}} {
	if [MimeDecode $fileName $name $encoding] {
            Exmh_Status "Save ok"
	}
    } else {
	Exmh_Status "Not saved"
    }
}
proc Mime_TempFile { basename } {
    global mime
    set uid 0
    while {[file exists $basename.$uid]} {
	incr uid
    }
    lappend mime(junkfiles) $basename.$uid
    return $basename.$uid
}

proc Mime_Debug { args } {
    puts stderr $args
}
