# seditMime.tcl
#
# Support for composing MIME-compliant messages in sedit
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc SeditMimeReset { t } {
    global sedit
    set sedit($t,enriched) 0
    catch {unset sedit($t,mimeHdrs)}
}
proc SeditMimeEnriched {draft t look} {
    global mimeFont mime sedit
    if [catch {$t tag names sel.first} tags] {
	SeditMsg $t "No selection?"
	return
    }
    if [$t compare [$t index header] > sel.first] {
	SeditMsg $t "No looks in headers"
	return
    }
    if ![info exists sedit($t,mimeHdrs)] {
	set sedit($t,mimeHdrs) Content-Type
	set sedit($t,mime,Content-Type) text/enriched
	set sedit($t,enriched) 1
    }
    if {[string compare $look "x-plain"] == 0} {
	Rich_TagClear $t sel.first sel.last
    } else {
	Rich_TagRange $t sel.first sel.last $look enriched
    }
}
proc SeditMimeHeaders { t out } {
    global sedit
    if ![info exists sedit($t,mimeHdrs)] {
	return
    }
    puts $out "Mime-Version: 1.0"
    foreach hdr $sedit($t,mimeHdrs) {
	puts $out "$hdr: $sedit($t,mime,$hdr)"
    }
}

proc SeditEnrichedExpand { t } {
    set tags {}	;# enriched text tags
    foreach tag [$t tag names] {
	if [regexp {Mime=(.+)} $tag match tagName] {
	    lappend tags $tagName
	}
	if [regexp {Look[0-9]+} $tag] {
	    $t tag delete $tag
	}
    }
    Exmh_Debug SeditEnrichedExpand tags $tags
    foreach tag $tags {
	if {$tag == "x-plain"} {
	    continue
	}
	set range [$t tag nextrange Mime=$tag 1.0]
	while {$range != {}} {
	    set first [lindex $range 0]
	    set last [lindex $range 1]
	    $t mark set first $first	;# need a floating mark
	    $t tag remove Mime=$tag $first $last
	    $t insert $last </$tag>
	    $t insert $first <$tag>
	    foreach tag2 [$t tag names first] {
		if [regexp {Mime=(.+)} $tag2 match tagName] {
		    # bleed the other mark backwards, over <tag> just inserted
		    set len [string length <$tag>]
		    $t tag add $match "first - $len chars" first
		}
	    }
	    set range [$t tag nextrange Mime=$tag 1.0]
	}
    }
}
