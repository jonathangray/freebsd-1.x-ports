# 
# faces.tcl
#
# facesaver support (bitmap display of who sent a message).
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

#### Faces support

proc Face_Show { fromwho {xface {}} } {
    global faces faceCache env

    # Compute faces search path
    if ![info exists faces(path)] {
	if [info exists env(FACEPATH)] {
	    foreach dir [split $env(FACEPATH) :] {
		lappend faces(path) $dir
	    }
	} else {
	    foreach dir $faces(set) {
		lappend faces(path) $faces(dir)/$dir
	    }
	}
	if [info exists faces(path)] {
	    Exmh_Debug FACEPATH $faces(path)
	}
    }
    if ![info exists faces(path)] {
	set faces(enabled) 0
    }
    # Clear previous face, if any.
    Face_Delete

    # Honor X-Face even if faces is disabled
    if {[string compare $xface ""] && \
	[string compare $faces(xfaceProg) ""]} {
	FaceXFace $xface
	return
    }
    if {! $faces(enabled)} {
	return
    }
    # Check for cached lookup result
    if [info exists faceCache($fromwho)] {
	if [file exists $faceCache($fromwho)] {
	    Face_ShowFile $faceCache($fromwho)
	    return
	} else {
	    unset $faceCache($fromwho)
	}
	return
    }
    set msg [Exmh_OldStatus]
    Exmh_Status "Looking up face of $fromwho ..."
    set parts [split $fromwho @]
    set people [lindex $parts 0]
    set machine [lindex $parts 1]
    if {$machine == {}} {
	set machine $faces(defaultDomain)
    }

    # Loop through Face path
    set lastp {}
    set lastf {}
    set pathlist {}
    set matches {}
    foreach dir $faces(path) {

	# Check for machine and people aliases
	set newm [FaceMachine $dir $machine]
	set newp [FacePeople $dir $newm $people]
	if {$newp != $lastp} {
#	    Exmh_Debug Faces: dir=$dir newm=$newm newp=$newp
	    set lastp $newp
	    set from [lindex $newp 1]
	    foreach item [split [lindex $newp 0] .] {
		if {$item != {}} {
		    lappend from $item
		}
	    }
	    set lastf $from

	    set user [string tolower [lindex $from 0]]
	    set path {}
	    set pathb {}
	    set prefix {}
	    set prefixb {}
	    set pathlist MISC
	    for {set i [expr [llength $from]-1]} {$i>0} {incr i -1} {
		set component [string tolower [lindex $from $i]]
		append path $prefix $component
		set prefix /
		set pathb $component$prefixb$pathb
		set prefixb .
		lappend pathlist $pathb $path
	    }
#	    Exmh_Debug $pathlist
	}
	# Finally, look for matching files
	foreach tail [list $user unknown] {
	    for {set i [expr [llength $pathlist]-1]} {$i>=0} {incr i -1} {
		set path2 [lindex $pathlist $i]
		set filename $dir/$path2/$tail/face.xbm
		if [file exists $filename] {
#		    Exmh_Debug $filename
		    lappend matches $filename
		}
	    }
	}
    }
    # Use first non-unknown face
    foreach filename $matches {
	if ![string match *unknown* $filename] {
	    Face_ShowFile $filename
	    set faceCache($fromwho) $filename
	    Exmh_Status $msg
	    return 1
	}
    }
    if {[llength $matches] > 0} {
	set filename [lindex $matches 0]
	Face_ShowFile $filename
	set faceCache($fromwho) $filename
	Exmh_Status $msg
	return 1
    }
    Exmh_Status "(no face)"
    Exmh_Debug FaceMiss user=$user fromwho=$fromwho path=$pathlist
}
proc Face_ShowFile { facefile {startup 0}} {
    global exwin
    Face_Delete
    if [catch {$exwin(faceCanvas) create bitmap 0 0 \
				-anchor nw -bitmap @$facefile} id] {
	Exmh_Debug $id
	return
    } else {
	set exwin(faceBitmap) $id
    }
}
proc Face_Delete {} {
    global exwin
    if [info exists exwin(faceBitmap)] {
	$exwin(faceCanvas) delete $exwin(faceBitmap)
	unset exwin(faceBitmap)
    }
}
proc FaceXFace { xface } {
    global faces
    Exmh_Status "$faces(xfaceProg)" red
    set fid [open "| $faces(xfaceProg) > /tmp/FACE" w]
    puts $fid $xface
    if [catch {close $fid} err] {
	Exmh_Status $err error
    } else {
	Face_ShowFile /tmp/FACE
	exec rm /tmp/FACE
	Exmh_Status ok
    }
    return
}
#    Faces information is administered by a pair of  ASCII  files
#    in  the  faces directory that associate related machines and
#    faces. The machine table machine.tab  attaches  machines  to
#    communities; the line
#          stard=sunaus
#    puts the machine stard  in  community  sunaus.  The  machine
#    table may be used to alias entire communities; the line
#          wseng.sun.com=eng.sun.com
#    will cause the wseng.sun.com domain  to  be  mapped  to  the
#    eng.sun.com   community.   The  people  table  associates  a
#    community/alias pair, with a real username.
#          sunaus/rburridge=richb
#    causes the alias rburridge to be translated  into  the  real
#    username richb for the community sunaus

proc FaceMachine {dir machine} {
    return [FaceMap $dir/machine.tab $machine]
}
proc FacePeople {dir machine people} {
    set map [FaceMap $dir/people.tab $machine/$people]
    if {[llength $map] == 1} {
	return [list $machine $map]
    } else {
	return $map
    }
}
proc FaceMap {file item} {
    global faceMap
    set item [string tolower $item]
    if [info exists faceMap($file,$item)] {
	return $faceMap($file,$item)
    }
    if [catch {open $file} in] {
	set faceMap($file,$item) [split $item /]
	return $faceMap($file,$item)
    }
    while {1} {
	set numBytes [gets $in input]
	if {$numBytes < 0} {
	    break
	}
	set parts [split $input =]
	set lhs [string trim [lindex $parts 0]]
	if {$lhs == $item} {
	    set rhs [split [string trim [lindex $parts 1]] /]
	    set faceMap($file,$item) $rhs
	    close $in
	    return $rhs
	}
    }
    close $in
    set faceMap($file,$item) [split $item /]
    return $faceMap($file,$item)
}
