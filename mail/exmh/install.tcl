#
#
# install.tcl - Installation support
#
proc install_init { appName dotFile } {
    global install
    set install(appName) $appName
    install_progVar wish /usr/local/bin/wish {wish absolute pathname}
    set install(dotFile) $dotFile
    if [file readable $dotFile] {
	if [catch {uplevel #0 source $dotFile} msg] {
	    puts stderr "source $dotFile: $msg"
	}
    } else {
	if {[catch {glob ../$appName*/$dotFile} files] == 0} {
	    installAlternates $files 1
	}
    }
}
proc installAlternates { files isdefault } {
    wm withdraw .
    toplevel .config
    set fm [frame .config.rim -bd 10]
    message $fm.msg -aspect 1500 -text \
"Please select an alternate configuration."
    global tk_version
    if {$tk_version >= 3.3} {
	pack $fm -fill both -expand 1
	pack $fm.msg
    } else {
	pack append .config $fm {top fill expand}
	pack append $fm $fm.msg {top fill expand}
    }
    set id 0
    foreach f $files {
	button $fm.but$id -text $f -command [list installConfig $f $isdefault]
	if {$tk_version >= 3.3} {
	    pack $fm.but$id -fill both -expand 1
	} else {
	    pack append $fm $fm.but$id {top fill expand}
	}
	incr id
    }
    button $fm.but$id -text "Ignore configurations" -command installConfig
    if {$tk_version >= 3.3} {
	pack $fm.but$id -fill both -expand 1
    } else {
	pack append $fm $fm.but$id {top fill expand}
    }
    tkwait window .config
    wm deiconify .
}
proc installConfigs {} {
    global install
    if {[catch {glob ../$install(appName)*/$install(dotFile)*} files] == 0} {
	installAlternates $files 0
    }
}
proc installConfig { {file {}} {isdefault 1} } {
    global install
    installFeedback "installConfig $file"
    if {$file != {}} {
	if [catch {uplevel #0 source $file} msg] {
	    installFeedback $msg
	    return
	}
	if {! $isdefault} {
	    set install(dotFile) $file
	    installUpdateAll
	}
    }
    destroy .config
}
proc install_var { var value {comment {}} } {
    global install
    lappend install(sequence) $var
    set install(field,$var) [list $var $value $comment]
}
proc install_version { var version {comment {}} } {
    global install
    if {$comment == {}} {
	if [info exists install(appName)] {
	    set comment "$appName version stamp"
	} else {
	    set comment {Version stamp}
	}
    }
    set install(versionVar) $var
    install_var $var $version $comment
}
proc install_dir { name value comment } {
    global install
    lappend install(dirlist) $name
    install_var install(dir,$name) $value $comment
}
proc install_glob { name args } {
    global install
    set install(glob,$name) $args
}
proc install_dirVar { var pathname comment } {
    install_var $var $pathname $comment
    global install
    lappend install(dircheck) $var
    set install(dircheck,$var) $comment
}
proc install_fileVar { var pathname comment } {
    install_var $var $pathname $comment
    global install
    lappend install(filecheck) $var
    set install(filecheck,$var) $comment
}
proc install_progVar { var pathname comment } {
    install_var $var $pathname $comment
    global install
    lappend install(progcheck) $var
    set install(progcheck,$var) $comment
}
proc install_sed { suffix args } {
    global install
    set install(sedSuffix) $suffix
    set install(sedProgs) $args
}
proc install_libDir {var pathname comment} {
    global install
    install_dirVar $var $pathname $comment
    set install(libDirVar) $var
}
proc install_testLib { pathname } {
    global install
    set install(testLib) $pathname
}
proc install_expect { var pathname comment } {
    install_var $var $pathname $comment
    global install
    set install(expectVar) $var
    lappend install(progcheck) $var
    set install(progcheck,$var) $comment
}

proc installFieldVar { item } { lindex $item 0 }
proc installFieldComment { item } { lindex $item 2 }
proc installFieldDefault { item {override 0} } {
    set default [lindex $item 1]
    if {$override} {
	return $default
    }
    set varName [installFieldVar $item]
    if [catch {installGetValue $varName} value] {
	return $default
    } else {
	return $value
    }
}

proc install_help { text } {
    global install
    set install(helpText) $text
}

proc installFeedback { text } {
    global install
    catch {
	$install(msg) configure -text $text
	update
    }
}
proc installError { text } {
    puts stderr $text
    installFeedback $text
}
proc installFieldInit {} {
    global install
    set install(lastentry) {}
}
proc installDoField { item {override 0} } {
    global install
    if ![info exists install(wuid)] { set install(wuid) 0 }
    incr install(wuid)
    set f [frame .rim.import$install(wuid) -relief raised]
    set var [installFieldVar $item]
    button $f.label -text [format "%-30s:" [installFieldComment $item]] \
	-command "installShowValue $var" -font fixed
    entry $f.entry -bg white -font fixed -width 35
    $f.entry insert 0 [installFieldDefault $item $override]
    bind $f.entry <Return> [list installSetValue $var]
    if {$install(lastentry) != {}} {
	bind $install(lastentry) <Tab> [list focus $f.entry]
    } else {
	set install(firstentry) $f.entry
    }
    set install(lastentry) $f.entry
    set install(entry,$var) $f.entry
    lappend install(allEntries) $f.entry

    pack append .rim $f {top expand fill}
    pack append $f $f.label {left padx 3}
    pack append $f $f.entry {right expand fill}
}
proc installUpdateField { item {override 0} } {
    global install
    set var [installFieldVar $item]
    set entry $install(entry,$var)
    $entry delete 0 end
    $entry insert 0 [installFieldDefault $item $override]
}
proc installFieldDone {} {
    global install
    if {[info exists install(firstentry)] && \
	[info exists install(lastentry)]} {
	bind $install(lastentry) <Tab> [list focus $install(firstentry)]
    }
}

proc installSetValue { _var } {
    global install
    if [info exists install(entry,$_var)] {
	set _value [$install(entry,$_var) get]
	if [string match *(* $_var] {
	    set _arrayName [lindex [split $_var (] 0]
	    global $_arrayName
	} else {
	    global $_var
	}
	set $_var $_value
#	installFeedback "$_var $_value"
    }
}
proc installGetValue { var } {
    if [string match *(* $var] {
	set arrayName [lindex [split $var (] 0]
	global $arrayName
    } else {
	global $var
    }
    return [set $var]
}
proc installShowValue { var } {
    global install
    if [info exists install(entry,$var)] {
	installSetValue $var
	set entry $install(entry,$var)
	$entry select from 0
	$entry select to end
	focus $entry
    }
}
proc installVerify {} {
    global install
    installFeedback "Checking Pathnames..."
    set errors {}
    if [info exists install(dircheck)] {
	foreach var $install(dircheck) {
	    installSetValue $var
	    set path [installGetValue $var]
	    if ![file isdirectory $path] {
		set willMakeDir 0
		foreach dirType $install(dirlist) {
		    set newdir $install(dir,$dirType)
		    if {$newdir == $path} {
			set willMakeDir 1
		    }
		}
		if {! $willMakeDir} {
		    lappend errors [format "%-30s <%s> %s" \
			$install(dircheck,$var) $path "not a directory"]
		}
	    }
	    if ![regexp ^/ $path] {
		lappend errors [format "%-30s Warning: <%s> %s" \
		    $install(dircheck,$var) $path "is not an absolute pathname"]
	    }
	}
    }
    if [info exists install(filecheck)] {
	foreach var $install(filecheck) {
	    installSetValue $var
	    set path [installGetValue $var]
	    if ![file exists $path] {
		lappend errors [format "%-30s <%s> %s" \
		    $install(filecheck,$var) $path "does not exist"]
	    }
	    if ![regexp ^/ $path] {
		lappend errors [format "%-30s Warning: <%s> %s" \
		    $install(filecheck,$var) $path "is not an absolute pathname"]
	    }
	}
    }
    if [info exists install(progcheck)] {
	foreach var $install(progcheck) {
	    installSetValue $var	;# Snarf current value from entry
	    set path [installGetValue $var]
	    if ![file executable $path] {
		lappend errors [format "%-30s <%s> %s" \
		    $install(progcheck,$var) $path "is not executable"]
	    }
	    if ![regexp ^/ $path] {
		lappend errors [format "%-30s Warning: <%s> %s" \
		    $install(progcheck,$var) $path "is not an absolute pathname"]
	    }
	}
    }
    if {$errors != {}} {
	installFeedback "Verify errors"
    } else {
	installFeedback "Verify OK"
	return
    }
    toplevel .verify
    frame .verify.top
    button .verify.top.quit -text "Dismiss" -command {destroy .verify}
    label .verify.top.label -text "  Verify Errors "
    pack append .verify .verify.top {top fill expand}
    pack append .verify.top .verify.top.quit left .verify.top.label {left fill}

    set numLines [llength $errors]
    if {$numLines < 30} {
	text .verify.t -width 80 -height $numLines -font fixed
	pack append .verify .verify.t {bottom expand fill}
    } else {
	text .verify.t -width 80 -height 30 -yscrollcommand {.verify.s set} -font fixed
	scrollbar .verify.s -orient vert -command {.verify.t yview}
	pack append .verify \
	    .verify.s {right filly} \
	    .verify.t {left expand fill}
    }
    foreach line $errors {
	.verify.t insert end $line\n
    }

}

proc installSed { } {
    global install
    set id 0
    while {[catch {open /tmp/sed.$id w} script]} {
	incr id
	if {$id > 100} {
	    installFeedback "installSed: Cannot create sed script in /tmp"
	    return
	}
    }
    installSetValue wish	;# Get current value from entry widget
    puts $script "s,#!wish,#![installGetValue wish],"

    # Set up for helper expect scripts, if needed and if possible
    if [info exists install(expectVar)] {
	installSetValue $install(expectVar)
	set pathname [installGetValue $install(expectVar)]
	if {$pathname != {}} {
	    puts $script "s,#!expect,#!$pathname,"
	}
    }

    # Insert configuration information
    puts $script /^#CONFIGURATION/a\\
    foreach v $install(sequence) {
	set item $install(field,$v)
	set var [installFieldVar $item]
	installSetValue $var
	global $var
	puts $script [list set $var [installGetValue $var]] nonewline
	puts $script \\
    }
    puts $script ""
    close $script
    foreach prog $install(sedProgs) {
	if [catch {
	    exec sed -f /tmp/sed.$id < ${prog}$install(sedSuffix) > $prog
	    exec chmod +x $prog
	} msg] {
	    installFeedback "sed error on $prog: $msg"
	}
    }
#    exec rm /tmp/sed.$id
}
proc installPatch {} {
    global install
    installSave
    installVerify
    installSed
    set sample [lindex $install(sedProgs) 0]
    catch {exec diff -c ${sample}$install(sedSuffix) $sample} diff
    catch {destroy .test}
    set numLines [llength [split $diff \n]]
    if {$numLines == 0} {
	installFeedback "No diffs after patching"
	return
    }
    toplevel .test
    frame .test.top
    button .test.top.quit -text "Dismiss" -command {destroy .test}
    label .test.top.label -text "  Context diff of $sample"
    pack append .test .test.top {top fill expand}
    pack append .test.top .test.top.quit left .test.top.label {left fill}

    installFeedback "$numLines lines of diff output"
    if {$numLines < 30} {
	text .test.t -width 80 -height $numLines -font fixed
	pack append .test .test.t {bottom expand fill}
    } else {
	text .test.t -width 80 -height 30 -yscrollcommand {.test.s set} -font fixed
	scrollbar .test.s -orient vert -command {.test.t yview}
	pack append .test \
	    .test.s {right filly} \
	    .test.t {left expand fill}
    }
    .test.t insert end $diff
}
proc install_test { args } {
    global install
    set install(test) $args
}
proc installTest {} {
    global install
    # Run patch again with testing library, if it is defined
    if {[info exists install(testLib)] && [info exists install(libDirVar)]} {
	set var $install(libDirVar)
	if [info exists install(entry,$var)] {
	    set realValue [$install(entry,$var) get]
	    $install(entry,$var) delete 0 end
	    $install(entry,$var) insert 0 $install(testLib)
	    installSed
	}
    }
    if [info exists install(test)] {
	installFeedback $install(test)
	eval $install(test)
    } else {
	installFeedback "No install_test command"
    }
    if [info exists realValue] {
	$install(entry,$var) delete 0 end
	$install(entry,$var) insert 0 $realValue
    }
}
proc installTclIndex {} {
    installFeedback "Refreshing ./lib/tclIndex"
    auto_mkindex ./lib *.tcl
    installFeedback ""
}

proc installButton {} {
    button .rim.buttons.yes -text "Really Install" -command {installInner}
    button .rim.buttons.no -text "Cancel" -command {installCancel}
    pack unpack .rim.buttons.install
    pack before .rim.buttons.quit .rim.buttons.no left .rim.buttons.yes left
}
proc installSave { } {
    global install argv0
    # Save it
    installSetValue install(dotFile)
    if [catch {open $install(dotFile) w} out] {
	installFeedback "Cannot write $install(dotFile)"
	return
    }
    if ![info exists argv0] {
	set argv0 $install(appName).install
    }
    puts $out "# Saved state from $argv0"
    puts $out "# [exec date]"
    foreach v $install(sequence) {
	set item $install(field,$v)
	set varName [installFieldVar $item]
	installSetValue $varName
	set value [installGetValue $varName]
	puts $out [list set $varName $value]
    }
    close $out
    installFeedback "Saved settings in $install(dotFile)"
}
proc installCancel {} {
    after 10 {
	destroy .rim.buttons.yes ; destroy .rim.buttons.no
	pack before .rim.buttons.quit .rim.buttons.install left
    }
}
proc installCmd { logProc unixCmd } {
    if {$logProc != "nolog"} {
	$logProc $unixCmd
    } else {
	eval exec $unixCmd
    }
}
proc installInner { {logProc nolog} } {
    global install
    installVerify
    installSed
    foreach dirType $install(dirlist) {
	#
	# Install directory - make sure it exists
	#
	set dir $install(dir,$dirType)
	if ![file isdirectory $dir] {
	    installCmd $logProc [list mkdir $dir]
	    installCmd $logProc [list chmod a+rx $dir]
	}
	if {($logProc == "nolog") && ![file isdirectory $dir]} {
	    installError "LibDir $dir is not a directory"
	    continue
	}
	if [info exists install(glob,$dirType)] {
	    #
	    # Install glob pattern - copy the files in
	    #
	    foreach f [eval glob $install(glob,$dirType)] {
		if [catch {
		    set t [file tail $f]
		    if {$dirType == "man"} {
			# Hack to tweak file suffix
			set end [expr [string length $dir]-1]
			set suffix [string index $dir $end]
			set newf [file root $t].$suffix
		    } else {
			set newf $t
		    }
		    installCmd $logProc [list rm -f $dir/$newf]
		    installCmd $logProc [list cp $f $dir/$newf]
		    installCmd $logProc [list chmod a+r $dir/$newf]
		} msg] {
		    installFeedback "Dir install error: $msg"
		    return
		} else {
		    if {$logProc == "nolog"} {
			installFeedback "Installed $newf"
		    }
		}
	    }
	}
    }
    if {$logProc == "nolog"} {
	installCancel
	installFeedback "Install complete"
    }
}
proc installFake {} {
    global exmh install
    toplevel .fake
    frame .fake.top
    button .fake.top.quit -text "Dismiss" -command {destroy .fake}
    label .fake.top.label -text "  Pending install actions"
    pack append .fake .fake.top {top fill expand}
    pack append .fake.top .fake.top.quit left .fake.top.label {left fill}

    text .fake.t -width 80 -height 20 -yscrollcommand {.fake.s set} -font fixed
    scrollbar .fake.s -orient vert -command {.fake.t yview}
    pack append .fake \
	.fake.s {right filly} \
	.fake.t {left expand fill}

    proc log { text } {
	.fake.t insert end $text\n
    }
    installInner log
}
proc install_dialog {} {
    global install
    toplevel .info
    wm title .info "Install info for $install(appName)"

    wm minsize . 100 100
    
    message .info.msg -aspect 1000 -font fixed -text $install(helpText)
    pack append .info .info.msg {top expand fill}
    update idletasks

    installBindInit
    
    frame .rim -bd 5 -relief flat
    pack append . .rim {top expand fill}

    installFieldInit
    foreach v $install(sequence) {
	if {$v == $install(versionVar)} {
	    set override 1	;# over-ride saved value with new version num.
	} else {
	    set override 0
	}
	installDoField $install(field,$v) $override
    }
    installFieldDone

    set install(msg) [label .rim.feedback -text ""]
    pack append .rim $install(msg) {top expand fill}
    
    frame .rim.buttons -relief raised
    pack append .rim .rim.buttons {top expand fill}
    
    button .rim.buttons.quit -text "Quit" -command {destroy .}
    button .rim.buttons.keys -text "Keys" -command {installBindKeys}
    button .rim.buttons.conf -text "Conf" -command {installConfigs}
    button .rim.buttons.patch -text "Patch" -command {installPatch}
    button .rim.buttons.test -text "Test" -command {installTest}
    button .rim.buttons.verify -text "Verify" -command {installFake}
    button .rim.buttons.install -text "Install" -command {installButton}
    button .rim.buttons.tclindex -text "TclIndex" -command {installTclIndex}
    frame .rim.buttons.space -width 10 -height 10

    pack append .rim.buttons \
	.rim.buttons.patch left \
	.rim.buttons.tclindex left \
	.rim.buttons.test left \
	.rim.buttons.verify left \
	.rim.buttons.space left \
	.rim.buttons.install left \
	.rim.buttons.quit right \
	.rim.buttons.keys right \
	.rim.buttons.conf right
}
proc installUpdateAll {} {
    global install
    foreach v $install(sequence) {
	if {$v == $install(versionVar)} {
	    set override 1	;# over-ride saved value with new version num.
	} else {
	    set override 0
	}
	installUpdateField $install(field,$v) $override
    }
}
proc installBindInit {} {
    global install

    set install(key,selpaste) <Control-y>
    set install(key,seldelete) <Control-w>
    set install(key,backspace) <Control-h>
    set install(key,backspace2) <Key-Delete>
    set install(key,backspace3) <Key-BackSpace>
    set install(key,deleol) <Control-k>
    set install(key,delword) <Escape>d	;# forwardly
    set install(key,delchar) <Control-d>

    set install(key,linestart) <Control-a>
    set install(key,lineend) <Control-e>
    set install(key,backword) <Escape>b
    set install(key,forwword) <Escape>f
    set install(key,backchar) <Control-b>
    set install(key,forwchar) <Control-f>

    installBindEntry

}
proc installBindKeys {} {
    global install tk_version
    toplevel .keys
    wm title .keys "Install Edit Preferences"
    frame .keys.b
    if {$tk_version >= 3.3} {
	pack .keys.b -side top -fill x
    } else {
	pack append .keys .keys.b {top fill}
    }
    button .keys.b.quit -text Dismiss -command {destroy .keys}
    button .keys.b.apply -text Apply -command installBindSet

    message .keys.msg -aspect 1500 -text \
"Key bindings for the installation tool.
Changes *do not* carry over to $install(appName)"
    set f [frame .keys.p -bd 10]
    if {$tk_version >= 3.3} {
	pack .keys.b .keys.msg .keys.p -side top -fill x
	pack .keys.b.quit .keys.b.apply -side right
    } else {
	pack append .keys .keys.b {top fill} .keys.msg {top fill} .keys.p {top fill}
	pack append .keys.b .keys.b.quit right .keys.b.apply right
    }

    set width 0
    foreach item [array names install] {
	if [string match key* $item] {
	    set name [lindex [split $item ,] 1]
	    set w [string length $name]
	    if {$w > $width} { set width $w }
	}
    }
    foreach item [lsort [array names install]] {
	if [string match key* $item] {
	    set name [lindex [split $item ,] 1]
	    set keystroke $install($item)
	    installKeyItem $f $width $name $keystroke
	}
    }
}
proc installKeyItem { frame width name keystroke } {
    global install tk_version
    frame $frame.$name
    label $frame.$name.label -text $name -width $width
    entry $frame.$name.entry -background white
    set install(keyval,$name) $frame.$name.entry
    $frame.$name.entry insert 0 $keystroke
    lappend install(allEntries) $frame.$name.entry
    if {$tk_version >= 3.3} {
	pack $frame.$name
	pack $frame.$name.label -side left
	pack $frame.$name.entry -side right -fill x -expand 1
    } else {
	pack append $frame $frame.$name {top expand fill}
	pack append $frame $frame.label left $frame.entry {right fillx expand}
    }
}
proc installBindSet {} {
    global install
    # Clear old bindings
    foreach item [array names install] {
	if [string match key,* $item] {
	    set name [lindex [split $item ,] 1]
	    bind Entry $install(key,$name) { }
	}
    }
    foreach item [array names install] {
	if [string match keyval,* $item] {
	    set name [lindex [split $item ,] 1]
	    set install(key,$name) [$install(keyval,$name) get]
	}
    }
    installBindEntry Entry
}
proc installBindEntry { {list {}} } {
    if {$list == {}} {
	installBindEntryInner Entry
    } else {
	foreach entry $list {
	    installBindEntryInner $entry
	}
    }
}
proc installBindEntryInner { what } {
    global install

    # Modification bindings

    bind $what <Escape> { }	;# no-op

    bind $what $install(key,selpaste) {
	catch {
	    %W insert insert [selection get]
	}
    }

    bind $what $install(key,seldelete) {
	catch {%W delete sel.first sel.last}
    }

    foreach bs {backspace backspace2 backspace3} {
	bind $what $install(key,$bs) {
	    tk_entryBackspace %W
	}
    }

    bind $what $install(key,deleol) {
	%W delete insert end
    }

    bind $what $install(key,delword) { info library }

    bind $what $install(key,delchar) {
	%W delete insert
    }

    bind $what $install(key,linestart) {
	%W icursor 0
    }

    bind $what $install(key,lineend) {
	%W icursor end
    }

    bind $what $install(key,backword) {
	set string [%W get]
	set curs [expr [%W index insert]-1]
	if {$curs < 0} return
	for {set x $curs} {$x > 0} {incr x -1} {
	    if {([string first [string index $string $x] " \t"] < 0)
		    && ([string first [string index $string [expr $x-1]] " \t"]
		    >= 0)} {
		break
	    }
	}
	%W icursor $x
    }

    bind $what $install(key,forwword) {
	set string [%W get]
	set curs [expr [%W index insert]+1]
	set len [string length $string]
	if {$curs < 0} return
	for {set x $curs} {$x < $len} {incr x} {
	    if {([string first [string index $string $x] " \t"] < 0)
		    && ([string first [string index $string [expr $x+1]] " \t"]
		    >= 0)} {
		break
	    }
	}
	%W icursor $x	
    }

    bind $what $install(key,backchar) {
	set x [%W index insert]
	if {$x > 0} {
	    incr x -1
	    %W icursor $x
	}
    }

    bind $what $install(key,forwchar) {
	set x [%W index insert]
	incr x
	%W icursor $x
    }

}
