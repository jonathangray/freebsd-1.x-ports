#
# Make an entry with some emacs-like edit keys......
#
proc emacsInsertSelect {ent} {
    if {[normal $ent] && ![catch {selection get} bf] && \
      ![string match ""  $bf]} {
	$ent insert insert $bf
	tk_entrySeeCaret $ent
    }
}

proc emacsTInsertSelect {ent} {
    if {[normal $ent] && ![catch {selection get} bf] && \
      ![string match "" $bf]} {
	$ent insert insert $bf	
	$ent yview insert
    }
}

proc emacsEntry {args} {
    set name [eval entry $args]
    bind $name <Control-a> { notIdle %W ; %W icursor 0 }
    bind $name <Control-b> {
	notIdle %W
	%W icursor [expr {[%W index insert] - 1}]
    }
    bind $name <Left> {
	notIdle %W
	%W icursor [expr {[%W index insert] - 1}]
    }
    bind $name <Control-d> { notIdle %W ; %W delete insert }
    bind $name <Control-e> { notIdle %W ; %W icursor end }
    bind $name <Control-f> {
	notIdle %W
	%W icursor [expr {[%W index insert] + 1}]
    }
    bind $name <Right> {
	notIdle %W
	%W icursor [expr {[%W index insert] + 1}]
    }
    bind $name <Control-k> { notIdle %W ; %W delete insert end }
    bind $name <Control-u> { notIdle %W ; %W delete 0 end }
    bind $name <ButtonPress-2> {notIdle %W ; emacsInsertSelect %W}
    bind $name <Delete> \
      {notIdle %W ; tk_entryBackspace %W; tk_entrySeeCaret %W}
    bind $name <BackSpace> \
      {notIdle %W ; tk_entryBackspace %W; tk_entrySeeCaret %W}
    bind $name <Control-h> \
      {notIdle %W ; tk_entryBackspace %W; tk_entrySeeCaret %W}
    bind $name <Meta-b> \
      { notIdle %W ; %W insert insert \002 ;tk_entrySeeCaret %W }
    bind $name <Meta-o> \
      { notIdle %W ; %W insert insert \017 ;tk_entrySeeCaret %W }
    bind $name <Meta-u> \
      { notIdle %W ; %W insert insert \037 ; tk_entrySeeCaret %W }
    bind $name <Meta-v> \
      { notIdle %W ; %W insert insert \026 ; tk_entrySeeCaret %W }
    return $name
}

proc emacsTEntry {args} {
    set name [eval text $args -height 1 -wrap none -setgrid 0]
    bind $name <Control-a> { notIdle %W ; %W mark set insert 1.0 }
    bind $name <Control-e> { notIdle %W ; %W mark set insert end }
    bind $name <Control-k> { notIdle %W ; %W delete insert end }
    bind $name <Control-d> { notIdle %W ; %W delete insert }
    bind $name <ButtonPress-2> {notIdle %W ; emacsTInsertSelect %W}
    bind $name <Control-u> { notIdle %W ; %W delete 1.0 end }
    bind $name <Control-v> { notIdle %W ; %W insert insert %A }
    bind $name <Meta-b> { notIdle %W ; %W insert insert \002 }
    bind $name <Meta-o> { notIdle %W ; %W insert insert \017 }
    bind $name <Meta-u> { notIdle %W ; %W insert insert \037 }
    bind $name <Meta-v> { notIdle %W ; %W insert insert \026 }
    return $name
}

proc entrySet {win val} { ${win} delete 0 end ; ${win} insert end $val }

proc labelEntry {t name opts init code} {
    frame $name
    eval label $name.label $opts
    [expr {$t ? "emacsTEntry" : "emacsEntry"}] $name.entry -relief raised
    $name.entry insert end $init
    pack $name.label -side left
    pack $name.entry -side left -expand 1 -fill x
    bind $name.entry <Return> "notIdle %W ; $code"
}
