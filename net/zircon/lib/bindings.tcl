proc doBindings {entry chid chan} {
    bind $entry <Meta-b> \
      { notIdle %W ; %W insert insert \002 ;tk_entrySeeCaret %W }
    bind $entry <Meta-v> \
      { notIdle %W ; %W insert insert \026 ; tk_entrySeeCaret %W }
    bind $entry <Meta-u> \
      { notIdle %W ; %W insert insert \037 ; tk_entrySeeCaret %W }
    bind $entry <Meta-s> {
	notIdle %W
	global smiley ; %W insert insert $smiley
	tk_entrySeeCaret %W
    }
    bind $entry <Shift-Meta-S> {
	notIdle %W
	global scowl ; %W insert insert $scowl
	tk_entrySeeCaret %W
    }
    bind $entry <Control-Meta-s> {
	notIdle %W
	global wink ; %W insert insert $wink
	tk_entrySeeCaret %W
    }
    bind $entry <Meta-j> {
	notIdle %W
	if {![catch {selection get} bf] && ![string match {} $bf]} {
	    channelJoin [cleanup $bf]
	}
    }
    bind $entry <Meta-m> {
	notIdle %W
	if {![catch {selection get} bf] && ![string match {} $bf]} {
	    Message :: make [string range [cleanup $bf] 0 8]
	}
    }
    bind $entry <Meta-f> {
	notIdle %W
	if {![catch {selection get} bf] && ![string match {} $bf]} {
	    finger [string range [cleanup $bf] 0 8]
	}
    }
    bind $entry <Meta-q> "oneLiner %W ${chid}"
    bind $entry <Meta-w> {
	notIdle %W
	sendIRC WHOIS [%W get]
	%W delete 0 end
    }
    bind $entry <Shift-Meta-W> {
	notIdle %W
	sendIRC WHO [%W get]
	%W delete 0 end
    }
    bind $entry <Any-KeyPress> {
	notIdle %W
	if ![string match {} %A"] { %W insert insert %A ; tk_entrySeeCaret %W }
    }
    bind $entry <Delete> \
      { notIdle %W ; tk_entryBackspace %W; tk_entrySeeCaret %W }
    bind $entry <BackSpace> \
      { notIdle %W ; tk_entryBackspace %W; tk_entrySeeCaret %W }
    bind $entry <Control-h> \
      { notIdle %W ; tk_entryBackspace %W; tk_entrySeeCaret %W }
    bind $entry <Control-d> { notIdle %W ; %W delete insert }
    bind $entry <Control-u> { notIdle %W ; %W delete 0 end }
    bind $entry <Control-w> \
      { notIdle %W ; tk_entryBackword %W; tk_entrySeeCaret %W }
    bind $entry <Escape> " notIdle %W ; $chid makePalette"
    bind $entry <Return> "
	$chid send \[$chid addToHist \[%W get\]\]
	%W delete 0 end
    "
    bind $entry <Shift-Return> "
	$chid action \[$chid addToHist \[%W get\]\]
	%W delete 0 end
    "
    bind $entry <Meta-Return> " doMisc2 $chid %W "
    bind $entry <Control-Return> "
	doNotice ${chan} \[$chid addToHist \[%W get\]\]
	%W delete 0 end
    "
    bind $entry <Control-p> "
	notIdle %W
	%W delete 0 end ; %W insert insert \[$chid getPrev\]
	tk_entrySeeCaret %W
    "
    bind $entry <Up> "
	notIdle %W
	%W delete 0 end ; %W insert insert \[$chid getPrev\]
	tk_entrySeeCaret %W
    "
    bind $entry <Control-n> "
	notIdle %W
	%W delete 0 end ; %W insert insert \[$chid getNext\]
	tk_entrySeeCaret %W
    "
    bind $entry <Down> "
	notIdle %W
	%W delete 0 end ; %W insert insert \[$chid getNext\]
	tk_entrySeeCaret %W
    "
    bind $entry <ButtonPress-2> "
	notIdle %W
	$chid insertSelect
    "
    global bindings
    foreach b $bindings {
	bind $entry [lindex $b 0] [lindex $b 1]
    }
    foreach b [$chid bindings] {
	bind $entry [lindex $b 0] [lindex $b 1]
    }
}
