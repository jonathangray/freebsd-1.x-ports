proc MakeMessageBox {msg} {
	catch {destroy .messageBox}
	toplevel .messageBox -relief raised
	wm title .messageBox "Message from Point"
	wm iconname .messageBox "Message from Point"
	message .messageBox.msg -aspect 700 -text $msg
	button .messageBox.okay -text "Okay" -command "destroy .messageBox"
	pack append .messageBox \
		.messageBox.msg {top fill expand} \
		.messageBox.okay {bottom}
}

proc MakeAboutBox {} {
	catch {destroy .aboutBox}
	toplevel .aboutBox -relief raised
	wm title .aboutBox "About Point"
	wm iconname .aboutBox "About Point"
	message .aboutBox.msg -text "\
Point -- An X Windows Text Editor \n\
             Version 1.62 \n\
             20 January 1992 \n\
\n\
                 by \n\
\n\
Charles Crowley \n\
Computer Science Department \n\
University of New Mexico \n\
Albuquerque, NM 87131 \n\
505-277-5446 \n\
crowley@unmvax.cs.unm.edu"
	bind .aboutBox.msg <Button> "destroy .aboutBox"
	bind .aboutBox.msg <Any-Key> "destroy .aboutBox"
	pack append .aboutBox .aboutBox.msg {top fill expand}
}

#
#
# Make various dialog boxes
#
#
proc HelpWindow {keyword} {
	global HelpDirectory
	set name $HelpDirectory/index.help
	if [file exists $name] {
 		set line [exec grep $keyword $name]
 		set name $HelpDirectory/[lindex $line 0]
 		if [file exists $name] {
			OpenWindow $name \
[lindex $line 1]x[lindex $line 2][lindex $line 3][lindex $line 4]
		} else {
			puts stderr "Cannot find help file $name"
		}
	} else {
		puts stderr "Cannot find help index file $name"
	}
}

proc GetSelectedKeyword {} {
	set sel [selection get]
	set kb [MakeKeywordBox]
	$kb.keyword insert 0 "$sel"
	FillKeywordBox $kb
}

proc MakeSearchOptionsBox {} {
	catch "destroy .sob"

	toplevel .sob
	wm title .sob "Search Options"
	wm iconname .sob "Search Options"

	label .sob.title -text "Change by typing a new value then Return" \
		-relief raised

	label .sob.l1 -text "Lines above a found string"
	entry .sob.lof -relief sunken -fg red -exportselection no
	.sob.lof insert 0 [Option get linesOverFind]
	bind .sob.lof <Return> "Option set linesOverFind \[.sob.lof get\]"

	label .sob.l2 -text \
		"File pattern to use in looking for keywords in files"
	entry .sob.kp -relief sunken -fg red -exportselection no
	.sob.kp insert 0 "[Option get keywordPattern]"
	bind .sob.kp <Return> "Option set keywordPattern \[.sob.kp get\]"

	button .sob.close -text "Close" -relief raised \
		-command "destroy .sob"

	pack append .sob \
		.sob.title {top fill} \
		.sob.l1 {top fill} \
		.sob.lof {top fill} \
		.sob.l2 {top fill} \
		.sob.kp {top fill} \
		.sob.close {top fill}
}

proc MakeOtherOptionsBox {} {
	catch "destroy .oob"

	toplevel .oob
	wm title .oob "Other Point Options"
	wm iconname .oob "Other Point Options"

	label .oob.title -text "Change by typing a new value then Return" \
		-relief raised

	label .oob.l1 -text "Format for the names of backup files"
	entry .oob.bnf -relief sunken -fg red -exportselection no
	.oob.bnf insert 0 "[Option get backupNameFormat]"
	bind .oob.bnf <Return> "Option set backupNameFormat \[%W get\]"

	label .oob.l2 -text "Default geometry for text windows"
	entry .oob.dtg -relief sunken -fg red -exportselection no
	.oob.dtg insert 0 "[Option get textGeometry]"
	bind .oob.dtg <Return> "Option set textGeometry \[%W get\]"

	label .oob.l3 -text "Default geometry for browser windows"
	entry .oob.bdg -relief sunken -fg red -exportselection no
	.oob.bdg insert 0 "[Option get browserGeometry]"
	bind .oob.bdg <Return> "Option set browserGeometry \[%W get\]"

	label .oob.l4 -text "Pattern for files in the file browser window"
	entry .oob.fp -relief sunken -fg red -exportselection no
	.oob.fp insert 0 "[Option get filePattern]"
	bind .oob.fp <Return> "Option set filePattern \[%W get\]"

	label .oob.l5 -text "Format of text window titles"
	entry .oob.ttf -relief sunken -fg red -exportselection no
	.oob.ttf insert 0 "[Option get textTitleFormat]"
	bind .oob.ttf <Return> "Option set textTitleFormat \[%W get\]"

	label .oob.l6 -text "Format of text window icons"
	entry .oob.tif -relief sunken -fg red -exportselection no
	.oob.tif insert 0 "[Option get textIconFormat]"
	bind .oob.tif <Return> "Option set textIconFormat \[%W get\]"

	button .oob.close -text "Close" -relief raised -command "destroy .oob"

	pack append .oob \
		.oob.title {top fill} \
		.oob.l1 {top fill} \
		.oob.bnf {top fill} \
		.oob.l2 {top fill} \
		.oob.dtg {top fill} \
		.oob.l3 {top fill} \
		.oob.bdg {top fill} \
		.oob.l4 {top fill} \
		.oob.fp {top fill} \
		.oob.l5 {top fill} \
		.oob.ttf {top fill} \
		.oob.l6 {top fill} \
		.oob.tif {top fill} \
		.oob.close {top fill}
}

proc MakeVerifyBox {} {
	catch {destroy .verifyBox}
	toplevel .verifyBox -relief raised
	wm title .verifyBox "Verify replacement"
	wm iconname .verifyBox "Verify replacement"
	label .verifyBox.label -text "Make this replacement?"
	button .verifyBox.yes -text "Yes, replace" \
		-command {
			global verifyString
			set verifyString "y"
		}
	button .verifyBox.no -text "No, do not replace" \
		-command {
			global verifyString
			set verifyString "n"
		}
	button .verifyBox.noverify -text "Replace the rest with no verify" \
		-command {
			global verifyString
			set verifyString "v"
		}
	button .verifyBox.cancel -text "Cancel replace" \
		-command {
			global verifyString
			set verifyString "c"
		}
	pack append .verifyBox \
		.verifyBox.label {top fill} \
		.verifyBox.yes {top fill} \
		.verifyBox.no {top fill} \
		.verifyBox.noverify {top fill} \
		.verifyBox.cancel {top fill}
}


proc DoRegexReplace {name} {
	global verify inSelection doFast

	# prevent an infinite loop when searches wrap around
	set saveWrap [Option get wrapAroundSearches]
	Option set wrapAroundSearches 0

	set searchFor [$name.searchFor get]
	set replaceWith [$name.replaceWith get]
	set sel [Sel get]
	set begin [lindex $sel 0]
	set end [lindex $sel 1]
	set replaceCount 0

	Undo begin
	if $verify {
		MakeVerifyBox
		set up "update"
	} else {
		if $doFast {
			RegexReplaceAll $searchFor $replaceWith $inSelection
			Undo end
			return
		} else {
			set up "noupdate"
		}
	}
	for {} {1} {} {
		set sucess [RegexSearch "$searchFor" forward {} $up]
		if {$sucess<0} break
		if {$inSelection && $sucess>$end} break
		if $verify {
			.verifyBox.label configure \
				-text "Make this replacement?"
			tkwait variable verifyString
			global verifyString
			set doReplace [string compare $verifyString "n"]
			if {[string compare $verifyString "c"]==0}  break
			if {[string compare $verifyString "v"]==0} {
				set verify 0
				set up "noupdate"
			}
			.verifyBox.label configure -text "Searching"
		} else {
			set doReplace 1
		}
		if $doReplace {
			set diffLengths \
				[RegexReplaceOne $searchFor $replaceWith]
			if $inSelection {
				set end [expr {$end + $diffLengths}]
			}
			incr replaceCount
		}
	}
	Undo end
	catch {destroy .verifyBox}
	Redraw
	Option set wrapAroundSearches $saveWrap
	if {$replaceCount==1} { set plural "" } { set plural "s" }
	MessageLine [format "Made %d replacement%s" $replaceCount $plural]
}

proc DoReplace {name} {
	global verify inSelection doFast

	# prevent an infinite loop when searches wrap around
	set saveWrap [Option get wrapAroundSearches]
	Option set wrapAroundSearches 0

	set searchFor [$name.searchFor get]
	set replaceWith [$name.replaceWith get]
	set sel [Sel get]
	set begin [lindex $sel 0]
	set end [lindex $sel 1]
	set diffLengths [expr {
		[string length $replaceWith] - [string length $searchFor]}]
	set replaceCount 0

	Undo begin
	if $verify {
		MakeVerifyBox
		set up "update"
	} else {
		if $doFast {
			Replace $searchFor $replaceWith $inSelection
			Undo end
			return
		} else {
			set up "noupdate"
		}
	}
	for {} {1} {} {
		set sucess [Search "$searchFor" forward {} $up]
		if {$sucess<0} break
		if {$inSelection && $sucess>$end} break
		if $verify {
			.verifyBox.label configure \
				-text "Make this replacement?"
			tkwait variable verifyString
			global verifyString
			set doReplace [string compare $verifyString "n"]
			if {[string compare $verifyString "c"]==0}  break
			if {[string compare $verifyString "v"]==0} {
				set verify 0
				set up "noupdate"
			}
			.verifyBox.label configure -text "Searching"
		} else {
			set doReplace 1
		}
		if $doReplace {
			DeleteToScrap $up
			InsertString "$replaceWith" $up
			if $inSelection {
				set end [expr {$end + $diffLengths}]
			}
			incr replaceCount
		}
	}
	Undo end
	catch {destroy .verifyBox}
	Redraw
	Option set wrapAroundSearches $saveWrap
	if {$replaceCount==1} { set plural "" } { set plural "s" }
	MessageLine [format "Made %d replacement%s" $replaceCount $plural]
}

proc MakeRegexReplaceBox {} {
	global verify inSelection doFast

	toplevel .regexReplaceBox -relief raised
	wm title .regexReplaceBox "Regex Search and Replace"
	wm iconname .regexReplaceBox "Regex Search and Replace"

	label .regexReplaceBox.label1 -text "RE to search for:"
	label .regexReplaceBox.label2 \
			-text "Special chars: . \[ \] \\\\ * + \\< \\> ^ $"
	entry .regexReplaceBox.searchFor -relief sunken -exportselection no

	label .regexReplaceBox.label3 -text "And replace it with:"
	label .regexReplaceBox.label4 \
				-text "Special chars: & \\\\ \\1 \\2 ... \\9"
	entry .regexReplaceBox.replaceWith -relief sunken -exportselection no

	checkbutton .regexReplaceBox.verify \
		-text "Verify each replacment                   "
	.regexReplaceBox.verify select

	checkbutton .regexReplaceBox.doFast \
		-text "Use fast internal replace (if no verify)"

	checkbutton .regexReplaceBox.inSelection \
		-text "Replace within the selection only        "

	button .regexReplaceBox.begin -text "Begin replacing" \
				-command "DoRegexReplace .regexReplaceBox"

	button .regexReplaceBox.close -text "Close" \
		-command {
			destroy .regexReplaceBox
			catch {destroy .verifyBox}
		}

	pack append .regexReplaceBox \
		.regexReplaceBox.label1 { top fill } \
		.regexReplaceBox.label2 { top fill } \
		.regexReplaceBox.searchFor { top fill } \
		.regexReplaceBox.label3 { top fill } \
		.regexReplaceBox.label4 { top fill } \
		.regexReplaceBox.replaceWith { top fill } \
		.regexReplaceBox.verify { top fill } \
		.regexReplaceBox.doFast { top fill } \
		.regexReplaceBox.inSelection { top fill } \
		.regexReplaceBox.begin { top fill } \
		.regexReplaceBox.close { top fill }
}

proc MakeReplaceBox {} {
	global verify inSelection doFast

	toplevel .replaceBox -relief raised
	wm title .replaceBox "Search and Replace"
	wm iconname .replaceBox "Search and Replace"

	label .replaceBox.label1 -text "Search for:"
	entry .replaceBox.searchFor -relief sunken -exportselection no

	label .replaceBox.label2 -text "And replace it with:"
	entry .replaceBox.replaceWith -relief sunken -exportselection no

	checkbutton .replaceBox.verify \
		-text "Verify each replacment                   "
	.replaceBox.verify select

	checkbutton .replaceBox.doFast \
		-text "Use fast internal replace (if no verify)"

	checkbutton .replaceBox.inSelection \
		-text "Replace within the selection only        "

	button .replaceBox.begin -text "Begin replacing" \
					-command "DoReplace .replaceBox"

	button .replaceBox.close -text "Close" \
		-command {
			destroy .replaceBox
			catch {destroy .verifyBox}
		}

	pack append .replaceBox \
		.replaceBox.label1 { top fill } \
		.replaceBox.searchFor { top fill } \
		.replaceBox.label2 { top fill } \
		.replaceBox.replaceWith { top fill} \
		.replaceBox.verify { top fill } \
		.replaceBox.doFast { top fill } \
		.replaceBox.inSelection { top fill } \
		.replaceBox.begin { top fill } \
		.replaceBox.close { top fill }
}

proc MakeModalEntry {title prompt okay cancel} {
	catch {destroy .modalEntry}
	toplevel .modalEntry -relief raised
	wm title .modalEntry $title
	wm iconname .modalEntry $title
	message .modalEntry.label -text $prompt -aspect 800
	entry .modalEntry.entry -relief sunken -exportselection no
	bind .modalEntry.entry <Return> {
		global returnString
		set returnString [.modalEntry.entry get]
		destroy .modalEntry
	}
	frame .modalEntry.buttons
	button .modalEntry.buttons.okay -text $okay -pady 10 -command {
		global returnString
		set returnString [.modalEntry.entry get]
		destroy .modalEntry
	}
	button .modalEntry.buttons.cancel -text $cancel -pady 10 -command {
		global returnString
		set returnString "XXXcancelXXX"
		destroy .modalEntry
	}
	pack append .modalEntry.buttons \
		.modalEntry.buttons.okay {left expand fill} \
		.modalEntry.buttons.cancel {right expand fill}
	pack append .modalEntry \
		.modalEntry.label {top fill} \
		.modalEntry.entry {top fill} \
		.modalEntry.buttons {top fill}
	global returnString
	tkwait visibility .modalEntry
	grab .modalEntry
	tkwait window .modalEntry
	return $returnString
}

proc MakeModalYesMaybeNo {title prompt okay maybe cancel} {
	catch {destroy .modalYesNo}
	toplevel .modalYesNo -relief raised
	wm title .modalYesNo $title
	wm iconname .modalYesNo $title
	message .modalYesNo.label -text $prompt -aspect 800
	frame .modalYesNo.buttons
	button .modalYesNo.buttons.okay -text $okay -pady 10 \
		-command {
			global returnString
			set returnString "y"
			destroy .modalYesNo
		}
	button .modalYesNo.buttons.maybe -text $maybe -pady 10 \
		-command {
			global returnString
			set returnString "m"
			destroy .modalYesNo
		}
	button .modalYesNo.buttons.cancel -text $cancel -pady 10 \
		-command {
			global returnString
			set returnString "n"
			destroy .modalYesNo
		}
	pack append .modalYesNo.buttons \
		.modalYesNo.buttons.okay {left expand fill} \
		.modalYesNo.buttons.maybe {left expand fill} \
		.modalYesNo.buttons.cancel {right expand fill}
	pack append .modalYesNo \
		.modalYesNo.label {top fill} \
		.modalYesNo.buttons {top fill}
	global returnString
	tkwait visibility .modalYesNo
	grab .modalYesNo
	tkwait window .modalYesNo
	return $returnString
}

proc MakeModalYesNo {title prompt okay cancel} {
	catch {destroy .modalYesNo}
	toplevel .modalYesNo -relief raised
	wm title .modalYesNo $title
	wm iconname .modalYesNo $title
	message .modalYesNo.message -text $prompt -aspect 800
	frame .modalYesNo.buttons
	button .modalYesNo.buttons.okay -text $okay -pady 10 \
		-command {
			global returnString
			set returnString "y"
			destroy .modalYesNo
		}
	button .modalYesNo.buttons.cancel -text $cancel -pady 10 \
		-command {
			global returnString
			set returnString "n"
			destroy .modalYesNo
		}
	pack append .modalYesNo.buttons \
		.modalYesNo.buttons.okay {left expand fill} \
		.modalYesNo.buttons.cancel {right expand fill}
	pack append .modalYesNo \
		.modalYesNo.message {top fill} \
		.modalYesNo.buttons {top fill}
	global returnString
	tkwait visibility .modalYesNo
	grab .modalYesNo
	tkwait window .modalYesNo
	return $returnString
}

proc MakeMsgBox {msg} {
	global wcounter

	set wcounter [expr $wcounter+1]
	set name [format ".mb%05d" $wcounter]

	toplevel $name -class PtPopup -relief raised
	wm title $name "Message"
	wm iconname $name "Message"
	message $name.msg -text "$msg"
	button $name.close -text "Close" -command "destroy $name"

	pack append $name \
		$name.msg {top fill expand} \
		$name.close {bottom fill}
}

proc MakemmBox {} {
	catch {destroy .mmBox}
	toplevel .mmBox -relief raised
	wm title .mmBox "Mouse Menu Parameters"
	wm iconname .mmBox "Mouse Menu Parameters"
	label .mmBox.title -text "Set Mouse Menu Commands"
	frame .mmBox.frame -relief raised
	frame .mmBox.frame.dirs -relief raised
	label .mmBox.frame.dirs.no -text "No motion"
	label .mmBox.frame.dirs.n -text "North"
	label .mmBox.frame.dirs.e -text "East"
	label .mmBox.frame.dirs.s -text "South"
	label .mmBox.frame.dirs.w -text "West"
	pack append .mmBox.frame.dirs \
		.mmBox.frame.dirs.no {top fill} \
		.mmBox.frame.dirs.n {top fill} \
		.mmBox.frame.dirs.e {top fill} \
		.mmBox.frame.dirs.s {top fill} \
		.mmBox.frame.dirs.w {top fill}
	frame .mmBox.frame.titles -relief raised
	entry .mmBox.frame.titles.no -relief sunken -exportselection no
	.mmBox.frame.titles.no insert 0 "[Option get lmm1]"
	bind .mmBox.frame.titles.no <Key-Return> \
		"Option set lmm1 \[.mmBox.frame.titles.no get\]"
	entry .mmBox.frame.titles.n -relief sunken -exportselection no
	bind .mmBox.frame.titles.n <Key-Return> \
		"Option set lmm1n \[.mmBox.frame.titles.n get\]"
	entry .mmBox.frame.titles.e -relief sunken -exportselection no
	bind .mmBox.frame.titles.e <Key-Return> \
		"Option set lmm1e \[.mmBox.frame.titles.e get\]"
	entry .mmBox.frame.titles.s -relief sunken -exportselection no
	bind .mmBox.frame.titles.s <Key-Return> \
		"Option set lmm1s \[.mmBox.frame.titles.s get\]"
	entry .mmBox.frame.titles.w -relief sunken -exportselection no
	bind .mmBox.frame.titles.w <Key-Return> \
		"Option set lmm1w \[.mmBox.frame.titles.e get\]"
	pack append .mmBox.frame.titles \
		.mmBox.frame.titles.no {top fill} \
		.mmBox.frame.titles.n {top fill} \
		.mmBox.frame.titles.e {top fill} \
		.mmBox.frame.titles.s {top fill} \
		.mmBox.frame.titles.w {top fill}
	frame .mmBox.frame.cmds -relief raised
	entry .mmBox.frame.cmds.no -relief sunken -exportselection no
	bind .mmBox.frame.titles.no <Key-Return> \
		{puts stdout "cmm1 str is [.mmBox.frame.titles.no get]"
		Option set cmm1 [.mmBox.frame.titles.no get]}
	entry .mmBox.frame.cmds.n -relief sunken -exportselection no
	bind .mmBox.frame.titles.n <Key-Return> \
		"Option set cmm1n \[.mmBox.frame.titles.n get\]"
	entry .mmBox.frame.cmds.e -relief sunken -exportselection no
	bind .mmBox.frame.titles.e <Key-Return> \
		"Option set cmm1e \[.mmBox.frame.titles.e get\]"
	entry .mmBox.frame.cmds.s -relief sunken -exportselection no
	bind .mmBox.frame.titles.s <Key-Return> \
		"Option set cmm1s \[.mmBox.frame.titles.s get\]"
	entry .mmBox.frame.cmds.w -relief sunken -exportselection no
	bind .mmBox.frame.titles.w <Key-Return> \
		"Option set cmm1w \[.mmBox.frame.titles.w get\]"
	pack append .mmBox.frame.cmds \
		.mmBox.frame.cmds.no {top fill} \
		.mmBox.frame.cmds.n {top fill} \
		.mmBox.frame.cmds.e {top fill} \
		.mmBox.frame.cmds.s {top fill} \
		.mmBox.frame.cmds.w {top fill}
	pack append .mmBox.frame \
		.mmBox.frame.dirs {left fill} \
		.mmBox.frame.titles {left fill} \
		.mmBox.frame.cmds {right fill expand}
	button .mmBox.close -text "Close" -command {destroy .mmBox}
	pack append .mmBox \
		.mmBox.title {top fill } \
		.mmBox.frame {top fill} \
		.mmBox.close {bottom fill}
}

proc MakeUndoBox {} {
	catch {destroy .ub}
	toplevel .ub
	wm title .ub "Command History"
	wm iconname .ub "Command History"
	wm minsize .ub 0 0
	label .ub.title -text "Command History Box" -relief flat
	frame .ub.list -relief flat
	scrollbar .ub.list.scrollbar -relief sunken \
		-command ".ub.list.list yview"
	listbox .ub.list.list -yscrollcommand ".ub.list.scrollbar set"  \
		-relief sunken -geometry 60x20
	tk_listboxSingleSelect .ub.list.list
	pack append .ub.list \
		.ub.list.scrollbar {left fill} \
		.ub.list.list {right fill expand}
	frame .ub.buttons -relief raised
#I can't figure out what this was supposed to be for???
#	bind .ub.list.list <Double-1> ".ub.list config -bg \[Sel escaped\]"
#
	button .ub.buttons.undo -text "Undo" -command "Undo 1"
	button .ub.buttons.redo -text "Redo" -command "Redo 1"
	button .ub.buttons.update -text "Update" -command "Undo update"
	button .ub.buttons.again -text "Again" -command "Again"
	button .ub.close -text "Close" -command "destroy .ub"
	pack append .ub.buttons \
		.ub.buttons.undo {left fill expand} \
		.ub.buttons.redo {left fill expand} \
		.ub.buttons.update {left fill expand} \
		.ub.buttons.again {right fill expand}
	pack append .ub \
		.ub.title {top fill} \
		.ub.list {top fill expand} \
		.ub.buttons {top fill} \
		.ub.close {bottom fill}
}

proc MakeColorBox {} {
	catch {destroy .cb}
	toplevel .cb
	wm title .cb "Select Text Colors"
	wm iconname .cb "Select Text Colors"
	wm minsize .cb 0 0
	label .cb.title -text "Color Selection Box" -relief flat \
		-borderwidth 5
	frame .cb.list -relief flat -borderwidth 40
	scrollbar .cb.list.scrollbar -relief sunken \
		-command ".cb.list.colors yview"
	listbox .cb.list.colors -yscrollcommand ".cb.list.scrollbar set"  \
		-relief sunken -geometry 20x20
	tk_listboxSingleSelect .cb.list.colors
	if ![LoadColors .cb.list.colors] {
		DefaultLoadColors .cb.list.colors
	}
	pack append .cb.list \
		.cb.list.scrollbar {left fill} \
		.cb.list.colors {right fill expand}
	frame .cb.buttons -relief raised
	bind .cb.list.colors <Double-1> \
		".cb.list config -bg \[selection get\]"
	button .cb.buttons.norfore \
		-text "Set normal foreground color" \
		-command "SetTextColor \[selection get\] normal foreground"
	button .cb.buttons.norback \
		-text "Set normal background color" \
		-command "SetTextColor \[selection get\] normal background"
	button .cb.buttons.selfore \
		-text "Set selected foreground color" \
		-command "SetTextColor \[selection get\] selected foreground"
	button .cb.buttons.selback \
		-text "Set selected background color" \
		-command "SetTextColor \[selection get\] selected background"
	button .cb.buttons.deselfore \
		-text "Set deselected foreground color" \
		-command "SetTextColor \[selection get\] deselected foreground"
	button .cb.buttons.deselback \
		-text "Set deselected background color" \
		-command "SetTextColor \[selection get\] deselected background"
	button .cb.buttons.close -text "Close" \
		-command "destroy .cb"
	pack append .cb.buttons \
		.cb.buttons.norfore {top fill} \
		.cb.buttons.norback {top fill} \
		.cb.buttons.selfore {top fill} \
		.cb.buttons.selback {top fill} \
		.cb.buttons.deselfore {top fill} \
		.cb.buttons.deselback {top fill} \
		.cb.buttons.close {top fill}
	pack append .cb \
		.cb.title {top fill} \
		.cb.list {top fill expand} \
		.cb.buttons {bottom fill}
}

proc LoadColors {w} {
	set ColorFile "/usr/lib/X11/rgb.txt"
	if ![file readable $ColorFile] {
		return 0
	}
	set fid [open $ColorFile r]
	set colorList ""
	while 1 {
		set line [gets $fid]
		if {$line==""} break
		regexp {[a-zA-Z].*} $line colorName
		lappend colorList $colorName
	}
	set colorList [lsort $colorList]
	eval $w insert end $colorList
	close $fid
	return 1
}

proc DefaultLoadColors {w} {
	$w insert 0 \
aliceblue antiquewhite aquamarine aquamarine1 aquamarine2 \
aquamarine3 aquamarine4 azure azure1 azure2 azure3 azure4 \
beige bisque bisque1 bisque2 bisque3 bisque4 black \
blanchedalmond blue blue1 blue2 blue3 blue4 \
blueviolet brown brown1 brown2 brown3 brown4 burlywood \
burlywood1 burlywood2 burlywood3 burlywood4 cadetblue cadetblue \
chartreuse chartreuse1 chartreuse2 chartreuse3 chartreuse4 \
chocolate chocolate1 chocolate2 chocolate3 chocolate4 coral \
coral1 coral2 coral3 coral4 cornflowerblue cornsilk cornsilk1 \
cornsilk2 cornsilk3 cornsilk4 cyan cyan1 cyan2 cyan3 cyan4 \
darkgoldenrod darkgreen darkkhaki darkolivegreen darkorange \
darkorchid darksalmon darkseagreen darkslateblue \
darkslategray darkturquoise darkviolet darkgoldenrod \
darkolivegreen3 darkolivegreen4 darkslategrey deeppink \
deepskyblue dimgray dodgerblue firebrick firebrick1 \
firebrick2 firebrick3 firebrick4 floralwhite forestgreen \
gainsboro ghostwhite gold gold1 gold2 gold3 gold4 \
goldenrod goldenrod1 goldenrod2 goldenrod3 goldenrod4 gray \
gray0 gray1 gray10 gray100 gray11 gray12 gray13 gray14 \
gray15 gray16 gray17 gray18 gray19 gray2 gray20 gray21 \
gray22 gray23 gray24 gray25 gray26 gray27 gray28 gray29 \
gray3 gray30 gray31 gray32 gray33 gray34 gray35 gray36 \
gray37 gray38 gray39 gray4 gray40 gray41 gray42 gray43 \
gray44 gray45 gray46 gray47 gray48 gray49 gray5 gray50 \
gray51 gray52 gray53 gray54 gray55 gray56 gray57 gray58 \
gray59 gray6 gray60 gray61 gray62 gray63 gray64 gray65 \
gray66 gray67 gray68 gray69 gray7 gray70 gray71 gray72 \
gray73 gray74 gray75 gray76 gray77 gray78 gray79 gray8 \
gray80 gray81 gray82 gray83 gray84 gray85 gray86 gray87 \
gray88 gray89 gray9 gray90 gray91 gray92 gray93 gray94 \
gray95 gray96 gray97 gray98 gray99 greenyellow green green1 \
green2 green3 green4 honeydew honeydew1 honeydew2 honeydew3 \
honeydew4 hotpink indianred ivory ivory1 ivory2 ivory3 \
ivory4 khaki khaki1 khaki2 khaki3 khaki4 lavenderblush \
lavender lawngreen lemonchiffon lightblue lightcoral \
lightcyan lightgoldenrodyellow lightgoldenrod lightgray \
lightpink lightsalmon lightseagreen lightskyblue \
lightslateblue lightslategray lightsteelblue lightyellow \
lightyellow1 lightyellow2 lightyellow3 lightyellow4 \
limegreen linen magenta magenta1 magenta2 magenta3 magenta4 \
maroon maroon1 maroon2 maroon3 maroon4 mediumaquamarine \
mediumblue mediumorchid mediumpurple mediumseagreen \
mediumslateblue mediumspringgreen mediumturquoise \
mediumvioletred midnightblue mintcream mistyrose moccasin \
navajowhite navajowhite1 navajowhite2 navajowhite3 navajowhite4 \
navyblue navy navyblue oldlace olivedrab orangered orange \
orange1 orange2 orange3 orange4 orchid orchid1 orchid2 \
orchid3 orchid4 palegoldenrod palegreen paleturquoise \
palevioletred papayawhip peachpuff peru pink pink1 pink2 \
pink3 pink4 plum plum1 plum2 plum3 plum4 powderblue purple \
purple1 purple2 purple3 purple4 red red1 red2 red3 red4 \
rosybrown royalblue saddlebrown salmon salmon1 salmon2 \
salmon3 salmon4 sandybrown seagreen seashell seashell1 \
seashell2 seashell3 seashell4 sienna sienna1 sienna2 sienna3 \
sienna4 skyblue slateblue slategray slategrey slategrey \
snow snow1 snow2 snow3 snow4 springgreen steelblue tan \
tan1 tan2 tan3 tan4 thistle thistle1 thistle2 thistle3 \
thistle4 tomato tomato1 tomato2 tomato3 tomato4 turquoise \
turquoise1 turquoise2 turquoise3 turquoise4 violetred violet \
wheat wheat1 wheat2 wheat3 wheat4 whitesmoke white \
yellowgreen yellow yellow1 yellow2 yellow3 yellow4
}

proc MakeCtagBox {} {
	catch {destroy .cTagBox}
	toplevel .cTagBox -relief raised
	wm title .cTagBox "Find C Tag"
	wm iconname .cTagBox "Find C Tag"
	label .cTagBox.label -text "C tag to search for:"
	entry .cTagBox.entry -relief sunken -exportselection no
	bind .cTagBox.entry <Return> {CTag [.cTagBox.entry get]}
	button .cTagBox.enter -text "Find C tag" \
		-command { CTag [.cTagBox.entry get] }
	button .cTagBox.close -text "Close" -command "destroy .cTagBox"
	pack append .cTagBox \
		.cTagBox.label {top fill} \
		.cTagBox.entry {top fill} \
		.cTagBox.enter {top fill} \
		.cTagBox.close {top fill}
}

proc FillKeywordBox {kb} {
	set keyword [$kb.keyword get]
	set files [$kb.files get]
	set infiles [glob -nocomplain $files]
	if {"$infiles"!=""} {
		set cmd [concat exec grep -l \"$keyword\" $infiles]
		set ok [catch {eval $cmd} outfiles]
	} else {
		set ok 1
	}
	if {$ok!=0} {
		set outfiles "***No_Matches***"
	}
	catch {$kb.slist.filenames delete 0 end}
	foreach file $outfiles {
		$kb.slist.filenames insert end $file
	}
}

proc MakeKeywordBox {} {
        global wcounter location1

        set wcounter [expr $wcounter+1]
        set name [format ".kb%05d" $wcounter]

        toplevel $name -relief raised
        wm title $name "Keyword Search"
        wm iconname $name "Keyword Search"
        wm minsize $name 0 0
        
        label $name.label1 -text "Search for:"
        entry $name.keyword -relief sunken
        bind $name.keyword <Return> "FillKeywordBox $name"

        label $name.label2 -text "in files:"
        entry $name.files -relief sunken
        bind $name.files <Return> "FillKeywordBox $name"
        $name.files insert 0 "[Option get keywordPattern]"

        frame $name.slist
        scrollbar $name.slist.scrollbar -relief sunken \
                -command "$name.slist.filenames yview"
        listbox $name.slist.filenames \
                -yscrollcommand "$name.slist.scrollbar set" -relief sunken
	tk_listboxSingleSelect $name.slist.filenames
        bind $name.slist.filenames <1> "handleKeywordBoxButton %W %y $name"
        pack append $name.slist \
                $name.slist.scrollbar {left fill} \
                $name.slist.filenames {right fill expand}
        button $name.find -text "Find keyword in files" \
                -command "FillKeywordBox $name"

        button $name.close -text "Close" -command "destroy $name"
        pack append $name \
                $name.label1 {top fill} \
                $name.keyword {top fill} \
                $name.label2 {top fill} \
                $name.files {top fill} \
                $name.slist {top fill} \
                $name.find {top fill} \
                $name.close {top fill}
        return $name
}


proc removeStars {name} {
	if [string compare [string range $name 0 0] "*"] {
		return $name
	} else {
		return [removeStars [string range $name 1 end]]
	}
}

proc handleKeywordBoxButton {window yCoord name} {
	global location1
	set index [$window nearest $yCoord]
	set item1 [$window get $index]
	set item [removeStars $item1]
	OpenWindow $item $location1
	set star "*"
	set item $star$item
	$window delete $index
	$window insert $index $item
	Search [$name.keyword get] forward
}


proc MakeGotoBox {} {
	catch {destroy .gotoBox}
	toplevel .gotoBox -relief raised
	wm title .gotoBox "Go To Line"
	wm iconname .gotoBox "Go To Line"
	label .gotoBox.label -text "Line number to go to:"
	entry .gotoBox.entry -relief sunken -exportselection no
	bind .gotoBox.entry <Return> {GotoLine [.gotoBox.entry get] lof}
	button .gotoBox.enter -text "Go to line" \
		-command {GotoLine [.gotoBox.entry get] lof}
	button .gotoBox.close -text "Close" -command "destroy .gotoBox"
	pack append .gotoBox \
		.gotoBox.label {top fill} \
		.gotoBox.entry {top fill} \
		.gotoBox.enter {top fill} \
		.gotoBox.close {top fill}
}

proc MakeDebugBox {} {
	catch {destroy .debugBox}
	toplevel .debugBox -relief raised
	wm title .debugBox "Set Debug Variable"
	wm iconname .debugBox "Set Debug Variable"
	label .debugBox.label -text "Set value of `debug' to:"
	entry .debugBox.entry -relief sunken -exportselection no
	bind .debugBox.entry <Return> \
		{Option set debug [.debugBox.entry get]}
	button .debugBox.enter -text "Set debug" \
		-command {Option set debug [.debugBox.entry get]}
	button .debugBox.close -text "Close" \
		-command "destroy .debugBox"
	pack append .debugBox \
		.debugBox.label {top fill} \
		.debugBox.entry {top fill} \
		.debugBox.enter {top fill} \
		.debugBox.close {top fill}
}

proc MakeAsciiBox {} {
	catch {destroy .asciiBox}
	toplevel .asciiBox -relief raised
	wm title .asciiBox "Insert ASCII"
	wm iconname .asciiBox "Insert ASCII"
	label .asciiBox.label -text "Enter decimal ASCII character"
	entry .asciiBox.entry -relief sunken -exportselection no
	bind .asciiBox.entry <Return> {InsertAscii [.asciiBox.entry get]}
	button .asciiBox.enter -text "Enter character" \
		-command {InsertAscii [.asciiBox.entry get]}
	button .asciiBox.close -text "Close" \
		-command "destroy .asciiBox"
	pack append .asciiBox \
		.asciiBox.label {top fill} \
		.asciiBox.entry {top fill} \
		.asciiBox.enter {top fill} \
		.asciiBox.close {top fill}
}

proc AnyFilesChanged {} {
	foreach w [GetTextWindowList] {
		set info [GetFileInfo $w]
		set changed [lindex $info 3]
		if [expr 4==($changed&4)] { return 1 }
	}
	return 0
}

proc MakeQuitBox {} {
	catch {destroy .quitBox}
	if ![AnyFilesChanged] { QuitPoint ask; return }
	toplevel .quitBox -relief raised
	wm title .quitBox "Quit Point"
	wm iconname .quitBox "Quit Point"
	label .quitBox.label -text "*** There are unsaved files ***"
	button .quitBox.saveAndQuit \
		-text "Save unsaved files and quit Point" \
		-command "QuitPoint save"
	button .quitBox.askAndQuit \
		-text "Ask about each unsaved file and quit Point" \
		-command "QuitPoint ask"
	button .quitBox.discardAndQuit \
		-text "Discard unsaved edits and quit Point" \
		-command "QuitPoint discard"
	button .quitBox.cancelQuit -text "Do not quit Point" \
		-command "destroy .quitBox"
	pack append .quitBox \
		.quitBox.label {top fill} \
		.quitBox.saveAndQuit {top fill} \
		.quitBox.askAndQuit {top fill} \
		.quitBox.discardAndQuit {top fill} \
		.quitBox.cancelQuit {top fill}
}

proc SearchBoxProc {} {
	global searchDirection reSearch
	set str [.searchBox.string get]
	if {$reSearch==1} {
		RegexSearch $str $searchDirection
	} else {
		Search $str $searchDirection
	}
}

proc MakeSearchBox {{regex normal}} {
	global searchDirection reSearch
	catch {destroy .searchBox}
	toplevel .searchBox -class "SearchWindow" -relief raised
	wm title .searchBox "String Search"
	wm iconname .searchBox "String Search"
	label .searchBox.label -text "Search string:"
	entry .searchBox.string -relief sunken -exportselection no
	bind .searchBox.string <Return> \
				{Search "[.searchBox.string get]" forward}
	checkbutton .searchBox.reSearch \
		-text "Regular expression search        " \
		-variable reSearch \
		-command { Option set ignoreCase $ignoreCase }
	label .searchBox.reExplain -text \
				"(^,$,.,*,+,\[,\],\\ are special in REs)"
	if ![string compare $regex normal] {
		.searchBox.reSearch deselect
	} else {
		.searchBox.reSearch select
	}
	checkbutton .searchBox.ignoreCase \
		-text "Ignore case in search            " \
		-variable ignoreCase \
		-command { Option set ignoreCase $ignoreCase }
	set xx [Option get ignoreCase]
	if {$xx==1} {
		.searchBox.ignoreCase select
	} else {
		.searchBox.ignoreCase deselect
	}
	checkbutton .searchBox.findWholeWords \
		-text "Find whole words only            " \
		-variable findWholeWords \
		-command { Option set findWholeWords $findWholeWords }
	set xx [Option get findWholeWords]
	if {$xx==1} {
		.searchBox.findWholeWords select
	} else {
		.searchBox.findWholeWords deselect
	}
	checkbutton .searchBox.searchDirection \
		-text "Search backwards                 " \
		-variable searchDirection \
		-offvalue "forward" -onvalue "backward"
	frame .searchBox.buttons
	button .searchBox.buttons.search -text "Search" \
		-command SearchBoxProc
	button .searchBox.buttons.close -text "Close" \
		-command "destroy .searchBox"
	pack append .searchBox.buttons \
		.searchBox.buttons.search { left fill expand } \
		.searchBox.buttons.close { left fill expand }
	pack append .searchBox \
		.searchBox.label { top fill } \
		.searchBox.string { top fill } \
		.searchBox.reSearch { top fill } \
		.searchBox.reExplain { top fill } \
		.searchBox.ignoreCase { top fill } \
		.searchBox.findWholeWords { top fill } \
		.searchBox.searchDirection { top fill } \
		.searchBox.buttons { bottom fill }
}


