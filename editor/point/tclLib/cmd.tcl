proc MakeKeyBindingsBox {} {
	#
	# first create the box, talk to thw wm and create the title
	#
	catch {destroy .kbb}
	toplevel .kbb
	wm title .kbb "Key Bindings"
	wm iconname .kbb "Key Bindings"
	wm minsize .kbb 0 0
	label .kbb.title -text "Key Bindings Box" -relief raised
	#
	# make a list of commands
	#
	global cmd_list
	set cmd_list {
		{***MAKE_BOXES***}
		{MakeRegexReplaceBox}
		{MakeReplaceBox}
		{MakeMsgBox msg}
		{MakeUndoBox}
		{MakeColorBox}
		{MakeCtagBox}
		{MakeKeywordBox}
		{MakeGotoBox}
		{MakeAsciiBox}
		{MakeQuitBox}
		{MakeSearchBox}
		{MakeKeyBindingsBox}
		{***COPYING***}
		{CopySelToMouse}
		{MoveSelToMouse}
		{CopyToHereMode}
		{MoveToHereMode}
		{***UNDO***}
		{Again}
		{Redo}
		{Undo}
		{ShowUndoStack}
		{***SEARCHING***}
		{Search string forward/backward}
		{RegexSearch pattern}
		{RepeatSearch forward/backward}
		{RepeatRegexSearch}
		{Replace from to inselection/""}
		{RegexReplaceOne from to one}
		{RegexReplaceAll from to inselection/""}
		{FindMatchingBracket}
		{CTag tag}
		{***MOVING***}
		{MoveToEndFile}
		{ShowSelection}
		{MoveToLastPlace}
		{GotoLine lineNumber lof/""}
		{GotoDigit digit}
		{MoveSel char/word/line left/right/up/down}
		{***EDITING***}
		{DeleteToScrap}
		{CopySelToScrap}
		{ExchangeWithScrap}
		{InsertFromScrap}
		{InsertString string}
		{InsertAscii asciiCode}
		{ChangeCaseOfSel}
		{JustifySel}
		{Sel set begin end window/"" char/word/line}
		{Sel get}
		{***TEXT_WINDOWS***}
		{OpenWindow fileName geometry}
		{CloseWindow save/ask/nosave}
		{MoveWindow geometry}
		{SetTextColor normal/sel foreground/back}
		{TextFont fontName}
		{LowerWindow}
		{RaiseWindow}
		{RaiseListWindow index}
		{ToggleReadOnly}
		{Redraw}
		{SetLineNumbers 1/0/toggle}
		{ScrollWindow up/down num_lines/page}
		{Zoom}
		{SaveAs}
		{SaveFile}
		{SaveAllFiles}
		{***BROWSER_WINDOWS***}
		{Browser geometry}
		{BrowserFont fontName}
		{CD directory}
		{CloseBrowser}
		{***ETC***}
		{DoNothing}
		{CancelModes}
		{QuitPoint save/ask/nosave}
		{PrintStats}
		{Option set name value}
		{Option get name}
		{IndentSelection outdent(1/0)}
		{Filter cmd}
		{MakeSaveOptionsBox}
		{InsertFile}
		{SearchForSel regex(1/0) up/down}
		{GetSelectedKeyword}
		{HelpWindow keyword}
		{***TCL***}
		{DefineMacro}
		{ExecMacro}
		{ExecSel}
	}
	frame .kbb.cmd_list -relief flat
	scrollbar .kbb.cmd_list.scrollbar -relief sunken \
		-command ".kbb.cmd_list.list yview"
	listbox .kbb.cmd_list.list \
		-yscrollcommand ".kbb.cmd_list.scrollbar set"  \
		-relief sunken -geometry 40x10
	tk_listboxSingleSelect .kbb.cmd_list.list
	bind .kbb.cmd_list.list <1> {
		set sel [.kbb.cmd_list.list nearest %y]
		.kbb.cmd_list.list select from $sel
		catch {.kbb.cmd_entry delete 0 end}
		set sel [.kbb.cmd_list.list get $sel]
		.kbb.cmd_entry insert 0 [lindex $sel 0]
	}
	bind .kbb.cmd_list.list <Any-Key> {
		set sel [ScanForLetter %A]
		.kbb.cmd_list.list select from $sel
		.kbb.cmd_list.list yview $sel
	}
	pack append .kbb.cmd_list \
		.kbb.cmd_list.scrollbar {left fill} \
		.kbb.cmd_list.list {right fill expand}
	foreach cmd $cmd_list {
		.kbb.cmd_list.list insert end $cmd
	}
	entry .kbb.cmd_entry -relief sunken -exportselection no
	label .kbb.cmd_args_label -text "Fixed args to command:"
	entry .kbb.cmd_args -relief sunken -exportselection no
	#
	# make a list of keys
	#
	set key_list {
		F1 F2 F3 F4 F5 F6 F7 F8 F9 F10 F11 F12
		Insert Delete Home End Prior Next
		Up Down Left Right
		Tab Escape Linefeed Break
		KP_Enter KP_Decimal KP_Add KP_Subtract KP_Multiply
		KP_Divide KP_0 KP_1 KP_2 KP_3 KP_4 KP_5 KP_6 KP_7
		KP_8 KP_9
		a b c d e f g h i j k l m n o p q r s t u v w x y z
		1 2 3 4 5 6 7 8 9 0
	}
	frame .kbb.key_list -relief flat
	scrollbar .kbb.key_list.scrollbar -relief sunken \
		-command ".kbb.key_list.list yview"
	listbox .kbb.key_list.list \
		-yscrollcommand ".kbb.key_list.scrollbar set"  \
		-relief sunken -geometry 40x5
	tk_listboxSingleSelect .kbb.key_list.list
	bind .kbb.key_list.list <1> {
		set sel [.kbb.key_list.list nearest %y]
		.kbb.key_list.list select from $sel
		catch {.kbb.key_entry delete 0 end}
		.kbb.key_entry insert 0 [selection get]
	}
	pack append .kbb.key_list \
		.kbb.key_list.scrollbar {left fill} \
		.kbb.key_list.list {right fill expand}
	foreach key $key_list {
		.kbb.key_list.list insert end $key
	}
	entry .kbb.key_entry -relief sunken -exportselection no
	#
	# commands
	#
	frame .kbb.buttons -relief raised
	button .kbb.buttons.remap -text "Remap Key" -command {
		foreach w [GetTextWindowList] {
			bind $w.vScrollAndText.text <[.kbb.key_entry get]> \
				"[.kbb.cmd_entry get] [.kbb.cmd_args get]"
		}
	}
	pack append .kbb.buttons \
		.kbb.buttons.remap {left fill expand}
	#
	# a close button
	#
	button .kbb.close -text "Close" -command "destroy .kbb"
	pack append .kbb \
		.kbb.title {top fill} \
		.kbb.cmd_list {top fill expand} \
		.kbb.cmd_entry {top fill} \
		.kbb.cmd_args_label {top fill} \
		.kbb.cmd_args {top fill} \
		.kbb.key_list {top fill expand} \
		.kbb.key_entry {top fill} \
		.kbb.buttons {top fill} \
		.kbb.close {bottom fill}
}
proc ScanForLetter {letter} {
	global cmd_list
	set len [llength $cmd_list]
	for {set n 0} {$n<$len} {incr n} {
		set item [string index [lindex $cmd_list $n] 0]
		if ![string compare $letter [string tolower $item]] {
			return $n
		}
	}
	return 0
}
#
# Finish this later -- getting the command descriptions directly
# from the latex documentation
#
proc CreateCmdInfo {{file ../doc/cmds.tex}} {
	global CmdName CmdInfo
	set CmdName ""
	set CmdInfo ""
	set fid [open $file r]
	set gettingPara 0
	while {![eof $fid]} {
		set line [gets $fid]
		if {[string index $line 0]=="\\" && $gettingPara} {
			set gettingPara 0
			lappend CmdInfo $para
		}
		if $gettingPara {
			set para [format "%s %s" $para $line]
		}
		if [regexp "\\subsubsection{(.*)}" $line junk cmd] {
			set gettingPara 1
			lappend CmdName [lindex $cmd 0]
			set para [format "%s\n" $cmd]
		}
	}
	if $gettingPara {
		lappend CmdInfo $para
	}
}
#CreateCmdInfo
#foreach x $CmdInfo {
#	puts stdout "<$x>"
#}
