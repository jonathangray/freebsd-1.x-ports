#
#
# The text menu bar specification
#
#
global QuitMenu
set QuitMenu {
	{command "And save all" {QuitPoint save}}
	{command "And ask" {MakeQuitBox}}
	{command "And discard edits" {QuitPoint discard}}
}

global HelpMenu
set HelpMenu {
	{command "General" {HelpWindow general}}
	{command "Scrolling" {HelpWindow scroll}}
	{command "FILE menu" {HelpWindow "filemenu"}}
	{command "EDIT menu" {HelpWindow "editmenu"}}
	{command "GOTO menu" {HelpWindow "gotomenu"}}
	{command "Function keys" {HelpWindow "function keys"}}
	{command "Mouse menus" {HelpWindow "mouse menus"}}
	{command "Options" {HelpWindow options}}
}

global FileMenu
set FileMenu {
	{command "Select text colors ..." {MakeColorBox}}
	{cascade "Select text font" {
		{command "6x13" {TextFont "6x13"}}
		{command "6x10" {TextFont "6x10"}}
		{command "5x8" {TextFont "5x8"}}
		{command "*clean-medium-r*6*50*"
				{TextFont "*clean-medium-r*6*50*"}}
		{command "7x13" {TextFont "7x13"}}
		{command "8x13" {TextFont "8x13"}}
		{command "9x15" {TextFont "9x15"}}
		{command "10x20" {TextFont "10x20"}}
		{command "6x13bold" {TextFont "6x13bold"}}
		{command "7x13bold" {TextFont "7x13bold"}}
		{command "8x13bold" {TextFont "8x13bold"}}
		{command "kana14" {TextFont "kana14"}}
		{command "Selected font name" {TextFont [selection get]}}
	}}
	{cascade "PREFS" PREFS}
	{separator}
	{command "Zoom vertical" {Zoom}}
	{command "Zoom full" {Zoom {} full}}
	{cascade "Move Window" {
		{command "Move to NW" {
			global location1
			MoveWindow $location1}
		}
		{command "Move to NE" {
			global location3
			MoveWindow $location3}
		}
		{command "Move to SE" {
			global location2
			MoveWindow $location2}
		}
		{command "Move to SW" {
			global location4
			MoveWindow $location4}
		}
	}}
	{separator}
	{command "Save file" {SaveFile}}
	{command "Save as ..." {SaveAs}}
	{separator}
	{command "Open selected file name" {OpenWindow [selection get]}}
	{command "Open New Browser ..." {
		global browser1
		Browser $browser1}
	}
	{cascade "Close window" {
		{command "and save" {CloseWindow save}}
		{command "and ask" {CloseWindow ask}}
		{command "and discard edits" {CloseWindow nosave}}
	}}
	{command "About Point ..." {MakeAboutBox}}
	{cascade "Quit Point" {
		{command "And save all" {QuitPoint save}}
		{command "And ask" {MakeQuitBox}}
		{command "And discard edits" {QuitPoint discard}}
	}}
}

global EditMenu
set EditMenu {
	{command "Insert X selection" {InsertSelectedString}}
	{command "Cancel copy mode" {CancelModes}}
	{separator}
	{command "Execute Selection as Tcl" {ExecSel}}
	{command "Define Selected Macro" {DefineMacro}}
	{command "Execute Macro" {ExecMacro}}
	{separator}
	{command "Search and Replace..." {MakeReplaceBox}}
	{command "RE Search and Replace..." {MakeRegexReplaceBox}}
	{separator}
	{command "Indent selected lines" {IndentSelection}}
	{command "Outdent selected lines" {IndentSelection 1}}
	{separator}
	{cascade "Delete" {
		{command "Delete to end of line" {DeleteLine}}
		{command "Delete entire line" {DeleteLine 1}}
		{command "Delete selection" {DeleteToScrap}}
	}}
	{cascade "Scrap" {
		{command "Insert from" {InsertFromScrap}}
		{command "Delete sel to" {DeleteToScrap}}
		{command "Copy sel to" {CopySelToScrap}}
		{command "Exchange sel with" {ExchangeWithScrap}}
	}}
	{cascade "Copy" {
		{command "Note destination" {CopyToHereMode}}
		{command "Sel to destination" {CopyToHereMode}}
		{command "Cancel copy mode" {CancelModes}}
	}}
	{cascade "Change Case" {
		{command "To upper" {ChangeCaseOfSel toupper}}
		{command "To lower" {ChangeCaseOfSel tolower}}
		{command "Toggle case" {ChangeCaseOfSel toggle}}
	}}
	{cascade "Undo/Again/Redo" {
		{command "Repeat last edit" {Again mostrecent}}
		{command "Last edit (this file)" {Again thisfile}}
		{command "Undo one edit" {Undo 1}}
		{command "Redo one edit" {Redo 1}}
		{command "Begin block undo" {Undo begin}}
		{command "End block undo" {Undo end}}
		{command "Show command history" {ShowUndoStack}}
	}}
	{command "Redraw window" {Redraw}}
}

global GotoMenu
set GotoMenu {
	{command "Goto Selected Line #" {GotoLine [selection get] lof}}
	{command "Goto Line # ..." {MakeGotoBox}}
	{command "Goto Selection" {ShowSelection}}
	{separator}
	{command "Find Selected Ctag" {CTag [selection get]}}
	{command "Find Ctag ..." {MakeCtagBox}}
	{command "Find Selected Keyword" {GetSelectedKeyword}}
	{command "Find Keyword ..." {MakeKeywordBox}}
	{command "Find Matching bracket" {FindMatchingBracket}}
	{separator}
	{cascade "Search for string" {
		{command ">> for selected RE" {SearchForSel 1 forward}}
		{command "<< for selected RE" {SearchForSel 1 backward}}
		{command ">> for last RE" {RegexSearch \{\} forward}}
		{command "<< for last RE" {RegexSearch \{\} backward}}
		{command ">> for last string" {RepeatSearch forward}}
		{command "<< for last string" {RepeatSearch backward}}
		{command ">> for selection" {SearchForSel 0 forward}}
		{command "<< for selection" {SearchForSel 0 backward}}
		{command "For string ..." {MakeSearchBox normal}}
	}}
	{cascade "Move in file to" {
		{command "Beginning" {GotoLine 1 top}}
		{command "End" {MoveToEndFile}}
		{command "Last place" {MoveToLastPlace}}
	}}
}

global TextMenuSpec
set TextMenuSpec {
	{menu   file  "FILE"   FileMenu
"Commands that affect files and windows"}
	{menu   edit  "EDIT"   EditMenu
"Commands that change the text"}
	{menu   goto  "FIND"   GotoMenu
"Commands that move around in the text"}
	{button << " << "   {
		{SearchForSel 0 backward} 
		{MakeSearchBox normal}
		{RepeatSearch backward}
"Search backwards Left(selection) Middle(dialog box) Right(last string)"
	}}
	{button >> " >> "   {
		{SearchForSel 0 forward} 
		{MakeSearchBox regex}
		{RepeatSearch forward}
"Search forewards Left(selection) Middle(dialog box) Right(last string)"
	}}
	{button close "Close"   {
		{CloseWindow save}
		{CloseWindow ask}
		{CloseWindow nosave}
"Close window Left(save files) Middle(ask re files) Right(do not save)"
	}}
	{button save " Sv "   {
		{SaveFile}
		{SaveAs}
		Redraw
"Left(save file) Middle(save as) Right(redraw screen)"
	}}
	{button jump "Jump"   {
		{GotoLine 1 top}
		MoveToLastPlace
		MoveToEndFile
"Left(begin file) Middle(last place jumped from) Right(end file)"
	}}
	{button dos "+Do-"    {
		{Redo 1}
		{Again}
		{Undo 1}
"Left(Redo undo cmd) Middle(repeat last cmd) Right(Undo last cmd)"
	}}
	{button zz "Zz"   {
		{Undo begin;ChangeCaseOfSel toupper;Undo end}
		{Undo begin;ChangeCaseOfSel toggle;Undo end}
		{Undo begin;ChangeCaseOfSel tolower;Undo end}
"Change case Left(to upper) Middle(toggle) Right(to lower)"
	}}
	{button line "Line#"   {
		{GotoLine [selection get] lof}
		MakeGotoBox
		{SetLineNumbers}
"Left(goto selected line #) Middle(goto linebox) Right(toggle line #s)"
	}}
	{menu   help  "HELP"   HelpMenu
"Help windows on various aspects of Point"}
}

#	{button insDel "insDel"   {
#		{InsertFromScrap} 
#		{InsertSelectedString}
#		{DeleteToScrap}
#	}}
#
#
# The text menu bindings
#
#
proc TextMenuBindings {w} {
	bind $w.<<     <Any-Enter> {+SearchCharacter 0 0}
	bind $w.<<     <Any-Key>   {+SearchCharacter %N %s}
	bind $w.>>     <Any-Enter> {+SearchCharacter 0 0}
	bind $w.>>     <Any-Key>   {+SearchCharacter %N %s}
	bind $w.goto   <Key>       {+GotoDigit %A}
	bind $w.line   <Key>       {+GotoDigit %A}
}

proc TextBindings {w name} {
	bind $w <Expose>		"Expose $name %x %y %w %h %c"
	bind $w <Configure>		"Configure $name"
	bind $w <Enter>			"focus $w"
	bind $w <Leave>			"focus none"
	bind $w <ButtonPress-1>		"Mouse $name BeginSelection %x %y 0"
	bind $w <Double-ButtonPress-1>	"Mouse $name BeginSelection %x %y 1"
	bind $w <Triple-ButtonPress-1>	"Mouse $name BeginSelection %x %y 2"
	bind $w <B1-Motion>		"Mouse $name ExtendSelection %x %y"
	bind $w <ButtonRelease-1>	"Mouse $name EndExtending %x %y"

	bind $w <ButtonPress-2>		"Mouse $name BeginMouseMenu2 %x %y"
	bind $w <B2-Motion>		"Mouse $name ContinueMouseMenu %x %y"
	bind $w <ButtonRelease-2>	"Mouse $name EndMouseMenu %x %y"
	bind $w <B2-ButtonRelease-1>	"Mouse $name CancelMouseMenu %x %y"
	bind $w <B2-ButtonRelease-3>	"Mouse $name CancelMouseMenu %x %y"

	bind $w <ButtonPress-3>		"Mouse $name BeginMouseMenu1 %x %y"
	bind $w <B3-Motion>		"Mouse $name ContinueMouseMenu %x %y"
	bind $w <ButtonRelease-3>	"Mouse $name EndMouseMenu %x %y"
	bind $w <B3-ButtonRelease-1>	"Mouse $name CancelMouseMenu %x %y"
	bind $w <B3-ButtonRelease-2>	"Mouse $name CancelMouseMenu %x %y"

	bind $w <Mod1-c>	{LatexSelection}

	bind $w <F1>		{DeleteToScrap}
	bind $w <F2>		{InsertFromScrap}
	bind $w <F3>		{RepeatSearch backward}
	bind $w <F4>		{RepeatSearch forward}
	bind $w <F5>		{Again}
	bind $w <F6>		{Redo 1}
	bind $w <F7>		{ScrollWindow up page}
	bind $w <F8>		{ScrollWindow down page}
	bind $w <F9>		{Undo 1}
	bind $w <F10>		{OpenFileOrCD 1}
	
	bind $w <Shift-Delete>	{DeleteToScrap}
	bind $w <Control-Delete> {CopySelToScrap}
	bind $w <Shift-Insert>	{InsertFromScrap}
	
	bind $w <Up>		{MoveSel char up}
	bind $w <Down>		{MoveSel char down}
	bind $w <Control-Left>	{MoveSel word left}
	bind $w <Mod1-Left>	{MoveSel word left}
	bind $w <Left>		{MoveSel char left}
	bind $w <Control-Right>	{MoveSel word right}
	bind $w <Mod1-Right>	{MoveSel word right}
	bind $w <Right>		{MoveSel char right}

	bind $w <Home>		{MoveSel line left}
	bind $w <End>		{MoveSel line right}
	bind $w <Prior>		{ScrollWindow up page}
	bind $w <Next>		{ScrollWindow down page}
	bind $w <Any-Key>       {Key %N %s}

	# bind control-s to incremental search
	bind $w <Control-s> {
		MessageLine "Search for: "
		# clear search string
		SearchCharacter 0 0
		# set the focus to the hidden incremental search frame
		global oldFocus
		set oldFocus [focus]
		focus .incrementalsearch
	}
}

