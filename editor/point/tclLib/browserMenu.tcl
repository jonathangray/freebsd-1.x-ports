#
#
# The browser menu bar specification
#
#
global PREFS
set PREFS {
	{check "Ignore case in searches" ignoreCase \
		{Option set ignoreCase $ignoreCase}}
	{check "Find whole words only in searches" findWholeWords \
		{Option set findWholeWords $findWholeWords}}
	{check "Typed characters replace the selection" insertReplaces \
		{Option set insertReplaces $insertReplaces}}
	{check "Show the sizes of files in browser list" showSizes \
		{Option set showSizes $showSizes;CD .}}
	{check "Use standard Tk style scroll bars" tkScrolling \
		{Option set tkScrolling $tkScrolling}}
	{check "Automatic indenting" autoIndent \
		{Option set autoIndent $autoIndent}}
	{check "Show full path names in window titles" pathNames \
		{Option set pathNames $pathNames;Redraw}}
	{cascade "Search Options" {
		{command "Other search options ..." {MakeSearchOptionsBox}}
		{check "Ignore case in searches" ignoreCase \
			{Option set ignoreCase $ignoreCase}}
		{check "Find whole words only in searches" findWholeWords \
			{Option set findWholeWords $findWholeWords}}
		{check "Wrap around to beginning in searches" \
			wrapAroundSearches \
			{Option set wrapAroundSearches $wrapAroundSearches}}
		{cascade "Placement of found strings" {
			{radio "Center found strings in the window" \
				linesOverFind 999 \
				{Option set linesOverFind $linesOverFind}}
			{radio "Place found strings at top of window" \
				linesOverFind 0 \
				{Option set linesOverFind $linesOverFind}}
			{radio "Place found strings 2 lines from top of window"\
				linesOverFind 2 \
				{Option set linesOverFind $linesOverFind}}
			{radio "Place found strings 5 lines from top of window"\
				linesOverFind 5 \
				{Option set linesOverFind $linesOverFind}}
		    {radio "Place found strings 10 lines from top of window" \
				linesOverFind 10 \
				{Option set linesOverFind $linesOverFind}}
		}}
	}}
	{cascade "Other boolean options" {
		{check "Undo motion as well as edits" undoMotion \
			{Option set undoMotion $undoMotion}}
		{check "Use mouse cursor to show mouse menu items" \
			mouseSpriteMenu \
			{Option set mouseSpriteMenu $mouseSpriteMenu}}
		{check "Left button scrolls down" button1ScrollsDown \
			{Option set button1ScrollsDown $button1ScrollsDown}}
		{separator}
		{check "Type over existing characters when inserting" overType \
			{Option set overType $overType}}
		{check "Show partial line at bottom of text windows" \
			showPartialLines \
			{Option set showPartialLines $showPartialLines;Redraw}}
		{separator}
		{check "Show directories before files in browser list" \
			showDirsFirst \
			{Option set showDirsFirst $showDirsFirst}}
		{separator}
		{check "Backup with a copy (not a move)" backupByCopy \
			{Option set backupByCopy $backupByCopy}}
		{check "Make new windows read only" readOnly \
			{Option set readOnly $readOnly}}
	}}
	{command "String valued options ..." {MakeOtherOptionsBox}}
	{cascade "Set text colors" {
	    {cascade "Normal foreground color" {
	        {command "Blue" {SetTextColor Blue}}
	        {command "Black" {SetTextColor Black}}
	        {command "White" {SetTextColor White}}
	        {command "Red" {SetTextColor Red}}
	        {command "Yellow" {SetTextColor Yellow}}
	        {command "Cyan" {SetTextColor Cyan}}
	        {command "Magenta" {SetTextColor Magenta}}
	    }}
	    {cascade "Normal background color" {
	        {command "Blue" {SetTextColor Blue normal background}}
	        {command "Black" {SetTextColor Black normal background}}
	        {command "White" {SetTextColor White normal background}}
	        {command "Red" {SetTextColor Red normal background}}
	        {command "Yellow" {SetTextColor Yellow normal background}}
	        {command "Cyan" {SetTextColor Cyan normal background}}
	        {command "Magenta" {SetTextColor Magenta normal background}}
	    }}
	    {cascade "Selected foreground color" {
	        {command "Blue" {SetTextColor Blue selected}}
	        {command "Black" {SetTextColor Black selected}}
	        {command "White" {SetTextColor White selected }}
	        {command "Red" {SetTextColor Red selected}}
	        {command "Yellow" {SetTextColor Yellow selected}}
	        {command "Cyan" {SetTextColor Cyan selected}}
	        {command "Magenta" {SetTextColor Magenta selected}}
	    }}
	    {cascade "Selected background color" {
	        {command "85% Grey" {SetTextColor grey85 selected background}}
	        {command "Blue" {SetTextColor Blue selected background}}
	        {command "Black" {SetTextColor Black selected background}}
	        {command "White" {SetTextColor White selected background}}
	        {command "Red" {SetTextColor Red selected background}}
	        {command "Yellow" {SetTextColor Yellow selected background}}
	        {command "Cyan" {SetTextColor Cyan selected background}}
	        {command "Magenta" {SetTextColor Magenta selected background}}
	    }}
	}}
	{cascade "Number of backups kept" {
	{radio "None" backupDepth 0 {Option set backupDepth $backupDepth}}
	{radio "1" backupDepth 1 {Option set backupDepth $backupDepth}}
	{radio "2" backupDepth 2 {Option set backupDepth $backupDepth}}
	{radio "3" backupDepth 3 {Option set backupDepth $backupDepth}}
	{radio "4" backupDepth 4 {Option set backupDepth $backupDepth}}
	{radio "5" backupDepth 5 {Option set backupDepth $backupDepth}}
	{radio "6" backupDepth 6 {Option set backupDepth $backupDepth}}
	}}
	{cascade "Width of a tab" {
	{radio "1" tabWidth 1 {Option set tabWidth $tabWidth}}
	{radio "2" tabWidth 2 {Option set tabWidth $tabWidth}}
	{radio "3" tabWidth 3 {Option set tabWidth $tabWidth}}
	{radio "4" tabWidth 4 {Option set tabWidth $tabWidth}}
	{radio "5" tabWidth 5 {Option set tabWidth $tabWidth}}
	{radio "6" tabWidth 6 {Option set tabWidth $tabWidth}}
	{radio "8" tabWidth 8 {Option set tabWidth $tabWidth}}
	{radio "10" tabWidth 10 {Option set tabWidth $tabWidth}}
	{radio "15" tabWidth 15 {Option set tabWidth $tabWidth}}
	}}
	{cascade "Right margin for automatic CRs" {
	{radio "None" rightMargin 999 {Option set rightMargin $rightMargin}}
	{radio "40" rightMargin 40 {Option set rightMargin $rightMargin}}
	{radio "50" rightMargin 50 {Option set rightMargin $rightMargin}}
	{radio "60" rightMargin 60 {Option set rightMargin $rightMargin}}
	{radio "70" rightMargin 70 {Option set rightMargin $rightMargin}}
	{radio "72" rightMargin 72 {Option set rightMargin $rightMargin}}
	{radio "74" rightMargin 74 {Option set rightMargin $rightMargin}}
	{radio "76" rightMargin 76 {Option set rightMargin $rightMargin}}
	{radio "78" rightMargin 78 {Option set rightMargin $rightMargin}}
	{radio "80" rightMargin 80 {Option set rightMargin $rightMargin}}
	{radio "90" rightMargin 90 {Option set rightMargin $rightMargin}}
	{radio "100" rightMargin 100 {Option set rightMargin $rightMargin}}
	}}
	{cascade "Selection style" {
	{radio "Show selection in the selected text colors" underlineSelection \
		0 {Option set underlineSelection $underlineSelection}}
	{radio "Underline the selection with one line" underlineSelection \
		1 {Option set underlineSelection $underlineSelection}}
	{radio "Underline the selection with two lines" underlineSelection \
		2 {Option set underlineSelection $underlineSelection}}
	}}
}

global SHELL
set SHELL {
	{command "Run SHELL in window" {RunProgramInWindow [UsersShell]}}
	{command "Run SHELL in file" {ConnectToPty [UsersShell]}}
	{command "Run and replace selection" {RunProgramInFile 1}}
	{command "Run selection in file" {RunProgramInFile 0}}
}

global MISC
set MISC {
###	{command "Latex the selection" {RunLatexOnSelection}}
	{command "Change key bindings ..." {MakeKeyBindingsBox}}
	{command "Save Point Options ..." {MakeSaveOptionsBox}}
	{command "Load scratch file" {OpenWindow scratch $location1 doNotAsk}}
	{command "About Point ..." {MakeAboutBox}}
	{command "Cancel copy mode" {CancelModes}}
	{command "Print Statistics" {PrintStats}}
	{separator}
	{command "Delete File" {exec rm -f [selection get]}}
	{command "Insert ASCII ..." {MakeAsciiBox}}
	{command "Save All Unsaved Files" {SaveAllFiles}}
	{command "Set debug ..." {MakeDebugBox}}
	{command "Information" \
			{puts stdout "Selection bounds: [Sel get]"}}
	{separator}
	{command "6x13" {BrowserFont 6x13}}
	{command "6x13bold" {BrowserFont 6x13bold}}
	{command "5x8" {BrowserFont 5x8}}
	{command "*clean-medium-r*6*50*" {BrowserFont *clean-medium-r*6*50*}}
	{command "8x13" {BrowserFont 8x13}}
}

global DIRS
set DIRS {
	{command "~"			{CD ~}}
	{command "~/bin"		{CD ~/bin}}
	{command "~/Mail"		{CD ~/Mail}}
	{command "~/News"		{CD ~/News}}
	{command "~/src"		{CD ~/src}}
	{separator}
	{command "/nfs/unmvax/src/X11R4/mit" {CD /nfs/unmvax/src/X11R4/mit}}
	{command "/usr/include"		{CD /usr/include}}
	{command "/usr/include/sys"	{CD /usr/include/sys}}
	{command "/usr/include/X11"	{CD /usr/include/X11}}
	{command "CD to selection"	{CD [selection get]}}
}

global BrowserMenuSpec
set BrowserMenuSpec {
	{menu browserMenu "MENU" BrowserMenu
"Various commands not related to a single window"}
	{menu dirs "DIRS" DIRS
"CD directly to a listed directory (or the selection as a directory name)"}
	{button new "New" {
		{global browser2;Browser $browser2}
		{global browser1;Browser $browser1}
		{global browser3;Browser $browser3}
"New browser:Left(right/this one)Middle(above this one)Right(far right)"
	}}
}

global BrowserMenu
set BrowserMenu {
	{cascade "User Preferences" PREFS}
	{cascade "Shell/Pty" SHELL}
	{command "New Window" {OpenFileOrCD 1}}
	{cascade "Dirs to CD" DIRS}
	{cascade "Misc Cmds"	MISC}
	{command "Refresh w/*" {Option set filePattern "*"}}
	{command "New Browser" {global browser2;Browser $browser2}}
	{command "Delete Selected Files" DeleteSelectedFiles}
	{command "Open Selected Files" OpenSelectedFiles}
	{command "Close This Browser" {CloseBrowser}}
	{cascade "Quit Point" QuitMenu}
}

proc browserMenuBindings {w} {
}

