proc MakeSaveOptionsBox {} {
	catch {destroy .saveOptionsBox}
	toplevel .saveOptionsBox -relief raised
	wm title .saveOptionsBox "Save Options to File"
	wm iconname .saveOptionsBox "Save Options"
	label .saveOptionsBox.label -text "File to save the options to:"
	entry .saveOptionsBox.entry -relief sunken -exportselection no
	.saveOptionsBox.entry insert 0 "options.tcl"
	bind .saveOptionsBox.entry <Return> {
		SaveOptions [.saveOptionsBox.entry get]
	}
	button .saveOptionsBox.enter -text "Save current options" \
		-command { SaveOptions [.saveOptionsBox.entry get] }
	button .saveOptionsBox.close -text "Close" \
					-command "destroy .saveOptionsBox"
	pack append .saveOptionsBox \
		.saveOptionsBox.label {top fill} \
		.saveOptionsBox.entry {top fill} \
		.saveOptionsBox.enter {top fill} \
		.saveOptionsBox.close {top fill}
}

proc SaveOneOption {name {fid stdout}} {
	set value [Option get $name]
	puts $fid [format "Option set %-25s {%s}" $name $value]
}

proc SaveOptions {filename} {
	set fid [open $filename w]
	puts $fid "\
#
# This file was automatically generated.  It contains the values of the Point
# options that were saved on ***** [exec date] *****.
# \"source\" this file at Point startup in order to set the options to
# these values.  Note that 1 is used for \"True\" and 0 for \"False\".
# The {brackets} around the values are necessary to avoid tcl interpreting
# and \$'s, \['s or spaces in the values.
#"
	puts $fid "\
#
# Browser options
#"
	SaveOneOption browserFont $fid
	SaveOneOption browserGeometry $fid
	SaveOneOption browserIconFormat $fid
	SaveOneOption browserTitleFormat $fid
	SaveOneOption filePattern $fid
	SaveOneOption showDirsFirst $fid
	SaveOneOption showSizes $fid
	puts $fid "\
#
# Editing interaction options
#"
	SaveOneOption autoIndent $fid
	SaveOneOption busySpriteName $fid
	SaveOneOption button1ScrollsDown $fid
	SaveOneOption copySpriteName $fid
	SaveOneOption insertReplaces $fid
	SaveOneOption messageFlags $fid
	SaveOneOption overType $fid
	SaveOneOption readOnly $fid
	SaveOneOption tkScrolling $fid
	SaveOneOption undoMotion $fid
	puts $fid "\
#
# Text appearance options
#"
	SaveOneOption eofChar $fid
	SaveOneOption rightMargin $fid
	SaveOneOption deSelectedTextBackground $fid
	SaveOneOption deSelectedTextForeground $fid
	SaveOneOption selectedTextBackground $fid
	SaveOneOption selectedTextForeground $fid
	SaveOneOption showPartialLines $fid
	SaveOneOption spriteBackground $fid
	SaveOneOption spriteForeground $fid
	SaveOneOption spriteName $fid
	SaveOneOption tabWidth $fid
	SaveOneOption textBackground $fid
	SaveOneOption textFont $fid
	SaveOneOption textForeground $fid
	SaveOneOption underlineSelection $fid
	puts $fid "\
#
# Text Window options
#"
	SaveOneOption autoZoom $fid
	SaveOneOption pathNames $fid
	SaveOneOption textGeometry $fid
	SaveOneOption textIconFormat $fid
	SaveOneOption textTitleFormat $fid
	puts $fid "\
#
# Backup options
#"
	SaveOneOption backupByCopy $fid
	SaveOneOption backupDepth $fid
	SaveOneOption backupNameFormat $fid
	puts $fid "\
#
# Search options
#"
	SaveOneOption findWholeWords $fid
	SaveOneOption ignoreCase $fid
	SaveOneOption keywordPattern $fid
	SaveOneOption linesOverFind $fid
	SaveOneOption wrapAroundSearches $fid
	puts $fid "\
#
# Menu options
#"
	SaveOneOption menuDelay $fid
	SaveOneOption menuTolerance $fid
	SaveOneOption mouseMenuFont $fid
	SaveOneOption mouseSpriteMenu $fid
	puts $fid "\
#
# Popup mouse menu options
#"
	SaveOneOption lmm1 $fid
	SaveOneOption cmm1 $fid
	SaveOneOption lmm1n $fid
	SaveOneOption cmm1n $fid
	SaveOneOption lmm1e $fid
	SaveOneOption cmm1e $fid
	SaveOneOption lmm1s $fid
	SaveOneOption cmm1s $fid
	SaveOneOption lmm1w $fid
	SaveOneOption cmm1w $fid
	SaveOneOption lmm2 $fid
	SaveOneOption cmm2 $fid
	SaveOneOption lmm2n $fid
	SaveOneOption cmm2n $fid
	SaveOneOption lmm2e $fid
	SaveOneOption cmm2e $fid
	SaveOneOption lmm2s $fid
	SaveOneOption cmm2s $fid
	SaveOneOption lmm2w $fid
	SaveOneOption cmm2w $fid
	puts $fid "\
#
# Miscellaneous options
#"
	SaveOneOption hypertextOn $fid
	SaveOneOption maxFiles $fid
	SaveOneOption nBuffers $fid
	close $fid
}
