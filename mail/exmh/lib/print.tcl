# Print
#

proc Print_Init {} {
    Preferences_Add "Printing" \
"Use these items to control how Exmh prints mail messages.  In the print command, the variable \$file is replaced by the name of the file(s) so you can embed it into a pipeline." {
	{print(cmd) printCmd {enscript $file} {Print command}
"This command is used to print the current message.
The variable $file is replaced by the name of the file(s)
so you can embed it into a pipeline."} \
    }
}
proc Print {} {
    global exmh print mhProfile
    set logvar {}
    set file {}
    Ftoc_Iterate line {
	set msgid [Ftoc_MsgNumber $line]
	lappend file $mhProfile(path)/$exmh(folder)/$msgid
    }
    set nprint [llength $file]
    if {$nprint == 0} {
	Exmh_Status "No messages selected for printing" red
	return
    }
    set plural s
    if {$nprint == 1} {
	set plural ""
    }
    Exmh_Status "Printing $nprint message$plural"

    # Because $print(cmd) embeds $file, extra levels of eval are required
    if {[catch {eval eval exec $print(cmd)} logvar]} {
	if [Exwin_Toplevel .printmsg "Print Messages"] {
	    Widget_Message .printmsg msg -aspect 1500 -relief raised 
	}
	.printmsg.msg configure -text \
"Messages generated when printing your message$plural:

$logvar
"

    }
}
proc PrintInner { msgid logvarName } {
    upvar $logvarName logvar

    global msg exmh print mhProfile
    Exmh_Status "[file tail [lindex $print(cmd) 0]] $exmh(folder)/$msgid"

    # Set $file for use by the print(cmd)
    set file $mhProfile(path)/$exmh(folder)/$msgid

    if [catch {eval exec $print(cmd)} err] {
	if [string match {couldn't find * to execute} $err] {
	    if [Exwin_Toplevel .printerror "Print error"] {
		Widget_Message .printerror msg -aspect 1500 -relief raised
	    }
	    .printerror.msg configure -text \
"An error occurred while trying to print your message:

$err

Try adjusting your print command via Preferences."
	    return 0
	} else {
	    append logvar "$exmh(folder)/$msgid\n" $err \n
	    return 1
	}
    } else {
	return 1
    }
}
