Return-Path: ars.rtp.nc.us!ars.rtp.nc.us!burdick
Received: from alpha.xerox.com ([13.1.64.93]) by palain.parc.xerox.com with SMTP id <269>; Fri, 21 Jan 1994 12:18:46 -0800
Received: from reggae.concert.net ([128.109.131.3]) by alpha.xerox.com with SMTP id <14914(8)>; Fri, 21 Jan 1994 12:18:34 PST
Received: from salzo.UUCP by reggae.concert.net (5.65/tas-reggae/nov93)
	id AA25804; Fri, 21 Jan 94 15:18:13 -0500
Received: by salzo.Cary.NC.US (1.65/waf)
	via UUCP; Fri, 21 Jan 94 15:05:28 EST
	for welch@parc.xerox.com
Received: from localhost by rtp.nc.us (4.1/SMI-4.1)
	id AA00643; Fri, 21 Jan 94 14:49:04 EST
Message-Id: <9401211949.AA00643@rtp.nc.us>
Reply-To: burdick@ars.rtp.nc.us
To:	Brent Welch <welch@parc.xerox.com>
Subject: revision to classifyMail [included]
X-Mailer: exmh version 1.2delta 1/6/94
Date:	Fri, 21 Jan 1994 11:49:03 -0800
From:	Bill Burdick <burdick@ars.rtp.nc.us>

Of course, there was a bug in the classifyMail program, which I found when I 
didn't receive the last 4 mail messages I got off the net.  I think I won't 
distribute this until I've tested it a bit more...  Anyway, here's the new 
prog.  I took out the mhlib variable and just set env(PATH) instead.  The other 
execs were failing, because mail from rmail is sent in a (I think) bare 
environment (no PATH variable).

	-- Bill Burdick
	burdick@ars.rtp.nc.us

P.S. Maybe you can look this over -- it's very simple...

#!/usr/local/bin/tcl -f
# Copyright 1994 Applied Reasoning Systems Corporation
# written by Bill Burdick (burdick@ars.rtp.nc.us)

##############
#### INFO ####
##############

# classifyMail program
#
# automatic mail delivery, as per <MH_LIB>/slocal
#
# differences between this and slocal:
#
# * component pattern is a regexp
# * order of fields in maildelivery file is different
# * written to be easily extended
# * actions can be arbitrary TCL code
# * determines who's using it (doesn't use the -user option)
# * error recovery
#
# my .forward entry:
#
# |/usr/local/bin/classifyMail
#
# format of ~/.maildelivery.tcl file:
#
# example:
#
# set w {[^a-zA-Z]}
# ? From:	ftpmail|archie			{+ ftp}
# ? To:		uucp|Postmaster|root|usenet	{+ admin}
# ? To:		${w}urs${w}|self-interest	{+ self}
# ? Cc:		self-interest			{+ self}
# ? To:		burdick				{+ bill}
# R *		-				{> /tmp/mail.tcl}
# default					{+ inbox}
#
# The deal:
#
# ~/.maildelivery.tcl is just a TCL source file.  It is sourced by the
# classifyMail file.  This is sort of roundabout, but I thought it
# would be nice to keep them separate, so that people didn't have to
# know TCL to use this.
#
# The conditions and actions are TCL functions.  Most conditions (?,
# N, A, R) take three arguments: a component, a pattern, and an action
# to perform if the component line matches the pattern (or if the
# component is "*"; then the pattern is ignored, but it must be
# present).  The actions are the traditional (+, >, file, |, pipe, ^,
# qpipe, destroy).  Actually, you can just use exit in place of
# destroy.  I have taken care to evaluate the actions at the level
# they are called from (see the conditions and the act? proc for the
# uplevel calls), so there shouldn't be any problem with using
# variables in the actions.
# 
# error recovery:
#
# if there are problems delivering mail (i.e. sourcing ~/.maildelivery.tcl
# causes a TCL error or the mail remains undelivered), classifyMail
# places an X-ERROR: component in the message and attempts to place it
# in the user's inbox (in the sequence 'error').  Failing that, it is
# appended to the ~/mail.errors file in the packf format.  So you should
# be able to view erroneous messages with 'show +inbox error'


#######################################
#### CONSTANTS -- configure these! ####
#######################################

set env(PATH) /bin:/usr/ucb:/usr/local/lib/mh


########################
#### INITIALIZATION ####
########################

# The problem here is that when classifyMail is called from sendmail
# (because it's in the .forward), the environment isn't set up
# properly.  In particular, $HOME is wrong (it's the home directory
# of whichever uid owns the sendmail process, which usually ends up
# being "/".  So, we have to set the env(HOME) variable to the proper
# value.

# get the user's home directory so that globbing and mh commands work
set home [glob ~[exec whoami]]
set env(HOME) $home


#################
#### GLOBALS ####
#################

set delivered 0
set success 1
# stamp is intended to be a header component put in by this package
# like "X-Received-By: classifyMail\n" or something
set stamp ""
# the mail message
set file {}
# the out of band sender information
set sender {}


####################
#### PROCEDURES ####
####################

# perform the action iff component matches pattern
proc act? {comp pat action} {
    global success msg

    if {$comp != "*" && (![info exists msg($comp)] || ![regexp $pat 
$msg($comp)])} {
	return 0
    }
    return [set success [expr ![catch {uplevel 1 $action}]]]
}

##################
### conditions ###
##################

proc default {action} {
    global delivered success

    if {!$delivered} {
	if {[set success [expr ![catch {uplevel 1 $action}]]]} {
	    set delivered 1
	}
    }
}

proc ? args {
    global delivered success

    if {$delivered} return
    if {[uplevel 1 act? $args]} {set delivered 1}
}

proc N args {
    global delivered success

    if {$delivered || !$success } return
    if {[uplevel 1 act? $args]} {set delivered 1}
}

proc A args {
    global delivered success

    if {[uplevel 1 act? $args]} {set delivered 1}
}

proc R args {
    uplevel 1 act? $args
}

###############
### actions ###
###############

proc + {folder} {
    global file

    eval exec rcvstore -create +$folder << {$file}
}

proc > {output} {
    global file

    set f [open $output a+]
    puts $f "\001\001\001\001"
    puts -nonewline $f "Delivery-Date: [exec date {+%a, %d %h %y %T }]"
    puts $f [format "%+.2d00" [expr {[exec date +%H] - [exec date -u +%H]}]]
    puts -nonewline $f $file
    puts $f "\001\001\001\001"
    close $f
}

proc file {output} {
    global file

    set f [open $output a+]
    puts $f "\001\001\001\001"
    puts -nonewline $f "Delivery-Date: [exec date {+%a, %d %h %y %T }]"
    puts $f [format "%+.2d00" [expr {[exec date +%H] - [exec date -u +%H]}]]
    puts -nonewline $f $file
    puts $f "\001\001\001\001"
    close $f
}

proc | args {
    global file

    exec sh -c "$args" << $file
}

proc pipe args {
    global file

    exec sh -c "$args" << $file
}

proc ^ args {
    global file

    exec $args << $file
}

proc qpipe args {
    global file

    exec $args << $file
}

proc destroy {} {
    exit
}


######################
### error recovery ###
######################

proc die {msg} {
    global file
    
    set file "X-ERROR: $msg\n$file"
    if {[catch {exec rcvstore +inbox -sequence error << $file}]} {
	> ~/mail.errors
    }
    exit
}

################################
### process the mail message ###
################################

# read it and parse the header

gets stdin sender
set file "Return-Path: [lindex $sender 1]\n"
while 1 {
    if {[gets stdin line] == -1} {
	break
    }
    if {$line == {}} {
# after the header is read, put the stamp in
	append file $stamp
	append file "$line\n"
	append file [read stdin]
	break
    }
    append file "$line\n"
    if {[string index $line 0] == "\t"} {
	append msg($component) "\n$line"
    } else {
	set component [lindex [split $line] 0]
	set msg($component) [string range $line [string length $component] end]
    }
}

# deliver the message according to the user's specs and recover from errors

if {[catch {source ~/.maildelivery.tcl}]} {
    die {problems executing the .maildelivery.tcl file}
}

# if the message remains undelivered, die
if {!$delivered} {
    die {this message remained undelivered}
}

