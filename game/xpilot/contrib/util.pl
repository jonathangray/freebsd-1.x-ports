From bjoerns@stud2.cs.uit.no  Thu Sep 17 01:52:16 1992
Received: from hpserv0.cs.UiT.No by stud2.cs.uit.no (16.8/HJ/Task-5)
	id AA15066; Thu, 17 Sep 92 01:52:16 +0200
Received: from stud2.cs.UiT.No by hpserv0.cs.uit.no (16.8/Task/HJ-5)
	id AA06965; Thu, 17 Sep 92 01:52:16 +0200
Received: by stud2.cs.uit.no (16.8/HJ/Task-5)
	id AA15062; Thu, 17 Sep 92 01:52:16 +0200
Resent-Date: Thu, 17 Sep 1992 01:49:25 +0200
Resent-Message-Id: <9209162352.AA15062@stud2.cs.uit.no>
Received: from hpserv0.cs.UiT.No by stud2.cs.uit.no (16.8/HJ/Task-5)
	id AA14650; Thu, 17 Sep 92 01:49:29 +0200
Received: from ifi.uio.no by hpserv0.cs.uit.no (16.8/Task/HJ-5)
	id AA06747; Thu, 17 Sep 92 01:49:29 +0200
Received: from hnoss.ifi.uio.no by ifi.uio.no with SMTP 
	id <AAifi.uio.no27399>; Thu, 17 Sep 1992 01:49:28 +0200
Received: by hnoss.ifi.uio.no ; Thu, 17 Sep 1992 01:49:27 +0200
Message-Id: <199209162349.AAhnoss.ifi.uio.no08633@hnoss.ifi.uio.no>
Reply-To: xpilot-list@cs.uit.no
Resent-From: xpilot-request@cs.uit.no
From: stigkr@ifi.uio.no (Stig Rune Kristoffersen)
Date: Thu, 17 Sep 1992 01:49:25 +0200
X-Mailer: Mail User's Shell (7.2.4 2/2/92)
To: bjoerns@staff.cs.uit.no
Subject: A little perl script to run on your Xpilot log
Status: RO

I wrote this little script to get a bit info about what maps has been used,
and which local users that had started the server.
Maybe some of you out there find this "usefull" as well.

An example of the output from the script:

$ chklog -m
                Status Rapport for the usage of Xpilot
Times started: 52       Total of users: 6       Total of maps: 14

Name of Map                      Times used
CloudScape                         1
Experimental Death Sphere Map V    1
Fireball                           2
Kruz                               6
My first design, so go easy on     3
Pizza                              2
Planet X                           1
Random map                         1
The Cave                           3
The Continent                     23
The new and improved map           6
TubeWorld - ride the ultimate t    1
Whirlpools, War And Wormholes -    1
With a medium city you can find    1

---Cut here---
#!/local/bin/perl 
###############################################################################
# CHKLOG Ver 0.5
# Author: Stig R. Kristoffersen (stigkr@ifi.uio.no)
# Redistribute freely. 
#
# Usage : chklog [-f filename] [-u] [-m]
# -f  specify there to find the log file
# -u  to list who has started the server & how many times they have started it
# -m  to list which maps has been used & how many times that particular map
#     has been in use.
###############################################################################
require "getopts.pl";
&Getopts('f:um');
$FILE = "/hom/stigkr/lib/xpilot/log" if (!($FILE = $opt_f));

open(LOG, $FILE) || die "$!: $FILE\n";
while(<LOG>) {
    if (($user,$map) = /^START.+\- (\w+).+\'(.*)\'/) {
	$t_started++;
	$t_users++ if (!$users{$user}++);
        $t_maps++ if (!$maps{$map}++);
    }
}
print "\t\tStatus Rapport for the usage of Xpilot\n";
print "Times started: $t_started \tTotal of users: $t_users \tTotal of maps: $t_maps\n\n";
if($opt_u) {
    $~ = 'USR';
    print "Username                      Times started\n"; 
    foreach $user (sort keys %users) { write; }	
    print "\n\n";		
}
if($opt_m) {
    $~ = 'MAP';
    print "Name of Map                   Times used\n";
    foreach $map (sort keys %maps) { write; }
}

# Format used to make the output a bit nicer(?)
format USR =
@<<<<<<<<<<<<<<<<<<<<<<<<<<< @###
$user,$users{$user}

