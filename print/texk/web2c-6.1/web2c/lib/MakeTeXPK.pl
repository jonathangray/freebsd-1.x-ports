#! /usr/local/bin/perl
#
# From: Martin Ward <Martin.Ward@durham.ac.uk>
# Date: Fri, 28 Jan 94 15:40:14 GMT
# 
# Here's my MakeTeXPK. Currently it puts all the .pk files in 
# $prefix/texmf/fonts/pk/dpiNNN/name.pk  and the .tfm files in
# $prefix/texmf/fonts/tfm
# With the automatic generation of pk's and tfm's (and a 40 MIP sparcstation 2
# to run metafont on!) I can get by quite happily with a minimal set of fonts.
# 
#   This script file makes a new TeX PK font, because one wasn't
#   found.  Parameters are:
#
#   name dpi bdpi magnification [mode]
#
#   `name' is the name of the font, such as `cmr10'.  `dpi' is
#   the resolution the font is needed at.  `bdpi' is the base
#   resolution, useful for figuring out the mode to make the font
#   in.  `magnification' is a string to pass to MF as the
#   magnification.  `mode', if supplied, is the mode to use.
#
#   Note that this file must execute Metafont, and then gftopk,
#   and place the result in the correct location for the PostScript
#   driver to find it subsequently.  If this doesn't work, it will
#   be evident because MF will be invoked over and over again.
#
#   Modified to generate dc fonts automatically. To generate dcXXXnn:
#   (1) Create dcXXXnn.mf in current dir, containing the lines:
#         if unknown dxbase: input dxbase fi;
#         gensize:=nnn;
#         generate dcXXX
#   (2) Use dxbase base file

print STDERR "MakeTeXPK @ARGV\n";

# Old defaults:
#$base = "/home/ws-csm3/user15/gnu/lib/tex/fonts";
#$pkbase = "/home/ws-csm3/user15/gnu/lib/tex/fonts/pk.RicohA";  # Default pk dir#$stdmode = "RicohA";                   # Default mode_def (write-white):
#$pkdefault = "/home/ws-csm3/user15/gnu/lib/tex/fonts/RicohA";

# Default paths:
$prefix = "/local/home/user15/gnu/lib";
$base = "$prefix/texmf/fonts";
$pkbase = "$prefix/texmf/fonts/pk";     # Default pk dir
$stdmode = "CanonCX";                   # Default mode_def (write-black):
$pkdefault = "$prefix/texmf/fonts/canon300";
$TEMPDIR = "/tmp";
$name = $ARGV[0];
$dpi = $ARGV[1];
$bdpi = $ARGV[2];
$mag = $ARGV[3];
$mode = $ARGV[4];

$FINALname = "$pkbase/dpi$dpi/$name.pk";
# cd to TEMPDIR if current dir is not writable:
chdir($TEMPDIR) if (! -w ".");
if (-f "$pkdefault/dpi$dpi/$name.pk") { # check if file exists
  die "dpi$dpi/$name.pk already exists in $pkdefault\n";
}
if (-f "$pkbase/dpi$dpi/$name.pk") {    # check if file exists
  die "dpi$dpi/$name.pk already exists in $pkbase\n";
}
if (-f $FINALname) {
  die "$FINALname already exists!\n";
}
if (! -w $pkbase) {
  mkdir($pkbase, 755);
  chmod 0755, $pkbase;
  if (! -w $pkbase) {
    die "Can't write to $pkbase\n";
  }
}
if ($mode eq "") {
  if ($bdpi = 300) {
    $mode = $stdmode;
  } elsif ($bdpi = 118) {
    $mode = "sun";
  } else {
  die "I don't know the mode for $bdpi\n";
  }
}

&grokformat;

# redirect stdout to stderr
open(SAVEOUT, ">&STDOUT");
open(STDOUT, ">&STDERR");
print "Running Metafont...\n";
# the split gives an array which prevents perl from using the shell:
$line = "virmf $fmt \\mode:=$mode; \\mag:=$mag; input $name";
print "$line\n";
system split(/\s/, $line);
if (!-f "$name.${dpi}gf") {
  $dpi++;
  if (!-f "$name.${dpi}gf") { 
    $dpi--; $dpi--;
    if (!-f "$name.${dpi}gf") { 
      die "Metafont failed to create $name.${dpi}gf\n";
    }
  }
}
system "gftopk ./$name.${dpi}gf";
die "gftopk failed on $name.${dpi}gf\n" unless (-f "$name.${dpi}pk");
if (! -d "$pkbase/dpi$dpi") {   # make the directory:
  mkdir("$pkbase/dpi$dpi",755);
  chmod 0755, "$pkbase/dpi$dpi";
}
system "mv $name.${dpi}pk $pkbase/dpi$dpi/$name.pk";
chmod 0644, "$pkbase/dpi$dpi/$name.pk";
if (! $mfpic) {
  system "mv $name.tfm $base/tfm";
  chmod 0644, "$base/tfm/$name.tfm";
}
unlink "$name.${dpi}gf";
unlink "$name.mf" if ($name =~ /^dc/);

# tell dvipsk where the pk file is:
print SAVEOUT "$pkbase/dpi$dpi/$name.pk\n";


exit(0);


sub grokformat {
  # See what fmt (if any) to use:
  if ($plain) {         # -plain option
    $fmt = '&plain';
  } elsif ($name =~ /^cm/) {    # computer modern
    $fmt = '&cmbase';
  } elsif ($name =~ /^dc/) {    # DC fonts
    $fmt = '&dxbase';
    # Create the .mf file in current directory:
    $name =~ m/^([a-zA-Z]+)(\d+)$/ || die "Error in dc font name: $name\n";
    $basename = $1;
    $designsize = $2;
    open(MFFILE, ">$name.mf");
    print MFFILE <<END;
if unknown dxbase: input dxbase fi;
gensize:=$designsize;
generate $basename
END
    close (MFFILE);
  } elsif (($name =~ /circle/)  || # latex circle fonts
           ($name =~ /^line/)   || # latex line fonts
           ($name =~ /^chess/)  || # chess font
           ($name =~ /^knot/)   || # Celtic knotwork fonts
           ($name =~ /^bbold/)  || # blackboard bold font
           ($name =~ /^pn/)     || # Pandora fonts
           ($name =~ /^uic/)    || # modified Pandora fonts
           ($name =~ /^blackletter/) || # blackletter
           ($name =~ /^manfnt/) || # TeX and METAFONT manuals special font
           ($name =~ /^ark/)    || # hands and pencils
           ($name =~ /^dingbat/)|| # nice borders
           ($name =~ /^font29/)  || # large headings font
           ($name =~ /^beam/)   || # music font
           ($name =~ /^music/)  || # music font
           ($name =~ /^slur/))   { # music font
    $fmt = '&plain';
  } elsif ($name =~ /^logo/) {  # logo font
    $fmt = '&logo';
  } elsif ($name =~ /^eu/) {    # Euler fonts
    $fmt = '&plain';
  } else {                      # some other font - use cmbase
    $fmt = '&cmbase';
  }
  # mfpic hack! If the mf file is in current dir, and contains
  # the string "graphbase" then its an mfpic file, so use plain base:
  if (-r "$name.mf") {
    system "egrep -s 'graphbase' $name.mf";
    if ($? == 0) {
      $mfpic = 1;
      $fmt = '&plain';
      $pkbase = ".";
    }
  }
}
