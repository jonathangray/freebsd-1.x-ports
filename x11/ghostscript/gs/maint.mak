#    Copyright (C) 1991, 1992, 1993 Aladdin Enterprises.  All rights reserved.
#
# This file is part of Ghostscript.
#
# Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the Ghostscript General Public License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# Ghostscript, but only under the conditions described in the Ghostscript
# General Public License.  A copy of this license is supposed to have been
# given to you along with Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# Auxiliary MS-DOS makefile for maintenance operations.

# This file pertains to Aladdin Enterprises maintenance operations,
# and is unlikely to be useful to users.

# Targets:
#	xmit -- make the .BE files for Kermit transmission
#	xfonts -- make the .BE files for the fonts
#	xfer -- make a diskette for transferring files to the Sun
#	release -- make the .BE files for a release
# Remember to erase *.zip / *.z / *.be, if needed, before running these.

# ---------------- Convert Windows icons ----------------

gsgraph.icx: gsgraph.uue
	eadcode / gsgraph.uue gsgraph.ico
	od -o gsgraph.icx -H gsgraph.ico

gstext.icx: gstext.uue
	eadcode / gstext.uue gstext.ico
	od -o gstext.icx -H gstext.ico

# ---------------- Make files for Kermit transmission ----------------

TARS=tar1.be tar2.be

FILES1=*.1 gs*.bat bdftops.bat font2c.bat ps2*.bat *.doc
FILES2=*.ps fontmap.* copying news readme bdftops font2c ps2*.
RM_FILES=c_*.* q* q*.* t.* comp1.*
SRC10=\rm.bat \cp.bat \mv.bat
SRC11=g*.asm i*.asm ansi2knr.c echogs.c ega.c turboc.cfg gs*.def
SRC12=*.icx gs*.rc *.h *.mak *.sh *.tr tar_*. ugly*.* gs*. ccgs
RM_SRC11=arch.h c_*.* gconfig*.h n*.h n*.mak l*.tr o*.tr
RM_SRC12=ugly*.bdf q* q*.* t.* comp1.* _temp*.*
SRC2=g*.c i*.c s*.c z*.c

xmit: $(TARS)

xexe: tar0.be

xfonts: tar8.be

xfer: tar1.z tar2.z
	@echo ---------------- Insert diskette:
	@command /c pause
	xcopy tar_x.* a:
	xcopy tar*.z a:

release: xmit xexe

# The dependency lists for the .be files should be much longer!

.z.be:
	bed $*.z $*.be
	erase $*.z
.taz.be:
	bed $*.taz $*.be
	erase $*.taz

gs-tests.taz:
	cd \gs\test
	tar -b1 -cf \gs\_temp_.tar *.ps
	cd \gs
	gzip _temp_.tar
	if exist gs-tests.taz erase gs-tests.taz
	rename _temp_.taz gs-tests.taz

# We don't make tar0.z a dependent of gs.exe, because that forces
# rebuilding of gs.exe if we've switched configurations recently.
tar0.z:
	@if not exist gs.exe echo gs.exe does not exist, do you want to proceed?
	@if not exist gs.exe pause
	@if not exist gs386.exe echo gs386.exe does not exist, do you want to proceed?
	@if not exist gs386.exe pause
	@if not exist gswin.exe echo gswin.exe does not exist, do you want to proceed?
	@if not exist gswin.exe pause
	tar -b1 -cf _temp_.tar -uexe -uico -ures gs*.exe gs*.ico gs*.res
	gzip _temp_.tar
	if exist tar0.z erase tar0.z
	rename _temp_.taz tar0.z

STAGING=\gs\master\staging

tar1.z:
	erase $(STAGING)\*.* <\y
	for %f in ($(FILES1)) do xcopy %f $(STAGING)
	for %f in ($(FILES2)) do xcopy %f $(STAGING)
	for %f in ($(SRC10)) do xcopy %f $(STAGING)
	for %f in ($(SRC11)) do xcopy %f $(STAGING)
	for %f in ($(SRC12)) do xcopy %f $(STAGING)
	cd $(STAGING)
	rm $(RM_FILES)
	xcopy \gs\quit.ps
	rm $(RM_SRC11)
	rm $(RM_SRC12)
	copy fontmap.gs fontmap
	tar -b1 -cf \gs\_temp_.tar -ubat -udef *.*
	erase $(STAGING)\*.* <\y
	cd \gs
	gzip _temp_.tar
	if exist tar1.z erase tar1.z
	rename _temp_.taz tar1.z

tar2.z: gs.c
	tar -b1 -cf _temp_.tar g*.c i*.c s*.c z*.c
	gzip _temp_.tar
	if exist tar2.z erase tar2.z
	rename _temp_.taz tar2.z

tar8.z: fonts\bchr.gsf fonts\hrsy_r.gsf
	tar -b1 -cf _temp_.tar fonts/*.gsf
	gzip _temp_.tar
	if exist tar8.z erase tar8.z
	rename _temp_.taz tar8.z

# ---------------- Make MS-DOS diskette sets ----------------

allzips: gsexe.zip gsfiles.zip gssrc1.zip gssrc2.zip \
  gsfonts1.zip gsfonts2.zip gsfonts3.zip gsfonts4.zip
	@echo ---------------- Done. ----------------

srczips: gsfiles.zip gssrc1.zip gssrc2.zip
	@echo ---------------- Done. ----------------

zips: gsexe.zip gsfiles.zip gssrc1.zip gssrc2.zip
	@echo ---------------- Done. ----------------

# Here are the ZIP files that go onto the diskettes.

# We don't make gsexe.zip a dependent of gs.exe, because that forces
# rebuilding of gs.exe if we've switched configurations recently.
gsexe.zip:
	@if not exist gs.exe echo gs.exe does not exist, do you want to proceed?
	@if not exist gs.exe pause
	@if not exist gs386.exe echo gs386.exe does not exist, do you want to proceed?
	@if not exist gs386.exe pause
	@if not exist gswin.exe echo gswin.exe does not exist, do you want to proceed?
	@if not exist gswin.exe pause
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip gs*.exe
	if exist gs386.exe pkzip -a _temp_.zip \watc\bin\dos4gw.exe
	if exist gswin.exe pkzip -a _temp_.zip \windows\system\shell.dll \windows\system\commdlg.dll gs*.ico gs*.res
	if exist gsexe.zip erase gsexe.zip
	rename _temp_.zip gsexe.zip

gsfiles.zip: bdftops.bat
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip $(FILES1)
	pkzip -a _temp_.zip $(FILES2)
	pkzip -d _temp_.zip $(RM_FILES)
	pkzip -a _temp_.zip quit.ps
	if exist gsfiles.zip erase gsfiles.zip
	rename _temp_.zip gsfiles.zip

gssrc1.zip: ansi2knr.c
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip $(SRC10)
	pkzip -a _temp_.zip $(SRC11)
	pkzip -a _temp_.zip $(SRC12)
	pkzip -d _temp_.zip $(RM_SRC11)
	pkzip -d _temp_.zip $(RM_SRC12)
	if exist gssrc1.zip erase gssrc1.zip
	rename _temp_.zip gssrc1.zip

gssrc2.zip: gs.c
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip $(SRC2)
	if exist gssrc2.zip erase gssrc2.zip
	rename _temp_.zip gssrc2.zip

gsfonts1.zip: fonts\phvr.gsf fonts\pncr.gsf fonts\pplr.gsf
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip fonts\ph*.* fonts\pn*.* fonts\pp*.*
	if exist gsfonts1.zip erase gsfonts1.zip
	rename _temp_.zip gsfonts1.zip

gsfonts2.zip: fonts\bchr.gsf fonts\cyr.gsf fonts\pagk.gsf fonts\pbkd.gsf
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip fonts\b*.* fonts\cy*.* fonts\pa*.* fonts\pb*.*
	if exist gsfonts2.zip erase gsfonts2.zip
	rename _temp_.zip gsfonts2.zip

gsfonts3.zip: fonts\psyr.gsf fonts\ptmr.gsf fonts\pzdr.gsf fonts\zcr.gsf
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip fonts\ps*.* fonts\pt*.* fonts\pz*.* fonts\z*.*
	if exist gsfonts3.zip erase gsfonts3.zip
	rename _temp_.zip gsfonts3.zip

gsfonts4.zip: fonts\ncrr.gsf fonts\putr.gsf fonts\hrsy_r.gsf fonts\u004006t.gsf
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip fonts\n*.gsf fonts\pu*.* fonts\h*.gsf fonts\u*.gsf
	if exist gsfonts4.zip erase gsfonts4.zip
	rename _temp_.zip gsfonts4.zip
