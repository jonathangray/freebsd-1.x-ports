#	NetHack Makefile.
#	SCCS Id: @(#)Makefile.dat	3.1	92/09/18

VARDAT = data rumors quest.dat oracles options

all:	$(VARDAT) spec_levs quest_levs dungeon

../util/makedefs:
	(cd ../util ; make makedefs)

../util/dgn_comp:
	(cd ../util ; make dgn_comp)

../util/lev_comp:
	(cd ../util ; make lev_comp)

data:	data.base ../util/makedefs
	../util/makedefs -d

rumors:	rumors.tru rumors.fal ../util/makedefs
	../util/makedefs -r

quest.dat:	quest.txt ../util/makedefs
	../util/makedefs -q

oracles:	oracles.txt ../util/makedefs
	../util/makedefs -h

# note: 'options' should have already been made when include/date.h was created
options:	../util/makedefs
	../util/makedefs -v


spec_levs: ../util/lev_comp \
	bigroom.des castle.des endgame.des gehennom.des knox.des medusa.des \
	mines.des oracle.des tower.des yendor.des
	../util/lev_comp bigroom.des
	../util/lev_comp castle.des
	../util/lev_comp endgame.des
	../util/lev_comp gehennom.des
	../util/lev_comp knox.des
	../util/lev_comp medusa.des
	../util/lev_comp mines.des
	../util/lev_comp oracle.des
	../util/lev_comp tower.des
	../util/lev_comp yendor.des
	touch spec_levs

quest_levs: ../util/lev_comp \
	Arch.des Barb.des Caveman.des Elf.des Healer.des Knight.des \
	Priest.des Rogue.des Samurai.des Tourist.des Valkyrie.des Wizard.des
	../util/lev_comp Arch.des
	../util/lev_comp Barb.des
	../util/lev_comp Caveman.des
	../util/lev_comp Elf.des
	../util/lev_comp Healer.des
	../util/lev_comp Knight.des
	../util/lev_comp Priest.des
	../util/lev_comp Rogue.des
	../util/lev_comp Samurai.des
	../util/lev_comp Tourist.des
	../util/lev_comp Valkyrie.des
	../util/lev_comp Wizard.des
	touch quest_levs

dungeon: dungeon.def ../util/makedefs ../util/dgn_comp
	../util/makedefs -e
	../util/dgn_comp dungeon.pdf

spotless:
	-rm -f spec_levs quest_levs *.lev $(VARDAT) dungeon dungeon.pdf
