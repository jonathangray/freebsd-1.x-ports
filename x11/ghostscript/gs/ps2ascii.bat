@echo off
if '%1'=='' goto a0
if '%2'=='' goto a1
gs -q -dNODISPLAY -dNOBIND -dWRITESYSTEMDICT -dSIMPLE ps2ascii.ps %1 quit.ps >%2
goto x
:a0
gs -q -dNODISPLAY -dNOBIND -dWRITESYSTEMDICT -dSIMPLE ps2ascii.ps - quit.ps
goto x
:a1
gs -q -dNODISPLAY -dNOBIND -dWRITESYSTEMDICT -dSIMPLE ps2ascii.ps %1 quit.ps
goto x
:x
