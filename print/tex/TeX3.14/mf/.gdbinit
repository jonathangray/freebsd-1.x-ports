directory MFwindow
directory ../common

set env MFPOOL .
set env MFBASES .
set env MFINPUTS .

define ppath
#p mem[curspec].hhfield.LH
#p mem[curspec].hhfield.RH
#p mem[curspec+1].cint
#p mem[curspec+2].cint
p mem[curspec+5].cint
p mem[curspec+6].cint
end

set args \&trap trap 
