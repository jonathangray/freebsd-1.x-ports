### MPW Make file for getmac.
# RI is the include directory for RLE files
# RL is the library directory (for librle.a)

RI = :::include:
RL = :::lib:

LIBES = "{RL}librle.o"
IFLAGS = -I "{RI}"
COptions = -d macintosh_os {IFLAGS}

getmac	D getmac.c.o
	Link -w -c 'MPS ' -t MPST getmac.c.o "{RL}librle.o" -o getmac 6
		-sn STDIO=Main 6
		-sn INTENV=Main 6
		-sn %A5Init=Main 6
		"{Libraries}"Stubs.o 6
		"{CLibraries}"CRuntime.o 6
		"{CLibraries}"StdCLib.o 6
		"{CLibraries}"CInterface.o 6
		"{CLibraries}"CSANELib.o 6
		"{Libraries}"Interface.o 6
		"{Libraries}"ToolLibs.o

# Default rule for making a program from its .c file.
# The mv will not be done if there is an error, so a broken program
# will not be left around.
#.c.o		.c
#	C {COptions} {Default}.c
getmac.c.o	D getmac.c
	C {COptions} getmac.c

#		-sn STDIO=Main 
#		-sn INTENV=Main 
#		-sn %A5Init=Main 
