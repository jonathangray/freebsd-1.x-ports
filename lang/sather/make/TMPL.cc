# template to go with a KR C cc compiler

CC           = cc
MAINCC       = ${CC}
SDBCC        = ${CC}

MAINFLAGS    = -DGC_ -DSAFE_RT_ -w
CFLAGS       = -O ${MAINFLAGS}
GCFLAGS      = -O

TCLIBS       =
MFLAGS       =
BOOTFLAGS    =

SKIPSDB      = YES
SKIPGC       = NO

CSFLAGS      = -w -ver -cc '${CC}' -cflags '${CFLAGS}'

