$ vo = 'f$verify(0)'
$!
$ ON CONTROL_Y THEN GOTO CLEANUP
$ ON ERROR THEN GOTO CLEANUP
$!
$ vo1 = f$verify(1)
$ library/create libgopher compatible.obj,daarray.obj,-
    gdgopherdir.obj,gsgopherobj.obj,-
    strstring.obj,blblock.obj,views.obj,util.obj,-
    getopt.obj,sockets.obj,Debug.obj
$!
$ vo1 = 'f$verify(0)'
$ CLEANUP:
$    vo1 = f$verify(vo)
$exit
