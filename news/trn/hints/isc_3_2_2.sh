set `echo $libswanted | sed -e 's/ x//' -e 's/malloc //'`
libswanted="inet malloc $*"
