case `(uname -r) 2>/dev/null` in
*2.1*) libswanted=`echo $libswanted | sed 's/malloc //'` ;;
esac
d_strchr=define
