NOOBJ= noobj

all clean cleandir depend install lint tags:


pkg:	pkg_src pkg_bin

pkg_src: pkg/+DESC_S pkg/+ONELINE_S pkg/+PLIST_S pkg/+REQ_S
	pkg_create -c pkg/+ONELINE_S -d pkg/+DESC_S -f pkg/+PLIST_S -r pkg/+REQ_S zcrypt_src

pkg_bin:
	
.include <bsd.prog.mk>	
