#
#
#
SUBDIR = src data docs 
user:
	if test -d /${HOME}/Data; \
	then \
		true; \
	else \
		mkdir /${HOME}/Data; \
	fi;

	if test -d /${HOME}/Prediction; \
	then \
		true; \
	else \
		mkdir ${HOME}/Prediction; \
	fi;

beforeinstall:

	if test -d /usr/local/sattrack; \
	then \
		true; \
	else \
		mkdir /usr/local/sattrack; \
	fi;
	
	if test -d /usr/local/sattrack/Data; \
	then \
		true; \
	else \
		mkdir /usr/local/sattrack/Data; \
	fi;

	if test -d /usr/local/sattrack/Prediction; \
	then \
		true; \
	else \
		mkdir /usr/local/sattrack/Prediction; \
	fi;

	if test -d /usr/local/sattrack/Doc; \
	then \
		true; \
	else \
		mkdir /usr/local/sattrack/Doc; \
	fi;

.include <bsd.subdir.mk>

