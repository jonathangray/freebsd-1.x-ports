/*
 * simulation of research unix's <sys/timeb.h> for Uglix.
 */
struct timeb {
	time_t time;
	unsigned short millitm;
	short timezone;
	short dstflag;
};
