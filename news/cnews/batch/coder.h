char header[] = "Decode the following with bdecode\n";
char codeset[] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-";

#define ENCODE(c)	codeset[c]

extern unsigned short crctab[];
#define CRC(crc, c)	 crc = (crc >> 8) ^ crctab[(crc^c) & 0xff]
