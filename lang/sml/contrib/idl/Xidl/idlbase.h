/*
	IDL:    Bernard Sufrin, Oxford
		@(#)idlbase.h	2.1 93/03/07 00:58:09
*/

extern  int     idlcaninput();
extern  int     idlinputchannel();
extern  int     idlcontinue;
extern  void    idlevent();

typedef char *string;
typedef char *address;
typedef void unit;
typedef char bool;
typedef char byte;


extern  string  read_string();
extern  void    write_string();

extern  address read_address();
extern  void    write_address();

extern  short   read_short();
extern  void    write_short();

extern  byte    read_byte();
extern  void    write_byte();

extern  unit    read_unit();
extern  void    write_unit();

extern  int     read_int();
extern  void    write_int();

extern  bool    read_bool();
extern  void    write_bool();


