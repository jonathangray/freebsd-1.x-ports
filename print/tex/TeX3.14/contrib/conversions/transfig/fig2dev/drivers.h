extern struct driver dev_box;
extern struct driver dev_epic;
extern struct driver dev_latex;
extern struct driver dev_pic;
extern struct driver dev_pictex;
extern struct driver dev_ps;
extern struct driver dev_pstex;
extern struct driver dev_pstex_t;
extern struct driver dev_textyl;
extern struct driver dev_tpic;

struct 
	{char *name; struct driver *dev;}
	drivers[]
	= {
		{"box",		&dev_box}, 
#ifdef EPIC
		{"epic",	&dev_epic},
		{"eepic",	&dev_epic},
		{"eepicemu",	&dev_epic},
#endif
#ifdef LATEX
		{"latex",	&dev_latex},
#endif
#ifdef PIC
		{"pic",		&dev_pic},
#endif
#ifdef PICTEX
		{"pictex",	&dev_pictex},
#endif
#ifdef PS
		{"ps",		&dev_ps},
#endif
#ifdef PSTEX
		{"pstex",	&dev_pstex},
		{"pstex_t",	&dev_pstex_t},
#endif
#ifdef TEXTYL
		{"textyl",	&dev_textyl},
#endif
#ifdef TPIC
		{"tpic",	&dev_tpic},
#endif
		{"",		NULL}
	};
