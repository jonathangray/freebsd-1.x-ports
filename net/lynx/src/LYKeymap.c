#include "HTUtils.h"
#include "LYUtils.h"
#include "LYKeymap.h"


/* the character gets 1 added to it before lookup,
 * so that EOF maps to 0
 */
char keymap[] = {

0,
/* EOF */

0,                      0,          LYK_PREV_PAGE,    0,
/* nul */           /* ^A */        /* ^B */      /* ^C */

LYK_ABORT,              0,          LYK_NEXT_PAGE,    0,
/* ^D */            /* ^E */        /* ^F */      /* ^G */

LYK_HISTORY,      LYK_NEXT_LINK,    LYK_ACTIVATE,     0,
/* bs */            /* ht */        /* nl */      /* ^K */

LYK_REFRESH,      LYK_ACTIVATE,     LYK_DOWN_TWO,     0,
/* ^L */            /* cr */        /* ^N */      /* ^O */

LYK_UP_TWO,             0,          LYK_RELOAD,       0,
/* ^P */            /* XON */       /* ^R */      /* XOFF */

0,                      0,              0,        LYK_REFRESH,
/* ^T */            /* ^U */        /* ^V */      /* ^W */

0,                      0,              0,            0,
/* ^X */            /* ^Y */        /* ^Z */      /* ESC */

0,                      0,              0,            0,
/* ^\ */            /* ^] */        /* ^^ */      /* ^_ */

LYK_NEXT_PAGE,    LYK_SHELL,            0,            0,
/* sp */             /* ! */         /* " */       /* # */

0,                      0,              0,             0,
/* $ */              /* % */         /* & */        /* ' */

0,                      0,              0,          LYK_NEXT_PAGE,
/* ( */              /* ) */         /* * */        /* + */

LYK_NEXT_PAGE,    LYK_PREV_PAGE,        0,          LYK_WHEREIS,
/* , */              /* - */         /* . */        /* / */

0,                LYK_1,          LYK_2,            LYK_3,
/* 0 */              /* 1 */         /* 2 */        /* 3 */

LYK_4,            LYK_5,          LYK_6,            LYK_7,
/* 4 */              /* 5 */         /* 6 */        /* 7 */

LYK_8,            LYK_9,                0,             0,
/* 8 */              /* 9 */         /* : */        /* ; */

0,                LYK_INFO,             0,          LYK_HELP,
/* < */              /* = */         /* > */        /* ? */

0,              LYK_ADD_BOOKMARK, LYK_PREV_PAGE,    LYK_COMMENT,
/* @ */              /* A */         /* B */        /* C */

LYK_DOWNLOAD,     LYK_EDIT,             0,          LYK_GOTO,
/* D */              /* E */         /* F */        /* G */

LYK_HELP,         LYK_INDEX,            0,             0,
/* H */              /* I */         /* J */        /* K */

0,                LYK_MAIN_MENU,  LYK_NEXT,         LYK_OPTIONS,
/* L */              /* M */         /* N */        /* O */

LYK_PRINT,        LYK_ABORT,           0,           LYK_INDEX_SEARCH,
/* P */              /* Q */         /* R */        /* S */

0,                LYK_PREV_DOC,   LYK_VIEW_BOOKMARK,   0,
/* T */              /* U */         /* V */        /* W */

LYK_FORM_UP,            0,        LYK_FORM_DOWN,       0,
/* X */              /* Y */         /* Z */        /* [ */

LYK_SOURCE,             0,              0,             0,
/* \ */              /* ] */         /* ^ */        /* _ */

0,             LYK_ADD_BOOKMARK,  LYK_PREV_PAGE,    LYK_COMMENT,
/* ` */              /* a */         /* b */        /* c */

LYK_DOWNLOAD,     LYK_EDIT,             0,          LYK_GOTO,
/* d */              /* e */         /* f */        /* g */

LYK_HELP,         LYK_INDEX,            0,             0,
/* h */              /* i */         /* j */        /* k */

0,                LYK_MAIN_MENU,  LYK_NEXT,         LYK_OPTIONS,
/* l */              /* m */         /* n */        /* o */

LYK_PRINT,        LYK_QUIT,            0,           LYK_INDEX_SEARCH,
/* p */              /* q */         /* r */        /* s */

0,                LYK_PREV_DOC,   LYK_VIEW_BOOKMARK,   0,
/* t */              /* u */         /* v */         /* w */

LYK_FORM_UP,            0,          LYK_FORM_DOWN,     0,
/* x */              /* y */          /* z */       /* { */

LYK_PIPE,               0,              0,          LYK_HISTORY,
/* | */               /* } */         /* ~ */       /* del */

LYK_PREV_LINK,    LYK_NEXT_LINK,    LYK_ACTIVATE,   LYK_PREV_DOC,
/* UPARROW */     /* DNARROW */     /* RTARROW */   /* LTARROW */

LYK_NEXT_PAGE,    LYK_PREV_PAGE,    LYK_HOME,       LYK_END,
/* PGDOWN */      /* PGUP */        /* HOME */      /* END */

LYK_HELP,         LYK_ACTIVATE,     LYK_HOME,       LYK_END,
/* F1*/ 	  /* Do key */      /* Find key */  /* Select key */

LYK_UP_TWO,       LYK_DOWN_TWO,
/* Insert key */  /* Remove key */

LYK_DO_NOTHING,
/* DO_NOTHING*/
};

PRIVATE char *revmap[] = {
"UNMAPPED",
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"SOURCE",
"RELOAD",
"PIPE",
"QUIT",
"ABORT",
"NEXT_PAGE",
"PREV_PAGE",
"UP_TWO",
"DOWN_TWO",
"REFRESH",
"HOME",
"END",
"PREV_LINK",
"NEXT_LINK",
"UP_LINK",
"DOWN_LINK",
"RIGHT_LINK",
"LEFT_LINK",
"HISTORY",
"PREV_DOC",
"ACTIVATE",
"GOTO",
"HELP",
"INDEX",
"FORM_UP",
"FORM_DOWN",
"MAIN_MENU",
"OPTIONS",
"INDEX_SEARCH",
"WHEREIS",
"NEXT",
"COMMENT",
"EDIT",
"INFO",
"PRINT",
"ADD_BOOKMARK",
"DEL_BOOKMARK",
"VIEW_BOOKMARK",
"SHELL",
"DOWNLOAD",
"DO_NOTHING",
NULL
};

 /*
  * install func as the mapping for key.
  * func must be present in the revmap table.
  * returns TRUE if teh mapping was made, FALSE if not.
  */
PUBLIC BOOLEAN remap ARGS2(char *,key, char *,func)
 {
       int i;
       char **mp;
       int c = 0;

       if (strlen(key) == 1)
               c = *key;
       else if (strlen(key) == 2 && *key == '^')
               c = key[1] & 037;
       else if (strlen(key) >= 2 && isdigit(*key))
               if (sscanf(key, "%i", &c) != 1)
                       return FALSE;
       for (i = 0, mp = revmap; *mp != NULL; mp++, i++) {
               if (!strncmp(*mp, func,strlen(*mp))) {
                       keymap[c+1] = i;
                       return TRUE;
               }
       }
       return FALSE;
 }


PUBLIC void set_vms_keys NOARGS
{
      keymap[26+1] = LYK_ABORT;  /* control-Z */
      keymap['$'+1] = LYK_SHELL;  
} 

static char saved_vi_keys[4];
static BOOLEAN did_vi_keys;

PUBLIC void set_vi_keys NOARGS
{
      saved_vi_keys[0] = keymap['h'+1];
      keymap['h'+1] = LYK_PREV_DOC;
      saved_vi_keys[1] = keymap['j'+1];
      keymap['j'+1] = LYK_NEXT_LINK;
      saved_vi_keys[2] = keymap['k'+1];
      keymap['k'+1] = LYK_PREV_LINK;
      saved_vi_keys[3] = keymap['l'+1];
      keymap['l'+1] = LYK_ACTIVATE;

      did_vi_keys = TRUE;
}

PUBLIC void reset_vi_keys NOARGS
{
      if (!did_vi_keys)
              return;

      keymap['h'+1] = saved_vi_keys[0];
      keymap['j'+1] = saved_vi_keys[1];
      keymap['k'+1] = saved_vi_keys[2];
      keymap['l'+1] = saved_vi_keys[3];

      did_vi_keys = FALSE;
}

static char saved_emacs_keys[4];
static BOOLEAN did_emacs_keys;

PUBLIC void set_emacs_keys NOARGS
{
      saved_emacs_keys[0] = keymap[2+1];
      keymap[2+1] = LYK_PREV_DOC;       /* ^B */
      saved_emacs_keys[1] = keymap[14+1];
      keymap[14+1] = LYK_NEXT_LINK;     /* ^N */
      saved_emacs_keys[2] = keymap[16+1];
      keymap[16+1] = LYK_PREV_LINK;     /* ^P */
      saved_emacs_keys[3] = keymap[6+1];
      keymap[6+1] = LYK_ACTIVATE;         /* ^F */

      did_emacs_keys = TRUE;
}

PUBLIC void reset_emacs_keys NOARGS
{
      if (!did_emacs_keys)
              return;

      keymap[2+1] = saved_emacs_keys[0];
      keymap[14+1] = saved_emacs_keys[1];
      keymap[16+1] = saved_emacs_keys[2];
      keymap[6+1] = saved_emacs_keys[3];

      did_emacs_keys = FALSE;
}

static char saved_number_keys[8];
static BOOLEAN did_number_keys;

PUBLIC void set_numbers_as_arrows NOARGS
{
      saved_number_keys[0] = keymap['4'+1];
      keymap['4'+1] = LYK_PREV_DOC;
      saved_number_keys[1] = keymap['2'+1];
      keymap['2'+1] = LYK_NEXT_LINK;
      saved_number_keys[2] = keymap['8'+1];
      keymap['8'+1] = LYK_PREV_LINK;
      saved_number_keys[3] = keymap['6'+1];
      keymap['6'+1] = LYK_ACTIVATE;
      saved_number_keys[4] = keymap['7'+1];
      keymap['7'+1] = LYK_HOME;
      saved_number_keys[5] = keymap['1'+1];
      keymap['1'+1] = LYK_END;
      saved_number_keys[6] = keymap['9'+1];
      keymap['9'+1] = LYK_PREV_PAGE;
      saved_number_keys[7] = keymap['3'+1];
      keymap['3'+1] = LYK_NEXT_PAGE;

      did_number_keys = TRUE;
}

PUBLIC void reset_numbers_as_arrows NOARGS
{
      if (!did_number_keys)
              return;

      keymap['4'+1] = saved_number_keys[0];
      keymap['2'+1] = saved_number_keys[1];
      keymap['8'+1] = saved_number_keys[2];
      keymap['6'+1] = saved_number_keys[3];
      keymap['7'+1] = saved_number_keys[4];
      keymap['1'+1] = saved_number_keys[5];
      keymap['9'+1] = saved_number_keys[6];
      keymap['3'+1] = saved_number_keys[7];

      did_number_keys = FALSE;
}
