/* popauth.h - POP authorization DB definitions */
/* @(#)popauth.h,v 1.1.1.1 1993/01/30 04:42:01 jtc Exp */


struct authinfo {
    char    auth_secret[16];
    int	    auth_secretlen;
};
