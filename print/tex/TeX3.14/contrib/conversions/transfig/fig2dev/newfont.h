Received: from cu-arpa.cs.cornell.edu by loki.cs.cornell.edu (4.0/I-1.91f)
	id AA13388; Fri, 21 Jul 89 12:48:10 EDT
Received: from thumper.bellcore.com by cu-arpa.cs.cornell.edu (5.61+2/1.91d)
	id AA02218; Fri, 21 Jul 89 12:48:00 -0400
Received: by thumper.bellcore.com (5.54/4.7)
	id AA07930; Fri, 21 Jul 89 12:47:12 EDT
Received: by dino.bellcore.com (4.12/4.7)
	id AA10363; Fri, 21 Jul 89 12:49:10 edt
Date: Fri, 21 Jul 89 12:49:10 edt
From: dana@thumper.bellcore.com (Dana A. Chee)
Message-Id: <8907211649.AA10363@dino.bellcore.com>
To: beck@cs.cornell.edu (Micah Beck)
In-Reply-To: <8907191419.AA10822@utgard.cs.cornell.edu>
Subject: Hello, Dana?
Status: R

Believe it or not, I haven't dropped off the face of the earth.  Yes
I'm still working on it when time permits, and below is my font
scheme (with all ifdefs removed).  As you see, there are three
components to a font, the face (font), style, and size.  Hope this
helps you out (and hopefully an alpha of this will be out soon).

Dana


typedef         struct f_text {
                        int                     type;
#define                                 T_LEFT_JUSTIFIED        0
#define                                 T_CENTER_JUSTIFIED      1
#define                                 T_RIGHT_JUSTIFIED       2
                        int                     font;
#define                                 TIMES                   0
#define                                 HELVETICA               4
#define                                 COURIER                 8
#define                                 NCENBOOK                12
                        int                     size;   /* point size */
                        int                     color;
                        int                     depth;
                        double                  angle;  /* in radian */
                        int                     style;
#define                                 ROMAN           0
#define                                 ITALIC          1
#define                                 OBLIQUE         1
#define                                 BOLD            2
#define                                 BOLD_ITALIC     3
#define                                 BOLD_OBLIQUE    3
                        int                     height; /* pixels */
                        int                     length; /* pixels */
                        int                     base_x;
                        int                     base_y;
                        struct f_pen            *pen;
                        char                    *cstring;
#ifdef  X11
                        GC                      gc;
#endif  /* X11 */
                        struct f_text           *next;
                        }
                F_text;
