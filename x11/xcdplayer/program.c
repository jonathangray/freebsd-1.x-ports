/*
 * Copyright (C) 1990 Regents of the University of California.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of the University of
 * California not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.  the University of California makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 */

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/List.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Toggle.h>
# include <X11/Xaw/Viewport.h>
# include <X11/Xaw/Dialog.h>


# include <stdio.h>
# include <string.h>
# include <sys/malloc.h>

# include "debug.h"
# include "cdrom_globs.h"
#ifdef __FreeBSD__
# include "cdrom_freebsd.h"
#endif
#ifdef sun
# include "cdrom_sun.h"
#endif
#ifdef sgi
# include "cdrom_sgi.h"
#endif


/* externally manipulated vars: */
char	program_str[100];
char	*disc_title = NULL;

static	Widget	program_form;
static	Widget	program_display_form;
static	Widget	program_anchor;
static	Widget	trackbuttons[9];
static	Widget	plus10_button;
static	Widget	new_track_button;
static	Boolean	adding_to_new_track = False;
static	struct	prognode *current_node = NULL;

static	void    cb_move_program_ptr();
static	void    cb_clear_program();
static	void    cb_cancel_program();
static	void    cb_save_program();
static	void    cb_trackbuttons();
static	void    cb_add_10();
static	void    cb_new_track();
static	void    cb_program_all();

void
program_form_setup (parent_widget)
	Widget	parent_widget;
{
        Arg	args[10];
	char	namestr[2];
	int	i;

	Widget	program_display_viewport;
	Widget	track_select_form;
	Widget	clear_button;
	Widget	cancel_button;
	Widget	save_button;
	Widget	all_button;

	/* 
	 * create program form as a non-managed widget, because this 
	 * form "pops up" when requested, as part of the main form, and
	 * therefore moves along with the main form. (Pop-ups don't do this
	 * easily.)
	 */
        program_form = XtCreateWidget("programForm",
				formWidgetClass,
                                parent_widget,
                                (ArgList) NULL, 0);

        program_display_viewport = XtCreateManagedWidget(
				"programDisplayViewport",
                                viewportWidgetClass,
				program_form,
                                (ArgList) NULL, 0);

        program_display_form = XtCreateManagedWidget("programDisplayForm",
                                formWidgetClass,
                                program_display_viewport,
                                (ArgList) NULL, 0);


	XtSetArg(args[0], XtNlabel, " ");
	XtSetArg(args[1], XtNstate, TRUE);
	program_anchor = XtCreateManagedWidget("programSelection",
				toggleWidgetClass,
				program_display_form,
				(ArgList) args, 2);
	XtAddCallback (program_anchor, XtNcallback, cb_move_program_ptr, NULL);


	XtSetArg(args[0], XtNfromVert, program_display_viewport);
	clear_button = XtCreateManagedWidget("Clear",
				commandWidgetClass,
				program_form,
				(ArgList) args, 1);
	XtAddCallback (clear_button, XtNcallback, cb_clear_program, NULL);

	XtSetArg(args[0], XtNfromVert, program_display_viewport);
	XtSetArg(args[1], XtNfromHoriz, clear_button);
	cancel_button = XtCreateManagedWidget("Cancel",
				commandWidgetClass,
				program_form,
				(ArgList) args, 2);
	XtAddCallback (cancel_button, XtNcallback, cb_cancel_program, NULL);


	XtSetArg(args[0], XtNfromVert, program_display_viewport);
	XtSetArg(args[1], XtNfromHoriz, cancel_button);
	save_button = XtCreateManagedWidget("Save",
				commandWidgetClass,
				program_form,
				(ArgList) args, 2);
	XtAddCallback (save_button, XtNcallback, cb_save_program, NULL);


	XtSetArg(args[0], XtNfromVert, clear_button);
        track_select_form = XtCreateManagedWidget("selectForm",
				formWidgetClass,
				program_form,
				(ArgList) args, 1);


	for (i = 1; i <= 9; i++)
	{
	    sprintf(namestr, "%d",i);
	    trackbuttons[i-1] = XtCreateManagedWidget(namestr,
					commandWidgetClass,
					track_select_form,
					(ArgList) NULL, 0);
	    		
	}

	XtAddCallback (trackbuttons[0], XtNcallback, cb_trackbuttons, NULL);

	for (i = 1; i < 9; i++)
	{
	    XtSetArg(args[0], XtNfromHoriz, trackbuttons[i-1]);
	    XtSetValues(trackbuttons[i], args, 1);
	    XtAddCallback (trackbuttons[i], XtNcallback, cb_trackbuttons, NULL);
	}	

	XtSetArg(args[0], XtNfromVert, trackbuttons[0]);
	plus10_button = XtCreateManagedWidget("+10",
				commandWidgetClass,
				track_select_form,
				(ArgList) args, 1);
	XtAddCallback (plus10_button, XtNcallback, cb_add_10, NULL);

	XtSetArg(args[0], XtNfromVert, trackbuttons[0]);
	XtSetArg(args[1], XtNfromHoriz, plus10_button);
	new_track_button = XtCreateManagedWidget("  ",
				commandWidgetClass,
				track_select_form,
				(ArgList) args, 2);
	XtAddCallback (new_track_button, XtNcallback, cb_new_track, NULL);

	XtSetArg(args[0], XtNfromVert, trackbuttons[0]);
	XtSetArg(args[1], XtNfromHoriz, trackbuttons[6]);
	all_button = XtCreateManagedWidget("All",
				commandWidgetClass,
				track_select_form,
				(ArgList) args, 2);
	XtAddCallback (all_button, XtNcallback, cb_program_all, NULL);

}

void
popup_program_form (widget, topLevel, call_data)
Widget		widget;
Widget		topLevel;
XtPointer	call_data;
{
        Arg             args[2];
	int		i;
	char		*progstr;
	char		*token;
	int		track;		

	extern FILE     *disc_info;


	cdi.selection = NULL;

	/* load saved program (if any) */
	if (disc_info != NULL)
	{
	    /* copy "open" program str for tokenization */
	    progstr = strdup(program_str);	
	    token = strtok(progstr, "-"); 	/* find 1st programmed track */
	    if (token != NULL)
	    {
	        debug_printf(1, "using stored program\n");
	        while (token != NULL)
	        {
	            sscanf(token, "%d",&track); /* convert to # */
	            program_add_track((unsigned char)track);
	            token = strtok(NULL,"-"); /* get next track str */
	        }
	    }
	}

	/* clear sensitivity for all */
	for (i=0; i < 9; i++)
	{
	    XtSetArg(args[0], XtNsensitive, FALSE);
	    XtSetValues(trackbuttons[i], args, 1);
	}

	XtSetArg(args[0], XtNsensitive, FALSE);
	XtSetValues(plus10_button, args, 1);
	XtSetValues(new_track_button, args, 1);

	for (i = 0; (i < 9) && (i < cdi.maxtrack); i++)
	{
	    /* set sensitivity for active buttons */
	    XtSetArg(args[0], XtNsensitive, TRUE);
	    XtSetValues(trackbuttons[i], args, 1);
	}
	if (cdi.maxtrack > 9)
	{
	    XtSetArg(args[0], XtNsensitive, TRUE);
	    XtSetValues(plus10_button, args, 1);
	}


	XtManageChild(program_form);
}



void 
make_program_button(node, next_to, label)
struct	prognode *node;
Widget	next_to;
char	label[];
{
	Arg	args[10];
	char	name[2];
	int	i = 0;

	XtSetArg(args[i], XtNfromHoriz, next_to); i++;
	XtSetArg(args[i], XtNlabel, label); i++;
	XtSetArg(args[i], XtNradioGroup, program_anchor); i++;
	XtSetArg(args[i], XtNstate, TRUE); i++; /* activates *this* button */

	node->button = XtCreateManagedWidget("programSelection",
				toggleWidgetClass,
				program_display_form,
				(ArgList) args, i);
	XtAddCallback(node->button, XtNcallback, cb_move_program_ptr, 
		      (XtPointer) node);

	if (node->next != NULL) /* then reposition widget following */
	{
	    XtSetArg(args[0], XtNfromHoriz, node->button);
	    XtSetValues(node->next->button, args, 1);
	}	
}

void 
program_add_track (track)
unsigned char	track;
{
	struct prognode	*insert_point, *new, *next;
	Arg	args[10];
	char	*p;
	char	label[3];
	int	i;

	XawFormDoLayout (program_display_form, FALSE);

	if (cdi.program == NULL)
	{
	    new = (struct prognode *) malloc(sizeof(struct prognode));
	    new->track = track;
	    new->next = NULL;
	    new->prev = NULL;
	    current_node = cdi.program = new;
	    cdi.lastprog = 1;

	    sprintf(label, "%d", track);
	    make_program_button(new, program_anchor, label);
	}

	else if (current_node == NULL) /* pre-insert */
	{
	    new = (struct prognode *) malloc(sizeof(struct prognode));
            new->track = track;
            new->next = cdi.program;
            new->prev = NULL;
	    current_node = cdi.program->prev = new;
	    cdi.program = cdi.program->prev;
	    cdi.lastprog++;

	    sprintf(label, "%d", track);
	    make_program_button(new, program_anchor, label);

	    sprintf(label, "-%d", new->next->track);
	    XtSetArg(args[0], XtNlabel, label);
	    XtSetValues(new->next->button, args, 1);
	}
	else
	{
	    insert_point = cdi.program;

	    while ((insert_point->next != NULL) && 
		   (insert_point != current_node))
		 insert_point = insert_point->next;

	    new = (struct prognode *) malloc(sizeof(struct prognode));
	    new->track = track;
	    new->next = insert_point->next;
	    new->prev = insert_point;
	    if (new->next != NULL)
	        new->next->prev = new; /* complete link back */

	    cdi.lastprog++;
	    current_node = insert_point->next = new;

	    sprintf(label, "-%d", track);
	    make_program_button(new, insert_point->button, label);
	}

	next = cdi.program;
	sprintf(program_str, "%d", next->track);
	for (i = 1; i < cdi.lastprog; i++)
	{
	    next = next->next;
	    sprintf(program_str, "%s-%d", program_str, next->track);
	}


	XawFormDoLayout (program_display_form, TRUE);


	if (cdi.state & CDROM_STATE_PAUSE) 
	{
	    cdi.selection = new;
	    cdi.curtrack = new->track;
	    track_button_update();
	    timer_button_update();
	}
}
		
void 
program_delete_track (node)
struct prognode	*node;
{
	int i;
	Arg	args[2];
	char	*p;
	char	label[3];

	if (cdi.program == NULL)
		return;
	
	if (node->prev != NULL)
	{
	    node->prev->next = node->next;
	    current_node = node->prev;
	    XtSetArg(args[0], XtNstate, True);
	    XtSetValues(current_node->button, args, 1);

	    XtSetArg(args[0], XtNfromHoriz, node->prev->button);
	}
	else
	{
	    cdi.program = node->next;
	    current_node = cdi.program;
	    if (current_node != NULL)
	    {
	    	sprintf(label, "%d", current_node->track);
	    	XtSetArg(args[0], XtNlabel, label);
	    	XtSetArg(args[1], XtNstate, True);
	    	XtSetValues(current_node->button, args, 2);

	    	XtSetArg(args[0], XtNfromHoriz, program_anchor);

	    }
	    else
	    {
	    	XtSetArg(args[0], XtNstate, True);
	    	XtSetValues(program_anchor, args, 1);
	    }
	}

	if (node->next != NULL)
	{
	    node->next->prev = node->prev;
	    XtSetValues(node->next->button, args, 1);
	}


	XtDestroyWidget(node->button);
	free(node);

	cdi.lastprog--;

	if (cdi.program == NULL)
	{
	    program_str[0] = '\0';
	    cdi.selection = NULL;
	}
	else
	{
	    cdi.selection = current_node;

	    if (cdi.state & CDROM_STATE_PAUSE) 
	    {
		cdi.curtrack = current_node->track;
	    	track_button_update();
	    	timer_button_update();
	    }

	    node = cdi.program;
	    sprintf(program_str, "%d", node->track);
	    for (i = 1; i < cdi.lastprog; i++)
	    {
		node = node->next;
		sprintf(program_str, "%s-%d", program_str, node->track);
	    }
	}

}

		
int
program_time_remaining()
{
	struct prognode *node;
	int		time_remaining = 0;

	if ((cdi.program == NULL) || (cdi.selection == NULL))
	    return (0);
		
	node = cdi.selection;
	while (node != NULL)
	{
	    time_remaining += cdi.times[node->track - 1];
	    node = node->next;
	}
	time_remaining -= cdi.duration;
	return (time_remaining);
}


unsigned char
program_goto_next_track()
{
	Arg	arg;
	Boolean	already_on;

	if (cdi.program == NULL)
	    return (0);
	else if (cdi.selection == NULL)
	    cdi.selection = cdi.program;
	else if (cdi.selection->next == NULL)
	{
	    cdi.selection = NULL;
	    return (0);
	}
	else
	    cdi.selection = cdi.selection->next;
	

	XtSetArg(arg, XtNstate, &already_on);
	XtGetValues(cdi.selection->button, &arg, 1);
	if (already_on == FALSE)
	{
	    XtSetArg(arg, XtNstate, TRUE);
	    XtSetValues(cdi.selection->button, &arg, 1);
	}

	pgm_button_set();
	return(cdi.selection->track);
}

unsigned char
program_resume()
{
	unsigned char	track;

	if (cdi.program != NULL)
	{
	    if (cdi.selection == NULL)
		track = program_goto_next_track();
	    else
		track = cdi.selection->track;
	}
	else
	    track = cdi.curtrack;
	pgm_button_set();

	return(track);
}


unsigned char
program_goto_prev_track()
{
	Arg	arg;
	Boolean	already_on;

	if ((cdi.program == NULL) || (cdi.selection->prev == NULL))
	    return (0);

	cdi.selection = cdi.selection->prev;

	XtSetArg(arg, XtNstate, &already_on);
	XtGetValues(cdi.selection->button, &arg, 1);
	if (already_on == FALSE)
	{
	    XtSetArg(arg, XtNstate, TRUE);
	    XtSetValues(cdi.selection->button, &arg, 1);
	}

	pgm_button_set();
	return (cdi.selection->track);
}

unsigned char
program_next_track()
{
	if (cdi.program == NULL)
	    return (0);

	else if (cdi.selection == NULL)
	    return (cdi.program->track);

	else if (cdi.selection->next == NULL)
	    return (0);
	else
	    return (cdi.selection->next->track);

}

unsigned char
program_prev_track()
{
	if ((cdi.program == NULL) || (cdi.selection == NULL) ||
	    (cdi.selection->prev == NULL))
	    return (0);

	else 
	    return (cdi.selection->prev->track);

}
		
void
clear_program()
{
	Arg	arg;

	/* clear new_track button and deactivate */
	XtSetArg(arg, XtNlabel, "  ");
	XtSetValues(new_track_button, &arg, 1);
	XtSetArg(arg, XtNsensitive, False);
	XtSetValues(new_track_button, &arg, 1);
	adding_to_new_track = False;

	if (cdi.program == NULL)
	    return;

	cdi.selection = cdi.program;
	while (cdi.selection->next != NULL)
	{
	    cdi.program = cdi.program-> next;
	    XtDestroyWidget(cdi.selection->button);
	    free(cdi.selection);
	    cdi.selection = cdi.program;
	}
	XtDestroyWidget(cdi.selection->button);
	free(cdi.selection);
	cdi.selection = cdi.program = NULL;
	cdi.lastprog = 0;
	XtSetArg(arg, XtNstate, True);
	XtSetValues(program_anchor, &arg, 1);

	program_str[0]= '\0'; /* clear "store" string */

}

void
program_cancel()
{
	debug_printf(1, "program mode cancelled\n");
	clear_program();

	XtUnmanageChild(program_form);
	pgm_button_reset();
	timer_button_reset();
	timer_button_update();
	cdi.state &= ~CDROM_STATE_PROGRAM;
}

void	
cb_move_program_ptr(widget, node, call_data)
Widget		widget;
struct prognode *node;
XtPointer	call_data;
{
	/*
	 * I cannot get XawToggleGetCurrent to work at all, so this
	 * is a bastardized way of doing this:
	 */

	Arg	arg;
	Boolean	state;

	XtSetArg(arg, XtNstate, &state);
	XtGetValues(widget, &arg, 1);

	if (state == True)
	{
	    if ((current_node == node) && (current_node != NULL))
	    {
		debug_printf(1, "deleting selection from program\n");
		program_delete_track(node);
	    }
	    else
	    {
	        current_node = node;

		if ((current_node != NULL) &&
		    ((cdi.state & CDROM_STATE_PAUSE) ||
		     ((cdi.state & CDROM_STATE_PLAY) == 0)))
		{
		    cdi.selection = node;
		    cdi.curtrack = node->track;
	    	    track_button_update();
	    	    timer_button_update();
		}
	    }
	}

}


static void
cb_program_all(widget, client_data, call_data)
Widget		widget;
XtPointer	client_data;
XtPointer	call_data;
{
	Arg	arg;
	int i;

	clear_program(); 

	debug_printf(1, "program replaced by in-sequence track list\n");
	for (i = 1; i <= cdi.maxtrack; i++)
	    program_add_track ((unsigned char) i);
}

static void
cb_clear_program (widget, client_data, call_data)
Widget		widget;
XtPointer	client_data;
XtPointer	call_data;
{
	clear_program(); /* get rid of program */
	debug_printf(1, "program cleared\n");
}

static void
cb_cancel_program (widget, client_data, call_data)
Widget		widget;
XtPointer	client_data;
XtPointer	call_data;
{
	program_cancel(); /* get rid of program and form */
}

void
cb_save_program (widget, client_data, call_data)
Widget		widget;
XtPointer	client_data;
XtPointer	call_data;
{
	Arg	arg;
	char	*progstr;
	extern FILE	*disc_info;

	disc_info = fopen(info_filename, "w+"); /* open for r/w */
	if (disc_info == NULL) {
		fprintf(stderr,"Can't open save file %s\n",info_filename);
		perror("cb_save_program");
		return;
	}
	fprintf(disc_info, "Title:%s\nProgram: %s\n", disc_title, program_str);
	fflush(disc_info);

	debug_printf(1, "program saved to %s\n", info_filename);
}

void
add_to_new_track (number)
int	number;
{
	Arg	arg;
	char	*namestr;
	int	track = 0;

	/* an apparent bug in my system won't allow the auto-display of this
	 * label change without explicit forcing redraw via something other
	 * than the label itself
	 */
	XtSetArg(arg, XtNsensitive, False);
	XtSetValues(new_track_button, &arg, 1);

	XtSetArg(arg, XtNlabel, &namestr);
	XtGetValues(new_track_button, &arg, 1);
	if (namestr[0] != ' ')
	    sscanf (namestr, "%d", &track);
	track += number;
	sprintf(namestr, "%d\n", track);
	XtSetArg(arg, XtNlabel, namestr);
	XtSetValues(new_track_button, &arg, 1);

	/* reset the sensitive flage for redraw */
	XtSetArg(arg, XtNsensitive, True);
	XtSetValues(new_track_button, &arg, 1);
}

static void
cb_add_10(widget, client_data, call_data)
Widget		widget;
XtPointer	client_data;
XtPointer	call_data;
{
	Arg	arg;
	char	*namestr;
	int	track = 0;

	XtSetArg(arg, XtNsensitive, True);
	XtSetValues(new_track_button, &arg, 1);
	adding_to_new_track = True;
	add_to_new_track(10);
}

static void
cb_new_track(widget, client_data, call_data)
Widget		widget;
XtPointer	client_data;
XtPointer	call_data;
{
	Arg	arg;
	char	*namestr;
	int	track;

	XtSetArg(arg, XtNlabel, &namestr);
	XtGetValues(widget, &arg, 1);
	sscanf (namestr, "%d", &track);

	if ((track >= cdi.mintrack) && (track <= cdi.maxtrack))
	{
	    debug_printf(1, "adding track %d to program\n", track);
	    program_add_track((unsigned char) track);
	}
	XtSetArg(arg, XtNlabel, "  ");
	XtSetValues(new_track_button, &arg, 1);
	XtSetArg(arg, XtNsensitive, False);
	XtSetValues(new_track_button, &arg, 1);
	adding_to_new_track = False;
}


static void
cb_trackbuttons(widget, client_data, call_data)
Widget		widget;
XtPointer	client_data;
XtPointer	call_data;
{
	Arg	arg;
	char	*namestr;
	int	track;
	Boolean state;

	XtSetArg(arg, XtNlabel, &namestr);
	XtGetValues(widget, &arg, 1);
	sscanf (namestr, "%d", &track);

	if (adding_to_new_track)
	    add_to_new_track(track);
	else
	{
	    debug_printf(1, "adding track %d to program\n", track);
	    program_add_track((unsigned char) track);
	}

}
