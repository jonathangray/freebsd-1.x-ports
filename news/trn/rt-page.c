/* $Id: rt-page.c,v 1.4 1993/11/17 23:03:49 nate Exp $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "cache.h"
#include "term.h"
#include "ngdata.h"
#include "trn.h"
#include "util.h"
#include "rthread.h"
#include "rt-select.h"
#include "rt-util.h"
#include "INTERN.h"
#include "rt-page.h"

extern char *display_mode;
extern char sel_disp_char[];

bool
set_sel_mode(ch)
char_int ch;
{
    switch (ch) {
    case 'a':
	set_selector(SM_ARTICLE, sel_artsort);
	break;
    case 's':
	set_selector(SM_SUBJECT, sel_threadsort);
	break;
    case 't':
	if (in_ng && !ThreadedGroup) {
	    bool always_save = thread_always;
	    ThreadedGroup = TRUE;
	    thread_always = TRUE;
	    if (sel_rereading)
		firstart = absfirst;
	    printf("\nThreading the group. "), fflush(stdout);
	    thread_open();
	    thread_always = always_save;
	    if (last_cached < lastart)
		ThreadedGroup = FALSE;
	}
	/* FALL THROUGH */
    case 'T':
	set_selector(SM_THREAD, sel_threadsort);
	break;
    default:
	return FALSE;
    }
    return TRUE;
}

bool
set_sel_sort(ch)
char_int ch;
{
    if (isupper(ch)) {
	sel_direction = -1;
	ch = tolower(ch);
    } else
	sel_direction = 1;
    switch (ch) {
    case 'd':
	sel_sort = SS_DATE;
	break;
    case 's':
	sel_sort = SS_SUBJECT;
	break;
    case 'a':
	sel_sort = SS_AUTHOR;
	break;
    case 'c':
	sel_sort = SS_COUNT;
	break;
    case 'g':
	sel_sort = SS_GROUPS;
	break;
    default:
	return FALSE;
    }
    if (sel_mode == SM_ARTICLE)
	set_selector(sel_mode, sel_sort);
    else
	set_selector(sel_threadmode, sel_sort);
    return TRUE;
}

void
set_selector(smode, ssort)
int smode;
int ssort;
{
    sel_mode = smode;
    sel_sort = ssort;

    if (!ThreadedGroup && sel_mode == SM_THREAD)
	sel_mode = SM_SUBJECT;

    if (sel_mode == SM_ARTICLE) {
	if (sel_sort == SS_COUNT)
	    sel_sort = SS_DATE;
    } else if (sel_sort == SS_AUTHOR || sel_sort == SS_GROUPS)
	sel_sort = SS_DATE;

    switch (sel_mode) {
    case SM_THREAD:
	sel_mode_string = "threads";
	sel_threadmode = smode;
	sel_threadsort = ssort;
	break;
    case SM_SUBJECT:
	sel_mode_string = "subjects";
	sel_threadmode = smode;
	sel_threadsort = ssort;
	break;
    case SM_ARTICLE:
	sel_mode_string = "articles";
	sel_artsort = ssort;
	break;
    }

    switch (sel_sort) {
    case SS_DATE:
	sel_sort_string = "date";
	break;
    case SS_SUBJECT:
	sel_sort_string = "subject";
	break;
    case SS_AUTHOR:
	sel_sort_string = "author";
	break;
    case SS_COUNT:
	sel_sort_string = "count";
	break;
    case SS_GROUPS:
	sel_sort_string = "SubjDate";
	break;
    }
}

void
init_pages()
{
try_again:
    sel_prior_arts = sel_total_arts = 0;

    if (sel_mode == SM_ARTICLE) {
	ARTICLE *ap, **app, **limit;

	if (sel_page_app) {
	    int desired_flags = (sel_rereading? AF_READ : 0);
	    limit = artptr_list + artptr_list_size;
	    for (app = sel_page_app; app < limit; app++) {
		ap = *app;
		if ((ap->flags & (AF_MISSING|AF_READ)) == desired_flags)
		    break;
	    }
	    sort_articles();
	    if (app == limit)
		sel_page_app = artptr_list + artptr_list_size;
	    else {
		for (app = artptr_list; app < limit; app++) {
		    if (*app == ap) {
			sel_page_app = app;
			break;
		    }
		}
	    }
	} else
	    sort_articles();

	while (sel_page_sp && sel_page_sp->misc == 0)
	    sel_page_sp = sel_page_sp->next;
	/* The artptr_list contains only unread or read articles, never both */
	limit = artptr_list + artptr_list_size;
	for (app = artptr_list; app < limit; app++) {
	    ap = *app;
	    if (sel_rereading && !(ap->flags & sel_mask))
		ap->flags |= AF_DEL;
	    if (sel_page_app == app
	     || (!sel_page_app && ap->subj == sel_page_sp)) {
		sel_page_app = app;
		sel_prior_arts = sel_total_arts;
	    }
	    if (!sel_exclusive || (ap->flags & sel_mask)) {
		sel_total_arts++;
		ap->flags |= AF_INCLUDED;
	    } else
		ap->flags &= ~AF_INCLUDED;
	}
	if (sel_exclusive && !sel_total_arts) {
	    sel_exclusive = FALSE;
	    goto try_again;
	}
	if (!sel_page_app)
	    (void) first_page();
	else if (sel_page_app >= limit)
	    (void) last_page();
    } else {
	SUBJECT *sp, *group_sp;
	int group_arts;

	if (sel_page_sp) {
	    while (sel_page_sp && sel_page_sp->misc == 0)
		sel_page_sp = sel_page_sp->next;
	    sort_subjects();
	    if (!sel_page_sp)
		sel_page_sp = last_subject;
	} else
	    sort_subjects();
	for (sp = first_subject; sp; sp = sp->next) {
	    if (sel_rereading && !(sp->flags & sel_mask))
		sp->flags |= SF_DEL;

	    group_sp = sp;
	    group_arts = sp->misc;

	    if (!sel_exclusive || (sp->flags & sel_mask))
		sp->flags |= SF_INCLUDED;
	    else
		sp->flags &= ~SF_INCLUDED;

	    if (sel_page_sp == group_sp)
		sel_prior_arts = sel_total_arts;
	    if (sel_mode == SM_THREAD) {
		while (sp->next && sp->next->thread == sp->thread) {
		    sp = sp->next;
		    if (sp == sel_page_sp) {
			sel_prior_arts = sel_total_arts;
			sel_page_sp = group_sp;
		    }
		    sp->flags &= ~SF_INCLUDED;
		    if (sp->flags & sel_mask)
			group_sp->flags |= SF_INCLUDED;
		    else if (sel_rereading)
			sp->flags |= SF_DEL;
		    group_arts += sp->misc;
		}
	    }
	    if (group_sp->flags & SF_INCLUDED)
		sel_total_arts += group_arts;
	}
	if (sel_exclusive && !sel_total_arts) {
	    sel_exclusive = FALSE;
	    goto try_again;
	}
	if (!sel_page_sp)
	    (void) first_page();
	else if (sel_page_sp == last_subject)
	    (void) last_page();
    }
}

bool
first_page()
{
    sel_prior_arts = 0;

    if (sel_mode == SM_ARTICLE) {
	ARTICLE **app, **limit;

	limit = artptr_list + artptr_list_size;
	for (app = artptr_list; app < limit; app++) {
	    if ((*app)->flags & AF_INCLUDED) {
		if (sel_page_app != app) {
		    sel_page_app = app;
		    return TRUE;
		}
		break;
	    }
	}
    } else {
	SUBJECT *sp;

	for (sp = first_subject; sp; sp = sp->next) {
	    if (sp->flags & SF_INCLUDED) {
		if (sel_page_sp != sp) {
		    sel_page_sp = sp;
		    return TRUE;
		}
		break;
	    }
	}
    }
    return FALSE;
}

bool
last_page()
{
    sel_prior_arts = sel_total_arts;

    if (sel_mode == SM_ARTICLE) {
	ARTICLE **app = sel_page_app;
	sel_page_app = artptr_list + artptr_list_size;
	if (!prev_page())
	    sel_page_app = app;
	else if (app != sel_page_app)
	    return TRUE;
    } else {
	SUBJECT *sp = sel_page_sp;
	sel_page_sp = Nullsubj;
	if (!prev_page())
	    sel_page_sp = sp;
	else if (sp != sel_page_sp)
	    return TRUE;
    }
    return FALSE;
}

bool
next_page()
{
    if (sel_mode == SM_ARTICLE) {
	if (sel_next_app < artptr_list + artptr_list_size) {
	    sel_page_app = sel_next_app;
	    sel_prior_arts += sel_page_arts;
	    return TRUE;
	}
    } else {
	if (sel_next_sp) {
	    sel_page_sp = sel_next_sp;
	    sel_prior_arts += sel_page_arts;
	    return TRUE;
	}
    }
    return FALSE;
}

bool
prev_page()
{
    int item_cnt = 0;

    /* Scan the items in reverse to go back a page */
    if (sel_mode == SM_ARTICLE) {
	ARTICLE *ap, **app, **page_app = sel_page_app;

	for (app = sel_page_app; --app >= artptr_list; ) {
	    ap = *app;
	    if (ap->flags & AF_INCLUDED) {
		page_app = app;
		sel_prior_arts--;
		if (++item_cnt >= sel_max_cnt)
		    break;
	    }
	}
	if (sel_page_app != page_app) {
	    sel_page_app = page_app;
	    return TRUE;
	}
    } else {
	SUBJECT *sp, *page_sp = sel_page_sp;
	int line_cnt, item_arts, line;

	line = 2;
	for (sp = (!page_sp? last_subject : page_sp->prev); sp; sp=sp->prev) {
	    item_arts = sp->misc;
	    if (sel_mode == SM_THREAD) {
		while (sp->prev && sp->prev->thread == sp->thread) {
		    sp = sp->prev;
		    item_arts += sp->misc;
		}
		line_cnt = count_thread_lines(sp, NULL);
	    } else
		line_cnt = count_subject_lines(sp, NULL);
	    if (!(sp->flags & SF_INCLUDED) || !line_cnt)
		continue;
	    if (line_cnt > LINES - 5)
		line_cnt = LINES - 5;
	    line += line_cnt;
	    if (line > LINES - 3) {
		sp = page_sp;
		break;
	    }
	    sel_prior_arts -= item_arts;
	    page_sp = sp;
	    if (++item_cnt >= sel_max_cnt)
		break;
	}
	if (sel_page_sp != page_sp) {
	    sel_page_sp = (page_sp? page_sp : first_subject);
	    return TRUE;
	}
    }
    return FALSE;
}

void
display_page()
{
    int sel;

    sel_chars = getval("SELECTCHARS", SELECTCHARS);
    sel_max_cnt = strlen(sel_chars);
    if (sel_max_cnt > MAX_SEL)
	sel_max_cnt = MAX_SEL;
    if (sel_max_cnt > LINES-5)
	sel_max_cnt = LINES-5;
#ifndef CLEAREOL
    clear();
#else
    if (can_home_clear) {
	home_cursor();
	maybe_eol();
    } else
	clear();
#endif
    carriage_return();

#ifdef NOFIREWORKS
    no_sofire();
#endif
    standout();
    fputs(ngname, stdout);
    un_standout();
    printf("          %ld %sarticle%s", (long)sel_total_arts,
	   sel_rereading? "read " : nullstr,
	   article_count == 1 ? nullstr : "s");
    if (sel_exclusive)
	printf(" out of %ld", (long)article_count);
    printf("%s\n", moderated);
#ifdef CLEAREOL
    maybe_eol();
#endif
    if (redirected && redirected != nullstr)
	printf("\t** Please start using %s **", redirected);
    putchar('\n') FLUSH;
try_again:
    sel_line = 2;
    sel_page_arts = 0;
    sel_item_cnt = 0;

    if (!sel_total_arts)
	;
    else if (sel_mode == SM_ARTICLE) {
	ARTICLE *ap, **app, **limit;

	limit = artptr_list + artptr_list_size;
	app = sel_page_app;
	do {
	    ap = *app;
	    if (ap == sel_last_ap)
		sel_item_index = sel_item_cnt;
	    if (!(ap->flags & AF_INCLUDED))
		continue;
	    sel = !!(ap->flags & sel_mask) + (ap->flags & AF_DEL);
	    sel_items[sel_item_cnt].ptr = (VOIDPTR*)ap;
	    sel_items[sel_item_cnt].line = sel_line;
	    sel_items[sel_item_cnt].sel = sel;
	    sel_page_arts++;
	    /* Output the article, with optional author */
	    display_article(ap, sel_chars[sel_item_cnt], sel);
	    sel_item_cnt++;
	} while (++app < limit && sel_item_cnt < sel_max_cnt);
	if (!sel_page_arts) {
	    if (last_page())
		goto try_again;
	}
	sel_next_app = app;
    } else {
	SUBJECT *sp;
	int line_cnt;
	bool etc;
	char ch;

	sp = sel_page_sp;
	do {
	    if (sp == sel_last_sp)
		sel_item_index = sel_item_cnt;

	    etc = FALSE;
	    if (sp->flags & SF_INCLUDED) {
		/* Compute how many lines we need to display this group */
		if (sel_mode == SM_THREAD)
		    line_cnt = count_thread_lines(sp, &sel);
		else
		    line_cnt = count_subject_lines(sp, &sel);
		if (line_cnt) {
		    /* If this item is too long to fit on the screen all by
		    ** itself, trim it to fit and set the "etc" flag.
		    */
		    if (line_cnt > LINES - 5) {
			line_cnt = LINES - 5;
			etc = TRUE;
		    }
		    /* If it doesn't fit, save it for the next page */
		    if (sel_line + line_cnt > LINES - 3)
			break;
		    sel_items[sel_item_cnt].ptr = (VOIDPTR*)sp;
		    sel_items[sel_item_cnt].line = sel_line;
		    sel_items[sel_item_cnt].sel = sel;
		    sel_page_arts += sp->misc;

		    ch = sel_chars[sel_item_cnt];
		    sel = sel_items[sel_item_cnt].sel;
		    sel_item_cnt++;
		    if (sp->misc) {
			display_subject(sp, ch, sel);
			ch = ' ';
		    }
		}
	    } else
		line_cnt = 0;
	    if (sel_mode == SM_THREAD) {
		while (sp->next && sp->next->thread == sp->thread) {
		    sp = sp->next;
		    if (!line_cnt || !sp->misc)
			continue;
		    if (sel_line < LINES - 3)
			display_subject(sp, ch, sel);
		    ch = ' ';
		    sel_page_arts += sp->misc;
		}
	    }
	    if (etc)
		fputs("      ...etc.", stdout);
	} while ((sp=sp->next)!=Nullsubj && !etc && sel_item_cnt<sel_max_cnt);
	if (!sel_page_arts) {
	    if (last_page())
		goto try_again;
	}
	sel_next_sp = sp;
    }
    sel_last_line = sel_line+1;
    sel_last_ap = Nullart;
    sel_last_sp = Nullsubj;
    sel_at_end = (sel_prior_arts + sel_page_arts == sel_total_arts);
#ifdef CLEAREOL
    maybe_eol();
#endif
    putchar('\n') FLUSH;
}

void
update_page()
{
    int sel;
    int j;

    for (j = 0; j < sel_item_cnt; j++) {
	sel = sel_items[j].sel;
	if (sel_mode == SM_ARTICLE) {
	    ARTICLE *ap = (ARTICLE*)sel_items[j].ptr;
	    if (sel == !!(ap->flags & sel_mask) + (ap->flags & AF_DEL))
		continue;
	} else {
	    SUBJECT *sp = (SUBJECT*)sel_items[j].ptr;
	    int real_sel;
	    if (sel_mode == SM_THREAD)
		(void) count_thread_lines(sp, &real_sel);
	    else
		(void) count_subject_lines(sp, &real_sel);
	    if (sel == real_sel)
		continue;
	}
	goto_line(sel_line, sel_items[j].line);
	sel_line = sel_items[j].line;
	sel_item_index = j;
	output_sel(!sel);
    }
    if (++sel_item_index == sel_item_cnt)
	sel_item_index = 0;
}

void
output_sel(sel)
int sel;
{
    putchar(sel_chars[sel_item_index]);
    putchar(sel_disp_char[sel]);
    sel_items[sel_item_index].sel = sel;
}

/* Counts the number of lines needed to output a subject, including
** optional authors.
*/
static int
count_subject_lines(subj, selptr)
SUBJECT *subj;
int *selptr;
{
    register ARTICLE *ap;
    register int sel;

    if (subj->flags & SF_DEL)
	sel = 2;
    else if (subj->flags & sel_mask) {
	sel = 1;
	for (ap = subj->articles; ap; ap = ap->subj_next) {
	    if ((!(ap->flags&AF_READ) ^ sel_rereading)
	      && !(ap->flags & sel_mask)) {
		sel = 3;
		break;
	    }
	}
    } else
	sel = 0;
    if (selptr)
	*selptr = sel;
    if (*display_mode == 'l')
	return subj->misc;
    if (*display_mode == 'm')
	return (subj->misc <= 4? subj->misc : (subj->misc - 4) / 3 + 4);
    return (subj->misc != 0);
}

/* Counts the number of lines needed to output a thread, including
** optional authors.
*/
static int
count_thread_lines(subj, selptr)
SUBJECT *subj;
int *selptr;
{
    register int total = 0;
    register ARTICLE *thread = subj->thread;
    int sel = -1, subj_sel;

    do {
	if (subj->misc) {
	    total += count_subject_lines(subj, &subj_sel);
	    if (sel < 0)
		sel = subj_sel;
	    else if (sel != subj_sel)
		sel = 3;
	}
    } while ((subj = subj->next) != Nullsubj && subj->thread == thread);
    if (selptr)
	*selptr = (sel < 0? 0 : sel);
    return total;
}

/* Display an article, perhaps with its author.
*/
static void
display_article(ap, ch, sel)
register ARTICLE *ap;
char_int ch;
int sel;
{
    int subj_width = COLS - 5;
    int from_width = COLS / 5;

#ifdef CLEAREOL
    maybe_eol();
#endif
    if (subj_width < 32)
	subj_width = 32;
    
    putchar(ch);
    putchar(sel_disp_char[sel]);
    if (*display_mode == 's' || from_width < 8)
	printf("  %s\n",compress_subj(ap->subj->articles,subj_width)) FLUSH;
    else {
	printf("%s  %s\n",
	   compress_from(ap, from_width),
	   compress_subj(ap, subj_width - from_width)) FLUSH;
    }
    sel_line++;
}

/* Display the given subject group, with optional authors.
*/
static void
display_subject(subj, ch, sel)
SUBJECT *subj;
char_int ch;
int sel;
{
    register ARTICLE *ap;
    register int j, i;
    int subj_width = COLS - 8;
    int from_width = COLS / 5;

#ifdef CLEAREOL
    maybe_eol();
#endif
    if (subj_width < 32)
	subj_width = 32;

    j = subj->misc;

    putchar(ch);
    if (ch != ' ')
	putchar(sel_disp_char[sel]);
    else
	putchar(' ');
    if (*display_mode == 's' || from_width < 8)
	printf("%3d  %s\n",j,compress_subj(subj->articles,subj_width)) FLUSH;
    else {
	/* Find the first unread article so we get the author right */
	for (ap = subj->articles; ap; ap = ap->subj_next) {
	    if (!(ap->flags&AF_READ) ^ sel_rereading)
		break;
	}
	printf("%s%3d  %s\n",
	   compress_from(ap, from_width), j,
	   compress_subj(ap, subj_width - from_width)) FLUSH;
	i = -1;
	if (--j && ap) {
	    for (ap = ap->subj_next; ap && j; ap = ap->subj_next) {
		if (!(!(ap->flags&AF_READ) ^ sel_rereading))
		    continue;
		j--;
		if (i < 0)
		    i = 0;
		else if (*display_mode == 'm') {
		    if (!j) {
			if (i)
			    putchar('\n');
		    } else {
			if (i == 3 || !i) {
			    if (i)
				putchar('\n');
			    if (++sel_line >= LINES - 3)
				return;
#ifdef CLEAREOL
			    maybe_eol();
#endif
			    i = 1;
			} else
			    i++;
			printf("  %s      ",
			       compress_from(ap, from_width)) FLUSH;
			continue;
		    }
		}
		if (++sel_line >= LINES - 3)
		    return;
#ifdef CLEAREOL
		maybe_eol();
#endif
		printf("  %s\n", compress_from(ap, from_width)) FLUSH;
	    }
	}
    }
    sel_line++;
}
