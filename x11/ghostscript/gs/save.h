/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* save.h */
/* Interface to Ghostscript save/restore machinery */

/* Store checking / change saving */
extern int alloc_save_new_mask;		/* l_new if in save, 0 if not */
extern int alloc_save_test_mask;	/* 0 if in save, -1 if not */
int alloc_array(P4(ref *paref, uint attrs, uint num_refs, const char *client_name));
void alloc_free_array(P2(ref *paref, const char *client_name));
int alloc_save_change(P2(ref *ptr, const char *client_name));

/* Save and restore state */
typedef struct alloc_save_s alloc_save;
typedef struct alloc_change_s alloc_change;
alloc_save *alloc_save_state(P0());	/* 0 if can't alloc save block */
int alloc_save_level(P0());
int alloc_is_since_save(P2(const char *, const alloc_save *));
int alloc_name_is_since_save(P2(const ref *, const alloc_save *));
int alloc_restore_state_check(P1(const alloc_save *));  /* 0 if OK, <0 if not */
void alloc_restore_state(P1(alloc_save *));
/* Release all memory -- like doing a restore "past the bottom". */
void alloc_restore_all(P0());
