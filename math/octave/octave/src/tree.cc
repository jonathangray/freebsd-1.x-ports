// Tree class.                                          -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef __GNUG__
#pragma implementation
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <iostream.h>
#include <strstream.h>
#include <ctype.h>
#include <values.h>
#include <stdio.h>

#include "user-prefs.h"
#include "variables.h"
#include "symtab.h"
#include "error.h"
#include "gripes.h"
#include "input.h"
#include "pager.h"
#include "utils.h"
#include "tree.h"
#include "octave.h"
#include "octave-hist.h"
#include "unwind-prot.h"
#include "parse.h"
#include "lex.h"

extern "C"
{
#include <readline/readline.h>
}

// Nonzero means we're breaking out of a loop.
static int breaking = 0;

// Nonzero means we're jumping to the end of a loop.
static int continuing = 0;

// Nonzero means we're returning from a function.
static int returning = 0;

// But first, some functions from the base class that can\'t be
// defined there.

tree_constant
tree::assign (tree_constant& t, tree_constant *args, int nargs)
{
  panic_impossible ();
  return tree_constant ();
}

tree_constant *
tree::eval (int print, int nargout)
{
  if (error_state)
    return NULL_TREE_CONST;

  tree_constant *retval = new tree_constant [nargout+1];
  retval[0] = eval (print);
  return retval;
}

tree_constant
tree::eval (int argc, char **argv, int print)
{
  panic_impossible ();
  return tree_constant ();
}

// But first, some extra functions used by the tree classes.

// We seem to have no use for this now.  Maybe it will be needed at
// some future date, so here it is.
#if 0
/*
 * Convert a linked list of trees to a vector of pointers to trees.
 */
static tree **
list_to_vector (tree *list, int& len)
{
  len = list->length () + 1;

  tree **args = new tree * [len];

// args[0] may eventually hold something useful, like the function
// name.
  tree *tmp_list = list;
  for (int k = 1; k < len; k++)
    {
      args[k] = tmp_list;
      tmp_list = tmp_list->next_elem ();
    }
  return args;
}
#endif

static int
print_as_scalar (const tree_constant& val)
{
  int nr = val.rows ();
  int nc = val.columns ();
  return (val.is_scalar_type ()
	  || val.is_string_type ()
	  || (val.is_matrix_type ()
	      && ((nr == 1 && nc == 1)
		  || nr == 0
		  || nc == 0)));
}

/*
 * Make sure that all arguments have values.
 */
static int
all_args_defined (tree_constant *args, int nargs)
{
  while (--nargs > 0)
    {
      if (args[nargs].is_undefined ())
	return 0;
    }
  return 1;
}

/*
 * Are any of the arguments `:'?
 */
static int
any_arg_is_magic_colon (const tree_constant *args, int nargs)
{
  while (--nargs > 0)
    {
      if (args[nargs].const_type () == tree_constant_rep::magic_colon)
	return 1;
    }
  return 0;
}

// NOTE: functions for the tree_constant_rep and tree_constant classes
// are now defined in tree-const.cc.  This should help speed up
// compilation when working only on the tree_constant class.

/*
 * General matrices.  This list type is much more work to handle than
 * constant matrices, but it allows us to construct matrices from
 * other matrices, variables, and functions.
 */
tree_matrix::tree_matrix (void)
{
  dir = tree::md_none;
  element = NULL_TREE;
  next = (tree_matrix *) NULL;
}

tree_matrix::tree_matrix (tree *t, tree::matrix_dir d)
{
  dir = d;
  element = t;
  next = (tree_matrix *) NULL;
}

tree_matrix::~tree_matrix (void)
{
  delete element;
  delete next;
}

tree_matrix *
tree_matrix::chain (tree *t, tree::matrix_dir d)
{
  tree_matrix *tmp = new tree_matrix (t, d);
  tmp->next = this;
  return tmp;
}

tree_matrix *
tree_matrix::reverse (void)
{
  tree_matrix *list = this;
  tree_matrix *next;
  tree_matrix *prev = (tree_matrix *) NULL;

  while (list != (tree_matrix *) NULL)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

int
tree_matrix::length (void)
{
  tree_matrix *list = this;
  int len = 0;
  while (list != (tree_matrix *) NULL)
    {
      len++;
      list = list->next;
    }
  return len;
}

tree_return_list *
tree_matrix::to_return_list (void)
{
  tree_return_list *retval = (tree_return_list *) NULL;
  tree_matrix *list;
  for (list = this; list != (tree_matrix *) NULL; list = list->next)
    {
      tree *elem = list->element;
      if (elem->is_identifier ())
	{
	  tree_identifier *id = (tree_identifier *) elem;
	  if (list == this)
	    retval = new tree_return_list (id);
	  else
	    retval = retval->chain (id);
	}
      else if (elem->is_index_expression ())
//	       && (((tree_index_expression *) elem) -> arg_list ()
//		   == (tree_argument_list *) NULL))
	{
	  tree_index_expression *idx_expr = (tree_index_expression *) elem;
	  if (list == this)
	    retval = new tree_return_list (idx_expr);
	  else
	    retval = retval->chain (idx_expr);
	}
      else
	{
	  delete retval;
	  retval = (tree_return_list *) NULL;
	  break;
	}
    }

  if (retval != (tree_return_list *) NULL)
    retval = retval->reverse ();
  return retval;
}

// Just about as ugly as it gets.

struct const_matrix_list
{
  tree::matrix_dir dir;
  tree_constant elem;
  int nr;
  int nc;
};

// Less ugly than before, anyway.

tree_constant
tree_matrix::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

// Just count the elements without looking at them.

  int total_len = length ();

// Easier to deal with this later instead of a tree_matrix structure.

  const_matrix_list *list = new const_matrix_list [total_len];

// Stats we want to keep track of.

  int all_strings = 1;

  int found_complex = 0;

  int row_total = 0;
  int col_total = 0;

  int row_height = 0;

  int cols_this_row = 0;

  int first_row = 1;

  int empties_ok = user_pref.empty_list_elements_ok;

  tree_matrix *ptr = this;

// Eliminate empties and gather stats.

  int found_new_row_in_empties = 0;

  int len = 0;
  for (int i = 0; i < total_len; i++)
    {
      tree *elem = ptr->element;
      if (elem == NULL_TREE)
	return tree_constant (Matrix ());

      tree_constant tmp = elem->eval (0);
      if (error_state || tmp.is_undefined ())
	return tree_constant ();

      int nr = tmp.rows ();
      int nc = tmp.columns ();

      matrix_dir direct = ptr->dir;

      if (nr == 0 || nc == 0)
	{
	  if (empties_ok < 0)
	    warning ("empty matrix found in matrix list");
	  else if (empties_ok == 0)
	    {
	      error ("empty matrix found in matrix list");
	      return tree_constant ();
	    }

	  if (direct == md_down)
	    found_new_row_in_empties = 1;

	  goto next;
	}

      if (found_new_row_in_empties)
	{
	  found_new_row_in_empties = 0;
	  list[len].dir = md_down;
	}
      else
	list[len].dir = direct;

      list[len].elem = tmp;
      list[len].nr = nr;
      list[len].nc = nc;

      if (all_strings && ! tmp.is_string_type ())
	all_strings = 0;

      if (! found_complex && tmp.is_complex_type ())
	found_complex = 1;

      len++;

    next:

      ptr = ptr->next;
    }

//  if (all_strings)
//    cerr << "all strings\n";

// Compute size of result matrix, and check to see that the dimensions
// of all the elements will match up properly.

  for (i = 0; i < len; i++)
    {
      matrix_dir direct = list[i].dir;
      int nr = list[i].nr;
      int nc = list[i].nc;

      if (i == 0)
	{
	  row_total = nr;
	  col_total = nc;

	  row_height = nr;
	  cols_this_row = nc;
	}
      else
	{
	  switch (direct)
	    {
	    case md_right:
	      {
		if (nr != row_height)
		  {
		    error ("number of rows must match");
		    return retval;
		  }
		else
		  {
		    cols_this_row += nc;
		    if (first_row)
		      col_total = cols_this_row;
		  }
	      }
	      break;
	    case md_down:
	      {
		if (cols_this_row != col_total)
		  {
		    error ("number of columns must match");
		    return retval;
		  }
		first_row = 0;
		row_total += nr;
		row_height = nr;
		cols_this_row = nc;
	      }
	      break;
	    default:
	      panic_impossible ();
	      break;
	    }
	}
    }

// Don\'t forget to check to see if the last element will fit.

  if (cols_this_row != col_total)
    {
      error ("number of columns must match");
      return retval;
    }

// Now, extract the values from the individual elements and insert
// them in the result matrix.

  Matrix m;
  ComplexMatrix cm;
  char *string = (char *) NULL;
  char *str_ptr = (char *) NULL;

  if (all_strings && row_total == 1 && col_total > 0)
    {
      string = str_ptr = new char [col_total + 1];
      string[col_total] = '\0';
    }
  else if (found_complex)
    cm.resize (row_total, col_total, 0.0);
  else
    m.resize (row_total, col_total, 0.0);

  int put_row = 0;
  int put_col = 0;

  int prev_nr = 0;
  int prev_nc = 0;

  for (i = 0; i < len; i++)
    {
      tree_constant tmp = list[i].elem;
      tree_constant_rep::constant_type tmp_type = tmp.const_type ();

      int nr = list[i].nr;
      int nc = list[i].nc;

      if (nr == 0 || nc == 0)
	continue;

      if (i == 0)
	{
	  put_row = 0;
	  put_col = 0;
	}
      else
	{
	  switch (list[i].dir)
	    {
	    case md_right:
	      put_col += prev_nc;
	      break;
	    case md_down:
	      put_row += prev_nr;
	      put_col = 0;
	      break;
	    default:
	      panic_impossible ();
	      break;
	    }
	}

      if (found_complex)
	{
	  switch (tmp_type)
	    {
	    case tree_constant_rep::scalar_constant:
	      cm (put_row, put_col) = tmp.double_value ();
	      break;
	    case tree_constant_rep::string_constant:
	      if (all_strings && str_ptr != (char *) NULL)
		{
		  memcpy (str_ptr, tmp.string_value (), nc);
		  str_ptr += nc;
		  break;
		}
	    case tree_constant_rep::range_constant:
	      tmp_type = tmp.force_numeric (1);
	      if (tmp_type == tree_constant_rep::scalar_constant)
		m (put_row, put_col) = tmp.double_value ();
	      else if (tmp_type == tree_constant_rep::matrix_constant)
		m.insert (tmp.matrix_value (), put_row, put_col);
	      else
		panic_impossible ();
	      break;
	    case tree_constant_rep::matrix_constant:
	      cm.insert (tmp.matrix_value (), put_row, put_col);
	      break;
	    case tree_constant_rep::complex_scalar_constant:
	      cm (put_row, put_col) = tmp.complex_value ();
	      break;
	    case tree_constant_rep::complex_matrix_constant:
	      cm.insert (tmp.complex_matrix_value (), put_row, put_col);
	      break;
	    case tree_constant_rep::magic_colon:
	    default:
	      panic_impossible ();
	      break;
	    }
	}
      else
	{
	  switch (tmp_type)
	    {
	    case tree_constant_rep::scalar_constant:
	      m (put_row, put_col) = tmp.double_value ();
	      break;
	    case tree_constant_rep::string_constant:
	      if (all_strings && str_ptr != (char *) NULL)
		{
		  memcpy (str_ptr, tmp.string_value (), nc);
		  str_ptr += nc;
		  break;
		}
	    case tree_constant_rep::range_constant:
	      tmp_type = tmp.force_numeric (1);
	      if (tmp_type == tree_constant_rep::scalar_constant)
		m (put_row, put_col) = tmp.double_value ();
	      else if (tmp_type == tree_constant_rep::matrix_constant)
		m.insert (tmp.matrix_value (), put_row, put_col);
	      else
		panic_impossible ();
	      break;
	    case tree_constant_rep::matrix_constant:
	      m.insert (tmp.matrix_value (), put_row, put_col);
	      break;
	    case tree_constant_rep::complex_scalar_constant:
	    case tree_constant_rep::complex_matrix_constant:
	    case tree_constant_rep::magic_colon:
	    default:
	      panic_impossible ();
	      break;
	    }
	}

      prev_nr = nr;
      prev_nc = nc;
    }

  delete [] list;

  if (all_strings && string != (char *) NULL)
    retval = tree_constant (string);
  else if (found_complex)
    retval = tree_constant (cm);
  else
    retval = tree_constant (m);

  return retval;
}

/*
 * Builtin functions.
 */
tree_builtin::tree_builtin (const char *nm = (char *) NULL)
{
  nargin_max = -1;
  nargout_max = -1;
  text_fcn = (Text_fcn) NULL;
  general_fcn = (General_fcn) NULL;
  if (my_name != (char *) NULL)
    my_name = strsave (nm);
}

tree_builtin::tree_builtin (int i_max, int o_max, Mapper_fcn& m_fcn,
			    const char *nm = (char *) NULL)
{
  nargin_max = i_max;
  nargout_max = o_max;
  mapper_fcn = m_fcn;
  text_fcn = (Text_fcn) NULL;
  general_fcn = (General_fcn) NULL;
  if (my_name != (char *) NULL)
    my_name = strsave (nm);
}

tree_builtin::tree_builtin (int i_max, int o_max, Text_fcn t_fcn,
			    const char *nm = (char *) NULL)
{
  nargin_max = i_max;
  nargout_max = o_max;
  text_fcn = t_fcn;
  general_fcn = (General_fcn) NULL;
  if (my_name != (char *) NULL)
    my_name = strsave (nm);
}

tree_builtin::tree_builtin (int i_max, int o_max, General_fcn g_fcn,
			    const char *nm = (char *) NULL)
{
  nargin_max = i_max;
  nargout_max = o_max;
  text_fcn = (Text_fcn) NULL;
  general_fcn = g_fcn;
  if (my_name != (char *) NULL)
    my_name = strsave (nm);
}

tree_builtin::~tree_builtin (void)
{
}

int
tree_builtin::is_builtin (void) const
{
  return 1;
}

tree_constant
tree_builtin::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (text_fcn != (Text_fcn) NULL)
    {
      char **argv = new char * [1];
      argv[0] = strsave (my_name);
      retval = (*text_fcn) (1, argv);
      delete [] argv;
    }
  else if (general_fcn != (General_fcn) NULL)
    {
      tree_constant *argv = new tree_constant [1];
      argv[0] = tree_constant (my_name);
      tree_constant *tmp = (*general_fcn) (argv, 1, 1);
      delete [] argv;
      if (tmp != NULL_TREE_CONST)
	retval = tmp[0];
      delete [] tmp;
    }
  else // Assume mapper function
    message (my_name, "argument expected");

  if (retval.is_defined ())
    return retval.eval (print);
  else
    return retval;
}

tree_constant *
tree_builtin::eval (int print, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (error_state)
    return retval;

  if (general_fcn != (General_fcn) NULL)
    {
      int n_in = 1;
      int n_out = nargout;
      tree_constant *args = NULL_TREE_CONST;
      retval = (*general_fcn) (args, n_in, n_out);
    }
  else
    panic ("%s should be a general builtin function but it's not", my_name);

  if (retval != NULL_TREE_CONST && retval[0].is_defined ())
    retval[0] = retval[0].eval (print);
  return retval;
}

tree_constant
tree_builtin::eval (int argc, char **argv, int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (text_fcn != (Text_fcn) NULL)
    retval = (*text_fcn) (argc, argv);
  else
    error ("%s: requires argument list in parentheses", argv[0]);

  if (retval.is_defined ())
    return retval.eval (print);
  else
    return retval;
}

tree_constant *
tree_builtin::eval (const tree_constant *args, int n_in, int n_out, int print)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (error_state)
    return retval;

  if (general_fcn != (General_fcn) NULL)
    if (any_arg_is_magic_colon (args, n_in))
      error ("invalid use of colon in function argument list");
    else
      retval = (*general_fcn) (args, n_in, n_out);
  else
    {
      if (n_in > nargin_max)
	message (my_name, "too many arguments");
      else if (args[1].is_defined ())
	{
	  tree_constant tmp = args[1].mapper (mapper_fcn, 0);
	  retval = new tree_constant [2];
	  retval[0] = tmp;
	  retval[1] = tree_constant ();
	}	
    }

  if (retval != NULL_TREE_CONST && retval[0].is_defined ())
    retval[0] = retval[0].eval (print);
  return retval;
}

char *
tree_builtin::name (void) const
{
  return my_name;
}

int
tree_builtin::max_expected_args (void)
{
  int ea = nargin_max;
  if (nargin_max < 0)
    ea = MAXINT;
  else
    ea = nargin_max;
  return ea;
}

/*
 * Symbols from the symbol table.
 */
tree_identifier::tree_identifier (int l = -1, int c = -1)
{
  sym = (symbol_record *) NULL;
  line_num = l;
  column_num = c;
  maybe_do_ans_assign = 0;
}

tree_identifier::tree_identifier (symbol_record *s, int l = -1, int c = -1)
{
  sym = s;
  line_num = l;
  column_num = c;
  maybe_do_ans_assign = 0;
}

tree_identifier::~tree_identifier (void)
{
}

int
tree_identifier::is_identifier (void) const
{
  return 1;
}

char *
tree_identifier::name (void) const
{
  return sym->name ();
}

void
tree_identifier::rename (const char *n)
{
  sym->rename (n);
}

tree_identifier *
tree_identifier::define (tree_constant *t)
{
  int status = sym->define (t);
  if (status)
    return this;
  else
    return (tree_identifier *) NULL;
}

tree_identifier *
tree_identifier::define (tree_function *t)
{
  int status = sym->define (t);
  if (status)
    return this;
  else
    return (tree_identifier *) NULL;
}

void
tree_identifier::document (char *s)
{
  if (sym != (symbol_record *) NULL && s != (char *) NULL)
    {
      char *tmp = strsave (s);
      sym->document (tmp);
    }
}

tree_constant
tree_identifier::assign (tree_constant& rhs)
{
  int status = 0;

  if (rhs.is_defined ())
    {
      if (! sym->is_defined ())
	{
	  if (! (sym->is_formal_parameter ()
		 || sym->is_linked_to_global ()))
	    {
	      link_to_builtin_variable (sym);
	    }
	}
      else if (sym->is_function ())
	{
	  sym->clear ();
	}

      tree_constant *tmp = new tree_constant (rhs);
      status = sym->define (tmp);
    }

  if (status)
    return rhs;
  else
    return tree_constant ();
}

tree_constant
tree_identifier::assign (tree_constant& rhs, tree_constant *args, int nargs)
{
  tree_constant retval;

  if (rhs.is_defined ())
    {
      if (! sym->is_defined ())
	{
	  if (! (sym->is_formal_parameter ()
		 || sym->is_linked_to_global ()))
	    {
	      link_to_builtin_variable (sym);
	    }
	}
      else if (sym->is_function ())
	{
	  sym->clear ();
	}

      if (sym->is_variable ())
	{
	  tree *tmp = sym->def ();
	  retval = tmp->assign (rhs, args, nargs);
	}
      else
	{
	  assert (! sym->is_defined ());

	  if (! user_pref.resize_on_range_error)
	    {
	      error ("indexed assignment to previously undefined variables");
	      error ("is only possible when resize_on_range_error is true");
	      return retval;
	    }

	  tree_constant *tmp = new tree_constant ();
	  retval = tmp->assign (rhs, args, nargs);
	  if (retval.is_defined ())
	    sym->define (tmp);
	}
    }

  return retval;
}

void
tree_identifier::bump_value (tree::expression_type etype)
{
  if (sym != (symbol_record *) NULL)
    {
      tree *tmp = sym->def ();
      if (tmp != NULL_TREE)
	tmp->bump_value (etype);
    }
}

int
tree_identifier::parse_m_file (int exec_script = 1)
{
  curr_m_file_name = name ();
  char *mf = m_file_in_path (curr_m_file_name);
  int script_file_executed = parse_m_file (mf, exec_script);
  delete [] mf;

  if (! (error_state || script_file_executed))
    force_link_to_function (name ());

  return script_file_executed;
}

static void
gobble_leading_white_space (FILE *mfile)
{
  int in_comment = 0;
  int c;
  while ((c = getc (mfile)) != EOF)
    {
      if (in_comment)
	{
	  if (c == '\n')
	    in_comment = 0;
	}
      else
	{
	  if (c == ' ' || c == '\t' || c == '\n')
	    continue;
	  else if (c == '%' || c == '#')
	    in_comment = 1;
	  else
	    {
	      ungetc (c, mfile);
	      break;
	    }
	}
    }
}

static int
is_function_file (FILE *mfile)
{
  int status = 0;

  gobble_leading_white_space (mfile);

  long pos = ftell (mfile);

  char buf [10];
  fgets (buf, 10, mfile);
  int len = strlen (buf);
  if (len > 8 && strncmp (buf, "function", 8) == 0
      && ! (isalnum (buf[8]) || buf[8] == '_'))
    status = 1;

  fseek (mfile, pos, SEEK_SET);

  return status;
}

int
tree_identifier::parse_m_file (char *mf, int exec_script = 1)
{
  begin_unwind_frame ("parse_m_file");

  int script_file_executed = 0;

  if (mf != (char *) NULL)
    {
// Open M-file and parse.

      int old_reading_m_file_state = reading_m_file;

      unwind_protect_ptr (rl_instream);
      unwind_protect_ptr (mf_instream);

      unwind_protect_int (using_readline);
      unwind_protect_int (input_line_number);
      unwind_protect_int (current_input_column);
      unwind_protect_int (reading_m_file);

      using_readline = 0;
      reading_m_file = 1;
      input_line_number = 0;
      current_input_column = 1;

      FILE *mfile = get_input_from_file (mf, 0);

      if (mfile != (FILE *) NULL)
	{
// Check to see if this file defines a function or is just a list of
// commands.

	  if (is_function_file (mfile))
	    {
	      parse_m_file (mfile, mf);
	    }
	  else if (exec_script)
	    {
// The value of `reading_m_file' will be restored to the proper value
// when we unwind from this frame.
	      reading_m_file = old_reading_m_file_state;

	      unwind_protect_int (reading_script_file);
	      reading_script_file = 1;

	      parse_and_execute (mfile, 1);

	      script_file_executed = 1;
	    }
	  fclose (mfile);
	}

      run_unwind_frame ("parse_m_file");
    }

  return script_file_executed;
}

void
tree_identifier::parse_m_file (FILE *mfile, char *mf)
{
  begin_unwind_frame ("parse_m_file_2");

  unwind_protect_int (echo_input);
  unwind_protect_int (saving_history);
  unwind_protect_int (reading_m_file);

  echo_input = 0;
  saving_history = 0;
  reading_m_file = 1;

  YY_BUFFER_STATE old_buf = current_buffer ();
  YY_BUFFER_STATE new_buf = create_buffer (mfile);

  add_unwind_protect (restore_input_buffer, (void *) old_buf);
  add_unwind_protect (delete_input_buffer, (void *) new_buf);

  switch_to_buffer (new_buf);

  unwind_protect_ptr (curr_sym_tab);

  int status = yyparse ();

  if (status != 0)
    {
      error ("parse error while reading m-file %s", mf);
      global_sym_tab->clear (curr_m_file_name);
    }

  run_unwind_frame ("parse_m_file_2");
}

void
tree_identifier::eval_undefined_error (void)
{
  char *nm = sym->name ();
  int l = line ();
  int c = column ();
  if (l == -1 && c == -1)
    error ("`%s' undefined");
  else
    error ("`%s' undefined near line %d column %d", nm, l, c);
}

/*
 * Try to find a definition for an identifier.  Here's how:
 *
 *   * If the identifier is already defined and is a function defined
 *     in an m-file that has been modified since the last time we
 *     parsed it, parse it again.
 *
 *   * If the identifier is not defined, try to find a builtin
 *     variable or an already compiled function with the same name.
 *
 *   * If the identifier is still undefined, try looking for an m-file
 *     to parse.
 */
tree *
tree_identifier::do_lookup (int& script_file_executed)
{
  script_file_executed = 0;

  if (! sym->is_linked_to_global ())
    {
      if (sym->is_defined ())
	{
	  if (sym->is_function () && symbol_out_of_date (sym))
	    {
	      script_file_executed = parse_m_file ();
	    }
	}
      else if (! sym->is_formal_parameter ())
	{
	  link_to_builtin_or_function (sym);
	  
	  if (! sym->is_defined ())
	    {
	      script_file_executed = parse_m_file ();
	    }
	  else if (sym->is_function () && symbol_out_of_date (sym))
	    {
	      script_file_executed = parse_m_file ();
	    }
	}
    }

  tree *ans = NULL_TREE;

  if (! script_file_executed)
    ans = sym->def ();

  return ans;
}

void
tree_identifier::mark_as_formal_parameter (void)
{
  if (sym != (symbol_record *) NULL)
    sym->mark_as_formal_parameter ();
}

void
tree_identifier::mark_for_possible_ans_assign (void)
{
  maybe_do_ans_assign = 1;
}

tree_constant
tree_identifier::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  int script_file_executed = 0;

  tree *ans = do_lookup (script_file_executed);

  if (! script_file_executed)
    {
      if (ans == NULL_TREE)
	eval_undefined_error ();
      else
	retval = ans->eval (0);
    }

  if (! error_state && retval.is_defined ())
    {
      if (maybe_do_ans_assign && ! ans->is_constant ())
	{
	  symbol_record *sr = global_sym_tab->lookup ("ans", 1, 0);

	  assert (sr != (symbol_record *) NULL);
      
	  tree_identifier *ans_id = new tree_identifier (sr);

	  tree_constant *tmp = new tree_constant (retval);

	  tree_simple_assignment_expression tmp_ass (ans_id, tmp);

	  tmp_ass.eval (print);
	}
      else
	{
	  if (print)
	    {
	      int pad_after = 0;
	      if (user_pref.print_answer_id_name)
		{
		  char *result_tag = name ();
    
		  if (print_as_scalar (retval))
		    {
		      ostrstream output_buf;
		      output_buf << result_tag << " = " << ends;
		      maybe_page_output (output_buf);
		    }
		  else
		    {
		      pad_after = 1;
		      ostrstream output_buf;
		      output_buf << result_tag << " =\n\n" << ends;
		      maybe_page_output (output_buf);
		    }
		}

	      retval.eval (print);

	      if (pad_after)
		{
		  ostrstream output_buf;
		  output_buf << "\n" << ends;
		  maybe_page_output (output_buf);
		}
	    }
	}
    }
  return retval;
}

tree_constant *
tree_identifier::eval (int print, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (error_state)
    return retval;

  int script_file_executed = 0;

  tree *ans = do_lookup (script_file_executed);

  if (! script_file_executed)
    {
      if (ans == NULL_TREE)
	eval_undefined_error ();
      else
	retval = ans->eval (print, nargout);
    }

  return retval;
}

tree_constant
tree_identifier::eval (int argc, char **argv, int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  int script_file_executed = 0;

  tree *ans = do_lookup (script_file_executed);

  if (! script_file_executed)
    {
      if (ans == NULL_TREE)
	eval_undefined_error ();
      else
	retval = ans->eval (argc, argv, print);
    }

  return retval;
}

tree_constant *
tree_identifier::eval (const tree_constant *args, int nargin, int nargout,
		       int print)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (error_state)
    return retval;

  int script_file_executed = 0;

  tree *ans = do_lookup (script_file_executed);

  if (! script_file_executed)
    {
      if (ans == NULL_TREE)
	eval_undefined_error ();
      else
	{
	  if (maybe_do_ans_assign && nargout == 1)
	    {
	      retval = ans->eval (args, nargin, nargout, 0);

	      if (retval != NULL_TREE_CONST && retval[0].is_defined ())
		{
		  symbol_record *sr = global_sym_tab->lookup ("ans", 1, 0);

		  assert (sr != (symbol_record *) NULL);
      
		  tree_identifier *ans_id = new tree_identifier (sr);

		  tree_constant *tmp = new tree_constant (retval[0]);

		  tree_simple_assignment_expression tmp_ass (ans_id, tmp);

		  tmp_ass.eval (print);
		}
	    }
	  else
	    retval = ans->eval (args, nargin, nargout, print);
	}
    }

  return retval;
}

/*
 * User defined functions.
 */
tree_function::tree_function (void)
{
  call_depth = 0;
  param_list = (tree_parameter_list *) NULL;
  ret_list = (tree_parameter_list *) NULL;
  sym_tab = (symbol_table *) NULL;
  cmd_list = NULL_TREE;
  file_name = (char *) NULL;
  fcn_name = (char *) NULL;
  t_parsed = 0;
  system_m_file = 0;
}

tree_function::tree_function (tree *cl, symbol_table *st)
{
  call_depth = 0;
  param_list = (tree_parameter_list *) NULL;
  ret_list = (tree_parameter_list *) NULL;
  sym_tab = st;
  cmd_list = cl;
  file_name = (char *) NULL;
  fcn_name = (char *) NULL;
  t_parsed = 0;
  system_m_file = 0;
}

tree_function::~tree_function (void)
{
}

tree_function *
tree_function::define (tree *t)
{
  cmd_list = t;
  return this;
}

tree_function *
tree_function::define_param_list (tree_parameter_list *t)
{
  param_list = t;
  return this;
}

tree_function *
tree_function::define_ret_list (tree_parameter_list *t)
{
  ret_list = t;
  return this;
}

void
tree_function::stash_m_file_name (char *s)
{
  file_name = strsave (s);
}

void
tree_function::stash_m_file_time (time_t t)
{
  t_parsed = t;
}

char *
tree_function::m_file_name (void)
{
  return file_name;
}

time_t
tree_function::time_parsed (void)
{
  return t_parsed;
}

void
tree_function::mark_as_system_m_file (void)
{
  if (file_name != (char *) NULL)
    {
// We really should stash the whole path to the file we found, when we
// looked it up, to avoid possible race conditions...  XXX FIXME XXX
//
// We probably also don't need to get the library directory every
// time, but since this function is only called when the M-file is
// parsed, it probably doesn't matter that much.

      char *oct_lib = octave_lib_dir ();
      int len = strlen (oct_lib);

      char *mf_name = m_file_in_path (file_name);

      if (strncmp (oct_lib, mf_name, len) == 0)
	system_m_file = 1;

      delete [] mf_name;
    }
  else
    system_m_file = 0;
}

int
tree_function::is_system_m_file (void) const
{
  return system_m_file;
}

void
tree_function::stash_function_name (char *s)
{
  fcn_name = strsave (s);
}

char *
tree_function::function_name (void)
{
  return fcn_name;
}

tree_constant
tree_function::eval (int print)
{
  tree_constant retval;

  if (error_state || cmd_list == NULL_TREE)
    return retval;

  tree_constant *tmp = eval (print, 1);

  if (! error_state && tmp != NULL_TREE_CONST)
    retval = tmp[0];
  delete [] tmp;

  return retval;
}

// For unwind protect.

static void
clear_symbol_table (void *table)
{
  symbol_table *tmp = (symbol_table *) table;
  tmp->clear ();
}

tree_constant *
tree_function::eval (int print, int nargout)
{
  return eval (NULL_TREE_CONST, 1, nargout, print);
}

tree_constant
tree_function::eval (int argc, char **argv, int print)
{
  if (! error_state)
    error ("%s: requires argument list in parentheses", argv[0]);
  return tree_constant ();
}

tree_constant *
tree_function::eval (const tree_constant *args, int nargin,
		     int nargout, int print)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (error_state)
    return retval;

  if (cmd_list == NULL_TREE)
    return retval;

  begin_unwind_frame ("func_eval");

  unwind_protect_int (call_depth);
  call_depth++;

  if (call_depth > 1)
    {
      error ("recursive function calls not supported yet");
      goto abort;
    }

// Force symbols to be undefined again when this function exits.

  add_unwind_protect (clear_symbol_table, (void *) sym_tab);

// Save old and set current symbol table context, for eval_undefined_error().

  unwind_protect_ptr (curr_sym_tab);
  curr_sym_tab = sym_tab;

  if (param_list != (tree_parameter_list *) NULL)
    {
      int expected_nargin = param_list->length () + 1;
      tree_parameter_list *ptr = param_list;
      for (int i = 1; i < expected_nargin; i++)
	{
	  tree_constant *tmp;
	  if (i < nargin)
	    {
	      if (args[i].is_defined ()
		  && args[i].const_type () == tree_constant_rep::magic_colon)
		{
		  error ("invalid use of colon in function argument list");
		  goto abort;
		}
	      tmp = new tree_constant (args[i]);
	    }
	  else
	    tmp = NULL_TREE_CONST;

	  ptr->define (tmp);
	  ptr = ptr->next_elem ();
	}
    }

// The following code is in a separate scope to avoid warnings from
// G++ about `goto abort' crossing the initialization of some
// variables.

  {
    bind_nargin_and_nargout (sym_tab, nargin, nargout);
      
// Evaluate the commands that make up the function.  Always turn on
// printing for commands inside functions.   Maybe this should be
// toggled by a user-leval variable?

    int pf = ! user_pref.silent_functions;
    tree_constant last_computed_value = cmd_list->eval (pf);

    if (returning)
      returning = 0;

    if (error_state)
      {
	traceback_error ();
	goto abort;
      }
    
// Copy return values out.

    if (ret_list != (tree_parameter_list *) NULL)
      {
	int nout = ret_list->length ();
	retval = new tree_constant [nout+1];
	int i = 0;
	tree_parameter_list *elem = ret_list;
	for ( ; elem != (tree_parameter_list *) NULL;
	     elem = elem->next_elem ()) 
	  retval[i++] = elem->eval (0);
	retval [nout] = tree_constant ();

	if (retval != NULL_TREE_CONST && retval[0].is_defined ())
	  retval[0].eval (print);
      }
    else if (user_pref.return_last_computed_value)
      {
	retval = new tree_constant [2];
	retval[0] = last_computed_value;
	retval[1] = tree_constant ();
      }
  }

 abort:
  run_unwind_frame ("func_eval");

  return retval;
}

int
tree_function::max_expected_args (void)
{
  if (param_list != NULL_TREE)
    return param_list->length () + 1;
  else
    return 1;
}

void
tree_function::traceback_error (void)
{
  error_state = -1;
  if (fcn_name != (char *) NULL)
    {
      if (file_name != (char *) NULL)
	error ("called from `%s' in file `%s'", fcn_name, file_name);
      else 
	error ("called from `%s'", fcn_name);
    }
  else
    {
      if (file_name != (char *) NULL)
	error ("called from file `%s'", file_name);
      else
	error ("called from `?unknown?'");
    }
}

/*
 * Expressions.
 */
tree_expression::tree_expression (void)
{
  etype = tree::unknown;
}

tree_expression::~tree_expression (void)
{
}

tree_constant
tree_expression::eval (int print)
{
  panic ("invalid evaluation of generic expression");
  return tree_constant ();
}

/*
 * Prefix expressions.
 */
tree_prefix_expression::tree_prefix_expression (int l = -1, int c = -1)
{
  id = (tree_identifier *) NULL;
  etype = unknown;
  line_num = l;
  column_num = c;
}

tree_prefix_expression::tree_prefix_expression (tree_identifier *t,
						tree::expression_type et,
						int l = -1, int c = -1)
{
  id = t;
  etype = et;
  line_num = l;
  column_num = c;
}

tree_prefix_expression::~tree_prefix_expression (void)
{
  delete id;
}

tree_constant
tree_prefix_expression::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (id != (tree_identifier *) NULL)
    {
      id->bump_value (etype);
      retval = id->eval (print);
      if (error_state)
	{
	  retval = tree_constant ();
	  if (error_state)
	    eval_error ();
	}
    }
  return retval;
}

void
tree_prefix_expression::eval_error (void)
{
  if (error_state > 0)
    {
      char *op;
      switch (etype)
	{
	case tree::increment: op = "++";      break;
	case tree::decrement: op = "--";      break;
	default:              op = "unknown"; break;
	}

      error ("evaluating prefix operator `%s' near line %d, column %d",
	     op, line (), column ());
    }
}

int
tree_prefix_expression::is_prefix_expression (void) const
{
  return 1;
}

/*
 * Postfix expressions.
 */
tree_postfix_expression::tree_postfix_expression (int l = -1, int c = -1)
{
  id = (tree_identifier *) NULL;
  etype = unknown;
  line_num = l;
  column_num = c;
}

tree_postfix_expression::tree_postfix_expression (tree_identifier *t,
						  tree::expression_type et,
						  int l = -1, int c = -1)
{
  id = t;
  etype = et;
  line_num = l;
  column_num = c;
}

tree_postfix_expression::~tree_postfix_expression (void)
{
  delete id;
}

tree_constant
tree_postfix_expression::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (id != (tree_identifier *) NULL)
    {
      retval = id->eval (print);
      id->bump_value (etype);
      if (error_state)
	{
	  retval = tree_constant ();
	  if (error_state)
	    eval_error ();
	}
    }
  return retval;
}

void
tree_postfix_expression::eval_error (void)
{
  if (error_state > 0)
    {
      char *op;
      switch (etype)
	{
	case tree::increment: op = "++";      break;
	case tree::decrement: op = "--";      break;
	default:              op = "unknown"; break;
	}

      error ("evaluating postfix operator `%s' near line %d, column %d",
	     op, line (), column ());
    }
}

/*
 * Unary expressions.
 */
tree_unary_expression::tree_unary_expression (int l = -1, int c = -1)
{
  etype = tree::unknown;
  op = NULL_TREE;
  line_num = l;
  column_num = c;
}

tree_unary_expression::tree_unary_expression (tree *a,
					      tree::expression_type t,
					      int l = -1, int c = -1)
{
  etype = t;
  op = a;
  line_num = l;
  column_num = c;
}

tree_unary_expression::~tree_unary_expression (void)
{
  delete op;
}

tree_constant
tree_unary_expression::eval (int print)
{
  if (error_state)
    return tree_constant ();

  tree_constant ans;

  switch (etype)
    {
    case tree::not:
    case tree::uminus:
    case tree::hermitian:
    case tree::transpose:
      if (op != NULL_TREE)
	{
	  tree_constant u = op->eval (0);
	  if (error_state)
	    eval_error ();
	  else if (u.is_defined ())
	    {
	      ans = do_unary_op (u, etype);
	      if (error_state)
		{
		  ans = tree_constant ();
		  if (error_state)
		    eval_error ();
		}
	    }
	}
      break;
    default:
      error ("unary operator %d not implemented", etype);
      break;
    }

  if (ans.is_defined ())
    return ans.eval (print);
  else
    return ans;
}

void
tree_unary_expression::eval_error (void)
{
  if (error_state > 0)
    {
      char *op;
      switch (etype)
	{
	case tree::not:        op = "!";       break;
	case tree::uminus:     op = "-";       break;
	case tree::hermitian:  op = "'";       break;
	case tree::transpose:  op = ".'";      break;
	default:               op = "unknown"; break;
	}

      error ("evaluating unary operator `%s' near line %d, column %d",
	     op, line (), column ());
    }
}

/*
 * Binary expressions.
 */
tree_binary_expression::tree_binary_expression (int l = -1, int c = -1)
{
  etype = tree::unknown;
  op1 = NULL_TREE;
  op2 = NULL_TREE;
  line_num = l;
  column_num = c;
}

tree_binary_expression::tree_binary_expression (tree *a, tree *b,
						tree::expression_type t,
						int l = -1, int c = -1)
{
  etype = t;
  op1 = a;
  op2 = b;
  line_num = l;
  column_num = c;
}

tree_binary_expression::~tree_binary_expression (void)
{
  delete op1;
  delete op2;
}

tree_constant
tree_binary_expression::eval (int print)
{
  if (error_state)
    return tree_constant ();

  tree_constant ans;
  switch (etype)
    {
    case tree::add:
    case tree::subtract:
    case tree::multiply:
    case tree::el_mul:
    case tree::divide:
    case tree::el_div:
    case tree::leftdiv:
    case tree::el_leftdiv:
    case tree::power:
    case tree::elem_pow:
    case tree::cmp_lt:
    case tree::cmp_le:
    case tree::cmp_eq:
    case tree::cmp_ge:
    case tree::cmp_gt:
    case tree::cmp_ne:
    case tree::and:
    case tree::or:
      if (op1 != NULL_TREE)
	{
	  tree_constant a = op1->eval (0);
	  if (error_state)
	    eval_error ();
	  else if (a.is_defined () && op2 != NULL_TREE)
	    {
	      tree_constant b = op2->eval (0);
	      if (error_state)
		eval_error ();
	      else if (b.is_defined ())
		{
		  ans = do_binary_op (a, b, etype);
		  if (error_state)
		    {
		      ans = tree_constant ();
		      if (error_state)
			eval_error ();
		    }
		}
	    }
	}
      break;
    default:
      error ("binary operator %d not implemented", etype);
      break;
    }

  if (ans.is_defined ())
    return ans.eval (print);
  else
    return ans;
}

void
tree_binary_expression::eval_error (void)
{
  if (error_state > 0)
    {
      char *op;
      switch (etype)
	{
	case tree::add:        op = "+";       break;
	case tree::subtract:   op = "-";       break;
	case tree::multiply:   op = "*";       break;
	case tree::el_mul:     op = ".*";      break;
	case tree::divide:     op = "/";       break;
	case tree::el_div:     op = "./";      break;
	case tree::leftdiv:    op = "\\";      break;
	case tree::el_leftdiv: op = ".\\";     break;
	case tree::power:      op = "^";       break;
	case tree::elem_pow:   op = ".^";      break;
	case tree::cmp_lt:     op = "<";       break;
	case tree::cmp_le:     op = "<=";      break;
	case tree::cmp_eq:     op = "==";      break;
	case tree::cmp_ge:     op = ">=";      break;
	case tree::cmp_gt:     op = ">";       break;
	case tree::cmp_ne:     op = "!=";      break;
	case tree::and:        op = "&&";      break;
	case tree::or:         op = "||";      break;
	default:               op = "unknown"; break;
	}

      error ("evaluating binary operator `%s' near line %d, column %d",
	     op, line (), column ());
    }
}

/*
 * Assignment expressions.
 */
tree_assignment_expression::tree_assignment_expression (void)
{
  in_parens = 0;
  etype = tree::assignment;
}

tree_assignment_expression::~tree_assignment_expression (void)
{
}

tree_constant
tree_assignment_expression::eval (int print)
{
  panic ("invalid evaluation of generic expression");
  return tree_constant ();
}

int
tree_assignment_expression::is_assignment_expression (void) const
{
  return 1;
}

/*
 * Simple assignment expressions.
 */
tree_simple_assignment_expression::tree_simple_assignment_expression
  (int l = -1, int c = -1)
{
  etype = tree::assignment;
  lhs = (tree_identifier *) NULL;
  index = (tree_argument_list *) NULL;
  rhs = NULL_TREE;
  line_num = l;
  column_num = c;
}

tree_simple_assignment_expression::tree_simple_assignment_expression
  (tree_identifier *i, tree *r, int l = -1, int c = -1)
{
  etype = tree::assignment;
  lhs = i;
  index = (tree_argument_list *) NULL;
  rhs = r;
  line_num = l;
  column_num = c;
}

tree_simple_assignment_expression::tree_simple_assignment_expression
  (tree_index_expression *idx_expr, tree *r, int l = -1, int c = -1)
{
  etype = tree::assignment;
  lhs = idx_expr->ident ();
  index = idx_expr->arg_list ();
  rhs = r;
  line_num = l;
  column_num = c;
}

tree_simple_assignment_expression::~tree_simple_assignment_expression (void)
{
//  delete lhs;
//  delete index; 
  delete rhs;
}

tree_constant
tree_simple_assignment_expression::eval (int print)
{
  assert (etype == tree::assignment);

  tree_constant ans;
  tree_constant retval;

  if (error_state)
    return retval;

  if (rhs != NULL_TREE)
    {
      tree_constant rhs_val = rhs->eval (0);
      if (error_state)
	{
	  if (error_state)
	    eval_error ();
	}
      else if (index == NULL_TREE)
	{
	  ans = lhs->assign (rhs_val);
	  if (error_state)
	    eval_error ();
	}
      else
	{
// Extract the arguments into a simple vector.
	  int nargs = 0;
	  tree_constant *args = index->convert_to_const_vector (nargs);
	  if (nargs > 1 && ! error_state)
	    {
	      ans = lhs->assign (rhs_val, args, nargs);
	      if (error_state)
		eval_error ();
	    }
	  else if (error_state)
	    eval_error ();

	  delete [] args;
	}
    }

  if (! error_state && ans.is_defined ())
    {
      int pad_after = 0;
      if (print && user_pref.print_answer_id_name)
	{
	  if (print_as_scalar (ans))
	    {
	      ostrstream output_buf;
	      output_buf << lhs->name () << " = " << ends;
	      maybe_page_output (output_buf);
	    }
	  else
	    {
	      pad_after = 1;
	      ostrstream output_buf;
	      output_buf << lhs->name () << " =\n\n" << ends;
	      maybe_page_output (output_buf);
	    }
	}

      retval = ans.eval (print);

      if (print && pad_after)
	{
	  ostrstream output_buf;
	  output_buf << "\n" << ends;
	  maybe_page_output (output_buf);
	}
    }

  return retval;
}

void
tree_simple_assignment_expression::eval_error (void)
{
  if (error_state > 0)
    {
      int l = line ();
      int c = column ();
      if (l != -1 && c != -1)
	error ("evaluating assignment expression near line %d, column %d",
	       l, c);
//      else
//	error ("evaluating assignment expression");
    }
}

/*
 * Multi-valued assignmnt expressions.
 */
tree_multi_assignment_expression::tree_multi_assignment_expression
  (int l = -1, int c = -1)
{
  etype = tree::multi_assignment;
  lhs = (tree_return_list *) NULL;
  rhs = NULL_TREE;
  line_num = l;
  column_num = c;
}

tree_multi_assignment_expression::tree_multi_assignment_expression
  (tree_return_list *lst, tree *r, int l = -1, int c = -1)
{
  etype = tree::multi_assignment;
  lhs = lst;
  rhs = r;
  line_num = l;
  column_num = c;
}

tree_multi_assignment_expression::~tree_multi_assignment_expression (void)
{
  delete lhs;
  delete rhs;
}

tree_constant
tree_multi_assignment_expression::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  tree_constant *result = eval (print, 1);

  if (result != NULL_TREE_CONST)
    {
      retval = result[0];
      delete [] result;
    }

  return retval;
}

tree_constant *
tree_multi_assignment_expression::eval (int print, int nargout)
{
  assert (etype == tree::multi_assignment);

  if (error_state || rhs == NULL_TREE)
    return NULL_TREE_CONST;

  nargout = lhs->length ();
  tree_constant *results = rhs->eval (0, nargout);

  if (error_state)
    eval_error ();

  int ma_line = line ();
  int ma_column = column ();

  if (results != NULL_TREE_CONST)
    {
      tree_return_list *elem;
      int i = 0;
      int pad_after = 0;
      int last_was_scalar_type = 0;
      for (elem = lhs; elem != (tree_return_list *) NULL;
	   elem = elem->next_elem ())
	{
	  tree_index_expression *lhs_expr = elem->idx_expr ();
	  if (i < nargout)
	    {
	      if (results[i].is_undefined ())
		{
		  tree_simple_assignment_expression tmp_expr
		    (lhs_expr, NULL_TREE_CONST, ma_line, ma_column);

		  results[i] = tmp_expr.eval (0); // Should stay undefined!

		  if (error_state)
		    break;

		  if (last_was_scalar_type && i == 1)
		    pad_after = 0;

		  break;
		}
	      else
		{
		  tree_constant *tmp = new tree_constant (results[i]);

		  tree_simple_assignment_expression tmp_expr
		    (lhs_expr, tmp, ma_line, ma_column);

		  results[i] = tmp_expr.eval (0); // May change

		  if (error_state)
		    break;

		  if (print && pad_after)
		    {
		      ostrstream output_buf;
		      output_buf << "\n" << '\0';
		      maybe_page_output (output_buf);
		    }

		  if (print && user_pref.print_answer_id_name)
		    {
		      tree_identifier *tmp_id = lhs_expr->ident ();
		      char *tmp_nm = tmp_id->name ();

		      if (print_as_scalar (results[i]))
			{
			  ostrstream output_buf;
			  output_buf << tmp_nm << " = " << '\0';
			  maybe_page_output (output_buf);
			  last_was_scalar_type = 1;
			}
		      else
			{
			  ostrstream output_buf;
			  output_buf << tmp_nm << " =\n\n" << '\0';
			  maybe_page_output (output_buf);
			  last_was_scalar_type = 0;
			}
		    }
		  results[i].eval (print);
		  pad_after++;
		}
	      i++;
	    }
	  else
	    {
	      tree_simple_assignment_expression tmp_expr
		(lhs_expr, NULL_TREE_CONST, ma_line, ma_column);

	      tmp_expr.eval (0);

	      if (error_state)
		break;

	      if (last_was_scalar_type && i == 1)
		pad_after = 0;

	      break;
	    }
	}

      if (print && pad_after)
	{
	  ostrstream output_buf;
	  output_buf << "\n" << '\0';
	  maybe_page_output (output_buf);
	}
    }

  return results;
}

void
tree_multi_assignment_expression::eval_error (void)
{
  if (error_state > 0)
    error ("evaluating assignment expression near line %d, column %d",
	   line (), column ());
}

/*
 * Colon expressions.
 */
tree_colon_expression::tree_colon_expression (int l = -1, int c = -1)
{
  etype = tree::colon;
  op1 = NULL_TREE;
  op2 = NULL_TREE;
  op3 = NULL_TREE;
  line_num = l;
  column_num = c;
}

tree_colon_expression::tree_colon_expression (tree *a, tree *b,
					      int l = -1, int c = -1)
{
  etype = tree::colon;
  op1 = a;		// base
  op2 = b;		// limit
  op3 = NULL_TREE;	// increment if not empty.
  line_num = l;
  column_num = c;
}

tree_colon_expression::~tree_colon_expression (void)
{
  delete op1;
  delete op2;
  delete op3;
}

tree_colon_expression *
tree_colon_expression::chain (tree *t)
{
  tree_colon_expression *retval = (tree_colon_expression *) NULL;
  if (op1 == NULL_TREE || op3 != NULL_TREE)
    error ("invalid colon expression");
  else
    {
      op3 = op2;	// Stupid syntax.
      op2 = t;

      retval = this;
    }
  return retval;
}

tree_constant
tree_colon_expression::eval (int print)
{
  tree_constant retval;

  if (error_state || op1 == NULL_TREE || op2 == NULL_TREE) 
    return retval;

  tree_constant tmp;

  tmp = op1->eval (0);

  if (tmp.is_undefined ())
    {
      eval_error ("invalid null value in colon expression");
      return retval;
    }

  tmp = tmp.make_numeric ();
  if (tmp.const_type () != tree_constant_rep::scalar_constant
      && tmp.const_type () != tree_constant_rep::complex_scalar_constant)
    {
      eval_error ("base for colon expression must be a scalar");
      return retval;
    }
  double base = tmp.double_value ();

  tmp = op2->eval (0);

  if (tmp.is_undefined ())
    {
      eval_error ("invalid null value in colon expression");
      return retval;
    }

  tmp = tmp.make_numeric ();
  if (tmp.const_type () != tree_constant_rep::scalar_constant
      && tmp.const_type () != tree_constant_rep::complex_scalar_constant)
    {
      eval_error ("limit for colon expression must be a scalar");
      return retval;
    }
  double limit = tmp.double_value ();

  double inc = 1.0;
  if (op3 != NULL_TREE)
    {
      tmp = op3->eval (0);

      if (tmp.is_undefined ())
	{
	  eval_error ("invalid null value in colon expression");
	  return retval;
	}

      tmp = tmp.make_numeric ();
      if (tmp.const_type () != tree_constant_rep::scalar_constant
	  && tmp.const_type () != tree_constant_rep::complex_scalar_constant)
	{
	  eval_error ("increment for colon expression must be a scalar");
	  return retval;
	}
      else
	inc = tmp.double_value ();
    }

  retval = tree_constant (base, limit, inc);

  if (error_state)
    {
      if (error_state)
	eval_error ("evaluating colon expression");
      return tree_constant ();
    }

  if (retval.is_defined ())
    return retval.eval (print);
  else
    return retval;
}

void
tree_colon_expression::eval_error (const char *s)
{
  if (error_state > 0)
    error ("%s near line %d column %d", s, line (), column ());
}

/*
 * Index expressions.
 */
tree_index_expression::tree_index_expression (int l = -1, int c = -1)
{
  id = (tree_identifier *) NULL;
  list = (tree_argument_list *) NULL;
  line_num = l;
  column_num = c;
}

tree_index_expression::tree_index_expression (tree_identifier *i,
					      tree_argument_list *lst,
					      int l = -1, int c = -1)
{
  id = i;
  list = lst;
  line_num = l;
  column_num = c;
}

tree_index_expression::tree_index_expression (tree_identifier *i,
					      int l = -1, int c = -1)
{
  id = i;
  list = (tree_argument_list *) NULL;
  line_num = l;
  column_num = c;
}

tree_index_expression::~tree_index_expression (void)
{
  delete id;
  delete list;
}

int
tree_index_expression::is_index_expression (void) const
{
  return 1;
}

tree_identifier *
tree_index_expression::ident (void)
{
  return id;
}

tree_argument_list *
tree_index_expression::arg_list (void)
{
  return list;
}

void
tree_index_expression::mark_for_possible_ans_assign (void)
{
  id->mark_for_possible_ans_assign ();
}


tree_constant
tree_index_expression::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (list == (tree_argument_list *) NULL)
    {
      retval = id->eval (print);
      if (error_state)
	eval_error ();
    }
  else
    {
// Extract the arguments into a simple vector.
      int nargin = 0;
      tree_constant *args = list->convert_to_const_vector (nargin);
// Don't pass null arguments.
      if (nargin > 1 && all_args_defined (args, nargin))
	{
	  tree_constant *tmp = id->eval (args, nargin, 1, print);

	  if (error_state)
	    eval_error ();

	  if (tmp != NULL_TREE_CONST)
	    retval = tmp[0];

	  delete [] tmp;
	}
      delete [] args;
    }
  return retval;
}

tree_constant *
tree_index_expression::eval (int print, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (error_state)
    return retval;

  if (list == (tree_argument_list *) NULL)
    {
      retval = id->eval (print, nargout);
      if (error_state)
	eval_error ();
    }
  else
    {
// Extract the arguments into a simple vector.
      int nargin = 0;
      tree_constant *args = list->convert_to_const_vector (nargin);
// Don't pass null arguments.
      if (nargin > 1 && all_args_defined (args, nargin))
	retval = id->eval (args, nargin, nargout, print);

      if (error_state)
	eval_error ();

      delete [] args;
    }
  return retval;
}

void
tree_index_expression::eval_error (void)
{
  if (error_state > 0)
    {
      int l = line ();
      int c = column ();
      char *fmt;
      if (l != -1 && c != -1)
	{
	  if (list != (tree_argument_list *) NULL)
	    fmt = "evaluating index expression near line %d, column %d";
	  else
	    fmt = "evaluating expression near line %d, column %d";

	  error (fmt, l, c);
	}
      else
	{
	  if (list != (tree_argument_list *) NULL)
	    error ("evaluating index expression");
	  else
	    error ("evaluating expression");
	}
    }
}

/*
 * Argument lists.
 */
tree_argument_list::tree_argument_list (void)
{
  arg = NULL_TREE;
  next = (tree_argument_list *) NULL;
}

tree_argument_list::tree_argument_list (tree *t)
{
  arg = t;
  next = (tree_argument_list *) NULL;
}

tree_argument_list::~tree_argument_list (void)
{
  delete arg;
  delete next;
}

tree_argument_list *
tree_argument_list::chain (tree *t)
{
  tree_argument_list *tmp = new tree_argument_list (t);
  tmp->next = this;
  return tmp;
}

tree_argument_list *
tree_argument_list::reverse (void)
{
  tree_argument_list *list = this;
  tree_argument_list *next;
  tree_argument_list *prev = (tree_argument_list *) NULL;

  while (list != (tree_argument_list *) NULL)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

int
tree_argument_list::length (void)
{
  tree_argument_list *list = this;
  int len = 0;
  while (list != (tree_argument_list *) NULL)
    {
      len++;
      list = list->next;
    }
  return len;
}

tree_argument_list *
tree_argument_list::next_elem (void)
{
  return next;
}

/*
 * Convert a linked list of trees to a vector of pointers to trees,
 * evaluating them along the way.
 */
tree_constant *
tree_argument_list::convert_to_const_vector (int& len)
{
  len = length () + 1;

  tree_constant *args = new tree_constant [len];

// args[0] may eventually hold something useful, like the function
// name.
  tree_argument_list *tmp_list = this;
  for (int k = 1; k < len; k++)
    {
      if (tmp_list != (tree_argument_list *) NULL)
	args[k] = tmp_list->eval (0);
      else
	args[k] = tree_constant ();
      tmp_list = tmp_list->next;
    }
  return args;
}

tree_constant
tree_argument_list::eval (int print)
{
  if (error_state || arg == NULL_TREE)
    return tree_constant ();
  else
    return arg->eval (print);
}

/*
 * Parameter lists.
 */
tree_parameter_list::tree_parameter_list (void)
{
  param = (tree_identifier *) NULL;
  next = (tree_parameter_list *) NULL;
}

tree_parameter_list::tree_parameter_list (tree_identifier *t)
{
  param = t;
  next = (tree_parameter_list *) NULL;
}

tree_parameter_list::~tree_parameter_list (void)
{
  delete param;
  delete next;
}

tree_parameter_list *
tree_parameter_list::chain (tree_identifier *t)
{
  tree_parameter_list *tmp = new tree_parameter_list (t);
  tmp->next = this;
  return tmp;
}

tree_parameter_list *
tree_parameter_list::reverse (void)
{
  tree_parameter_list *list = this;
  tree_parameter_list *next;
  tree_parameter_list *prev = (tree_parameter_list *) NULL;

  while (list != (tree_parameter_list *) NULL)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

int
tree_parameter_list::length (void)
{
  tree_parameter_list *list = this;
  int len = 0;
  while (list != (tree_parameter_list *) NULL)
    {
      len++;
      list = list->next;
    }
  return len;
}

char *
tree_parameter_list::name (void) const
{
  return param->name ();
}

void
tree_parameter_list::mark_as_formal_parameters (void)
{
  param->mark_as_formal_parameter ();
  if (next != (tree_parameter_list *) NULL)
    next->mark_as_formal_parameters ();
}

tree_identifier *
tree_parameter_list::define (tree_constant *t)
{
  return param->define (t);
}

tree_parameter_list *
tree_parameter_list::next_elem (void)
{
  return next;
}

tree_constant
tree_parameter_list::eval (int print)
{
  if (error_state || param == NULL_TREE)
    return tree_constant ();
  else
    return param->eval (print);
}

/*
 * Return lists.
 */
tree_return_list::tree_return_list (void)
{
  retval = (tree_index_expression *) NULL;
  next = (tree_return_list *) NULL;
}

tree_return_list::tree_return_list (tree_identifier *t)
{
  retval = new tree_index_expression (t);
  next = (tree_return_list *) NULL;
}

tree_return_list::tree_return_list (tree_index_expression *t)
{
  retval = t;
  next = (tree_return_list *) NULL;
}

tree_return_list::~tree_return_list (void)
{
  delete retval;
  delete next;
}

tree_return_list *
tree_return_list::chain (tree_identifier *t)
{
  tree_return_list *tmp = new tree_return_list (t);
  tmp->next = this;
  return tmp;
}

tree_return_list *
tree_return_list::chain (tree_index_expression *t)
{
  tree_return_list *tmp = new tree_return_list (t);
  tmp->next = this;
  return tmp;
}

tree_return_list *
tree_return_list::reverse (void)
{
  tree_return_list *list = this;
  tree_return_list *next;
  tree_return_list *prev = (tree_return_list *) NULL;

  while (list != (tree_return_list *) NULL)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

int
tree_return_list::length (void)
{
  tree_return_list *list = this;
  int len = 0;
  while (list != (tree_return_list *) NULL)
    {
      len++;
      list = list->next;
    }
  return len;
}

tree_index_expression *
tree_return_list::idx_expr (void)
{
  return retval;
}

tree_return_list *
tree_return_list::next_elem (void)
{
  return next;
}

tree_constant
tree_return_list::eval (int print)
{
  panic ("invalid evaluation of return list");
  return tree_constant ();
}

/*
 * Word lists.
 */
tree_word_list::tree_word_list (void)
{
  word = (char *) NULL;
  next = (tree_word_list *) NULL;
}

tree_word_list::tree_word_list (char *s)
{
  word = strsave (s);
  next = (tree_word_list *) NULL;
}

tree_word_list::~tree_word_list (void)
{
  delete [] word;
  delete next;
}

tree_word_list *
tree_word_list::chain (char *s)
{
  tree_word_list *tmp = new tree_word_list (s);
  tmp->next = this;
  return tmp;
}

tree_word_list *
tree_word_list::reverse (void)
{
  tree_word_list *list = this;
  tree_word_list *next;
  tree_word_list *prev = (tree_word_list *) NULL;

  while (list != (tree_word_list *) NULL)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

int
tree_word_list::length (void)
{
  tree_word_list *list = this;
  int len = 0;
  while (list != (tree_word_list *) NULL)
    {
      len++;
      list = list->next;
    }
  return len;
}

char *
tree_word_list::name (void) const
{
  return word;
}

tree_word_list *
tree_word_list::next_elem (void)
{
  return next;
}

tree_constant
tree_word_list::eval (int print)
{
  if (! error_state)
    error ("executing word_list_command");
  return tree_constant ();
}

/*
 * A command or two to be executed.
 */
tree_command_list::tree_command_list (void)
{
  command = NULL_TREE;
  print_flag = 1;
  next = (tree_command_list *) NULL;
}

tree_command_list::tree_command_list (tree *t)
{
  command = t;
  print_flag = 1;
  next = (tree_command_list *) NULL;
}

tree_command_list::~tree_command_list (void)
{
  delete command;
  delete next;
}

void
tree_command_list::set_print_flag (int flag)
{
  print_flag = flag;
}

tree_command_list *
tree_command_list::chain (tree *t)
{
  tree_command_list *tmp = new tree_command_list (t);
  tmp->next = this;
  return tmp;
}

tree_command_list *
tree_command_list::reverse (void)
{
  tree_command_list *list = this;
  tree_command_list *next;
  tree_command_list *prev = (tree_command_list *) NULL;

  while (list != (tree_command_list *) NULL)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

tree_constant
tree_command_list::eval (int print)
{
  int pf;
  tree_constant retval;

  if (error_state)
    return retval;

  tree_command_list *list;
  for (list = this; list != (tree_command_list *) NULL; list = list->next)
    {
      if (print == 0)
	pf = 0;
      else
	pf = list->print_flag;

      tree *cmd = list->command;
      if (cmd == NULL_TREE)
	retval = tree_constant ();
      else
	{
	  retval = cmd->eval (pf);

	  if (error_state)
	    return tree_constant ();

	  if (breaking || continuing)
	    break;

	  if (returning)
	    break;
	}
    }
  return retval;
}

/*
 * Global.
 */
tree_global_command::tree_global_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  sr = (symbol_record *) NULL;
  rhs = NULL_TREE;
  next = (tree_global_command *) NULL;
}

tree_global_command::tree_global_command (symbol_record *s,
					  int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  sr = s;
  rhs = NULL_TREE;
  next = (tree_global_command *) NULL;
}

tree_global_command::tree_global_command (symbol_record *s, tree *e,
					  int l = -1, int c = -1) 
{
  line_num = l;
  column_num = c;
  sr = s;
  rhs = e;
  next = (tree_global_command *) NULL;
}

tree_global_command::~tree_global_command (void)
{
  delete next;
}

tree_global_command *
tree_global_command::chain (symbol_record *s, int l = -1, int c = -1)
{
  tree_global_command *tmp = new tree_global_command (s, l, c);
  tmp->next = this;
  return tmp;
}

tree_global_command *
tree_global_command::chain (symbol_record *s, tree *e,
			    int l = -1, int c = -1)
{
  tree_global_command *tmp = new tree_global_command (s, e, l, c);
  tmp->next = this;
  return tmp;
}

tree_global_command *
tree_global_command::reverse (void)
{
  tree_global_command *list = this;
  tree_global_command *next;
  tree_global_command *prev = (tree_global_command *) NULL;

  while (list != (tree_global_command *) NULL)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

tree_constant
tree_global_command::eval (int print)
{
  tree_constant retval;

  link_to_global_variable (sr);

  if (rhs != NULL_TREE)
    {
      tree_identifier *id = new tree_identifier (sr);
      tree_constant tmp_rhs = rhs->eval (0);
      if (error_state)
	{
	  eval_error ();
	  return retval;
	}
      else
	{
	  tree_constant *tmp_val = new tree_constant (tmp_rhs);
	  tree_simple_assignment_expression tmp_ass (id, tmp_val);
	  tmp_ass.eval (0);
	  if (error_state)
	    {
	      eval_error ();
	      return retval;
	    }
	}
    }

  if (next != (tree_global_command *) NULL)
    next->eval (print);

  return retval;
}

void
tree_global_command::eval_error (void)
{
  if (error_state > 0)
    error ("evaluating global command near line %d, column %d",
	   line (), column ());
}

/*
 * While.
 */
tree_while_command::tree_while_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  expr = NULL_TREE;
  list = NULL_TREE;
}

tree_while_command::tree_while_command (tree *e, int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  expr = e;
  list = NULL_TREE;
}

tree_while_command::tree_while_command (tree *e, tree *lst,
					int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  expr = e;
  list = lst;
}

tree_while_command::~tree_while_command (void)
{
  delete expr;
  delete list;
}

tree_constant
tree_while_command::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  for (;;)
    {
      int expr_value = 0;
      if (expr == NULL_TREE)
	return tree_constant ();
      tree_constant t1 = expr->eval (0);

      if (error_state)
	{
	  eval_error ();
	  return tree_constant ();
	}

      if (t1.rows () == 0 || t1.columns () == 0)
	{
	  int flag = user_pref.propagate_empty_matrices;
	  if (flag < 0)
	    warning ("while: empty matrix used in conditional");
	  else if (flag == 0)
	    {
	      error ("while: empty matrix used in conditional");
	      return tree_constant ();
	    }
	  t1 = tree_constant (0.0);
	}
      else if (! t1.is_scalar_type ())
	{
	  tree_constant t2 = t1.all ();
	  t1 = t2.all ();
	}

      tree_constant_rep::constant_type t = t1.const_type ();
      if (t == tree_constant_rep::scalar_constant)
	expr_value = (int) t1.double_value ();
      else if (t == tree_constant_rep::complex_scalar_constant)
	expr_value = t1.complex_value () != 0.0;
      else
	panic_impossible ();

      if (expr_value)
	{
	  if (list != NULL_TREE)
	    {
	      retval = list->eval (1);
	      if (error_state)
		{
		  eval_error ();
		  return tree_constant ();
		}
	    }

	  if (returning)
	    break;

	  if (breaking)
	    {
	      breaking--;
	      break;
	    }

	  if (continuing)
	    {
	      continuing--;
	      if (continuing)  // Maybe handle `continue N' someday...
		break;
	    }
	}
      else
	break;
    }
  return retval;
}

void
tree_while_command::eval_error (void)
{
  if (error_state > 0)
    error ("evaluating while command near line %d, column %d",
	   line (), column ());
}

/*
 * For.
 */
tree_for_command::tree_for_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  id = (tree_index_expression *) NULL;
  expr = NULL_TREE;
  list = NULL_TREE;
}

tree_for_command::tree_for_command (tree_index_expression *ident,
				    tree *e, tree *lst,
				    int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  id = ident;
  expr = e;
  list = lst;
}

tree_for_command::~tree_for_command (void)
{
  delete id;
  delete expr;
  delete list;
}

tree_constant
tree_for_command::eval (int print)
{
  tree_constant retval;

  if (error_state || expr == NULL_TREE)
    return retval;

  tree_constant tmp_expr = expr->eval (0);

  if (error_state || tmp_expr.is_undefined ())
    {
      eval_error ();
      return retval;
    }

  tree_constant_rep::constant_type expr_type = tmp_expr.const_type ();
  switch (expr_type)
    {
    case tree_constant_rep::complex_scalar_constant:
    case tree_constant_rep::scalar_constant:
      {
	tree_constant *rhs = new tree_constant (tmp_expr);
	tree_simple_assignment_expression tmp_ass (id, rhs);
	tmp_ass.eval (0);
	if (error_state)
	  {
	    eval_error ();
	    return tree_constant ();
	  }

	if (list != NULL_TREE)
	  {
	    retval = list->eval (1);
	    if (error_state)
	      {
		eval_error ();
		return tree_constant ();
	      }
	  }
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
    case tree_constant_rep::matrix_constant:
      {
	Matrix m_tmp;
	ComplexMatrix cm_tmp;
	int nr;
	int steps;
	if (expr_type == tree_constant_rep::matrix_constant)
	  {
	    m_tmp = tmp_expr.matrix_value ();
	    nr = m_tmp.rows ();
	    steps = m_tmp.columns ();
	  }
	else
	  {
	    cm_tmp = tmp_expr.complex_matrix_value ();
	    nr = cm_tmp.rows ();
	    steps = cm_tmp.columns ();
	  }

	for (int i = 0; i < steps; i++)
	  {
	    tree_constant *rhs;

	    if (nr == 1)
	      {
		if (expr_type == tree_constant_rep::matrix_constant)
		  rhs = new tree_constant (m_tmp (0, i));
		else
		  rhs = new tree_constant (cm_tmp (0, i));
	      }
	    else
	      {
		if (expr_type == tree_constant_rep::matrix_constant)
		  rhs = new tree_constant (m_tmp.extract (0, i, nr-1, i));
		else
		  rhs = new tree_constant (cm_tmp.extract (0, i, nr-1, i));
	      }

	    tree_simple_assignment_expression tmp_ass (id, rhs);
	    tmp_ass.eval (0);
	    if (error_state)
	      {
		eval_error ();
		return tree_constant ();
	      }

	    if (list != NULL_TREE)
	      {
		retval = list->eval (1);
		if (error_state)
		  {
		    eval_error ();
		    return tree_constant ();
		  }
	      }

	    if (returning)
	      break;

	    if (breaking)
	      {
		breaking--;
		break;
	      }

	    if (continuing)
	      {
		continuing--;
		if (continuing)  // Maybe handle `continue N' someday...
		  break;
	      }
	  }
      }
      break;
    case tree_constant_rep::string_constant:
      gripe_string_invalid ();
      break;
    case tree_constant_rep::range_constant:
      {
	Range rng = tmp_expr.range_value ();

	int steps = rng.nelem ();
	double b = rng.base ();
	double increment = rng.inc ();

	for (int i = 0; i < steps; i++)
	  {
	    double tmp_val = b + i * increment;
	    tree_constant *rhs = new tree_constant (tmp_val);
	    tree_simple_assignment_expression tmp_ass (id, rhs);
	    tmp_ass.eval (0);
	    if (error_state)
	      {
		eval_error ();
		return tree_constant ();
	      }

	    if (list != NULL_TREE)
	      {
		retval = list->eval (1);
		if (error_state)
		  {
		    eval_error ();
		    return tree_constant ();
		  }
	      }

	    if (returning)
	      break;

	    if (breaking)
	      {
		breaking--;
		break;
	      }

	    if (continuing)
	      {
		continuing--;
		if (continuing)  // Maybe handle `continue N' someday...
		  break;
	      }
	  }
      }
      break;
    default:
      panic_impossible ();
      break;
    }
  
  return retval;
}

void
tree_for_command::eval_error (void)
{
  if (error_state > 0)
    error ("evaluating for command near line %d, column %d",
	   line (), column ());
}

/*
 * If.
 */
tree_if_command::tree_if_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  expr = NULL_TREE;
  list = NULL_TREE;
  next = (tree_if_command *) NULL;
}

tree_if_command::tree_if_command (tree *t, int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  expr = NULL_TREE;
  list = t;
  next = (tree_if_command *) NULL;
}

tree_if_command::tree_if_command (tree *e, tree *t, int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
  expr = e;
  list = t;
  next = (tree_if_command *) NULL;
}

tree_if_command::~tree_if_command (void)
{
  delete expr;
  delete list;
  delete next;
}

tree_if_command *
tree_if_command::chain (tree *t, int l = -1, int c = -1)
{
  tree_if_command *tmp = new tree_if_command (t, l, c);
  tmp->next = this;
  return tmp;
}

tree_if_command *
tree_if_command::chain (tree *t1, tree *t2, int l = -1, int c = -1)
{
  tree_if_command *tmp = new tree_if_command (t1, t2, l, c);
  tmp->next = this;
  return tmp;
}

tree_if_command *
tree_if_command::reverse (void)
{
  tree_if_command *list = this;
  tree_if_command *next;
  tree_if_command *prev = (tree_if_command *) NULL;

  while (list != (tree_if_command *) NULL)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

tree_constant
tree_if_command::eval (int print)
{
  int expr_value = 0;
  tree_constant retval;

  if (error_state)
    return retval;

  tree_if_command *lst;
  for (lst = this; lst != (tree_if_command *) NULL; lst = lst->next)
    {
      if (lst->expr != NULL_TREE)
	{
	  tree *tmp = lst->expr;
	  if (tmp == NULL_TREE)
	    return tree_constant ();
	  tree_constant t1 = tmp->eval (0);
	  if (error_state || t1.is_undefined ())
	    {
	      lst->eval_error ();
	      break;
	    }

	  if (t1.rows () == 0 || t1.columns () == 0)
	    {
	      int flag = user_pref.propagate_empty_matrices;
	      if (flag < 0)
		warning ("if: empty matrix used in conditional");
	      else if (flag == 0)
		{
		  error ("if: empty matrix used in conditional");
		  lst->eval_error ();
		  return tree_constant ();
		}
	      t1 = tree_constant (0.0);
	    }
	  else if (! t1.is_scalar_type ())
	    {
	      tree_constant t2 = t1.all ();
	      t1 = t2.all ();
	    }

	  tree_constant_rep::constant_type t = t1.const_type ();
	  if (t == tree_constant_rep::scalar_constant)
	    expr_value = (int) t1.double_value ();
	  else if (t == tree_constant_rep::complex_scalar_constant)
	    expr_value = t1.complex_value () != 0.0;
	  else
	    panic_impossible ();

	  if (expr_value)
	    {
	      if (lst->list != NULL_TREE)
		retval = lst->list->eval (1);
	      else
		error ("if: empty command list");

	      if (error_state)
		lst->eval_error ();

	      break;
	    }
	}
      else
	{
	  if (lst->list != NULL_TREE)
	    retval = lst->list->eval (1);
	  else
	    error ("if: empty command list");

	  if (error_state)
	    lst->eval_error ();

	  break;
	}
    }

  return retval;
}

void
tree_if_command::eval_error (void)
{
  if (error_state > 0)
    error ("evaluating if command near line %d, column %d",
	   line (), column ());
}

/*
 * Break.  Is this overkill, or what?
 */
tree_break_command::tree_break_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
}

tree_break_command::~tree_break_command (void)
{
}

tree_constant
tree_break_command::eval (int print)
{
  if (! error_state)
    breaking = 1;
  return tree_constant ();
}

/*
 * Continue.
 */
tree_continue_command::tree_continue_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
}

tree_continue_command::~tree_continue_command (void)
{
}

tree_constant
tree_continue_command::eval (int print)
{
  if (! error_state)
    continuing = 1;
  return tree_constant ();
}

/*
 * Return.
 */
tree_return_command::tree_return_command (int l = -1, int c = -1)
{
  line_num = l;
  column_num = c;
}

tree_return_command::~tree_return_command (void)
{
}

tree_constant
tree_return_command::eval (int print)
{
  if (! error_state)
    returning = 1;
  return tree_constant ();
}

/*
 * Functions that take a list of strings as arguments.
 */
tree_word_list_command::tree_word_list_command (void)
{
  ident = (tree_identifier *) NULL;
  word_list = (tree_word_list *) NULL;
}

tree_word_list_command::tree_word_list_command (tree_identifier *id,
						tree_word_list *wl)
{
  ident = id;
  word_list = wl;
}

tree_word_list_command::~tree_word_list_command (void)
{
  delete ident;
  delete word_list;
}

/*
 * Turn word list into char **argv with length = argc.
 */
tree_constant
tree_word_list_command::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  int argc = 1;
  if (word_list != (tree_word_list *) NULL)
    argc += word_list->length ();

  char **argv = new char * [argc];

  argv[0] = strsave (ident->name ());

  tree_word_list *wl = word_list;
  for (int i = 1; i < argc; i++)
    {
      char *s = wl->name ();
      argv[i] = strsave (s);
      wl = wl->next_elem ();
    }

  retval = ident->eval (argc, argv, print);

  delete [] argv;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
