//
// hfstools - a Macintosh filesystem access tool
// (C) Copyright 1993 by Equivalence
//
// This file part of hfs.
//
// hfs is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
// 
// hfs is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with hfs; see the file COPYING.  If not, write to
// the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
//
//
// $Id: args.h,v 1.3 1994/05/15 06:35:55 rich Exp $
//

#ifndef _ARGS_H
#define _ARGS_H

#include <string.h>
#include <stdlib.h>
#include <iostream.h>

#ifdef _MSC_VER
#pragma warning(disable:4710) // inlines not expanded warning
#endif

class ArgList {
  private:
    int          arg_count;
    char      ** arg_values;

    char      * programName;
    char      * programPath;
    char      * argumentSpec;
    char      ** argumentList;
    int       * optionList;
    int       shift;
    
  public:
    ArgList            (int theArgc, char ** theArgv, const char *argumentSpec);
    ArgList            (char *arg_zero);
    virtual ~ArgList();
    void Parse (int theArgc, char ** theArgv, const char * theArgumentSpec);

    // return the program name
    inline char * GetProgname() const
      { return programName; }
    inline char * progname() const
      { return programName; }

    // return the program directory
    inline char * GetProgPath() const
      { return programPath; }
    inline char * progpath() const
      { return programPath; }

    // return True if the option was selected on the command line
    int OptionSpecified (const char option) const;
    inline int isopt (const char option) const
      { return OptionSpecified(option); }

    // return ptr to string argument if option if a string was specified, or NULL
    char * GetOption (const char option) const;
    inline char * opt (const char option) const 
      { return GetOption (option); }

    // return ptr to program argument, or NULL
    char * GetArgument (const int argnum) const;
    inline char * operator [] (const int argnum) const { return GetArgument (argnum); }

    // return number of arguments (not including options and strings)
    inline int GetArgCount () const
      { return arg_count-shift; }
    inline int argc() const
      { return arg_count-shift; }

    // shift arguments by specified amount
    inline void Shift (const int sh) 
      { if ((sh < 0) && (shift > 0)) shift -= sh;
        else if ((sh > 0) && (shift < arg_count)) shift += sh; }
    inline void operator << (const int sh) 
      { if (shift < arg_count) shift += sh; }
    inline void operator >> (const int sh) 
      { if (shift > 0) shift -= sh; }

    // called when access to illegal argument index made
    virtual void IllegalArgumentIndex (const int idx) const
      { cerr << programName << ": attempt to access undefined argument index " << idx << "\n"; }

    // called when unknown option encountered
    virtual void UnknownOption (const char option) const
      { cerr << programName << ": unknown option \"" << option << "\"\n"; exit (0); }

    // called when option argument missing
    virtual void MissingArgument (const char option) const
      { cerr << programName << ": option \"" << option << "\" requires argument\n"; }

  private:
    void ParseProgname(char * argv0);
};

#endif
