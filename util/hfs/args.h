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
// $Id: args.h,v 1.1 1994/05/15 05:44:51 rich Exp $
// $Log: args.h,v $
// Revision 1.1  1994/05/15 05:44:51  rich
// Initial revision
//
// Revision 1.5  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.4  1993/12/16  20:13:02  craigs
// Separated parsing of program name and options, and removed
// indirection on argc and argv
//
// Revision 1.3  1993/11/24  21:30:35  craigs
// Added pragma to disable warnings under MSDOS
//   by robertj
//
// Revision 1.2  1993/11/23  20:03:32  craigs
// Added MSDOS/Windows compatibility
//   by robertj
//
// Revision 1.1  1993/11/22  22:27:51  craigs
// Initial revision
//
//
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
