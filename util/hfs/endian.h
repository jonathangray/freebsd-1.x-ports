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
// $Id: endian.h,v 1.2 1994/05/15 06:19:19 rich Exp $
// $Log: endian.h,v $
// Revision 1.2  1994/05/15 06:19:19  rich
// hfs for FreeBSD.
//
// hfs allows files to be copied off a Macintosh HFS disk on a
// non-Macintosh computer.  Written by Craig Southeren,
// geoffw@extro.ucc.su.oz.au.
//
// Revision 1.1.1.1  1994/05/15  05:44:52  rich
// hfs 0.3 from sunsite
//
// Revision 1.3  1994/01/06  03:05:08  craigs
// Final checkin to include GNU header
//
// Revision 1.2  1993/11/24  21:36:05  craigs
// Various changes remove warnings under MSDOS/NT
//     by robertj
//
// Revision 1.1  1993/11/22  22:27:51  craigs
// Initial revision
//
//
//

/*
//  This file defines the primitive data types used to access
//  data structures on macintosh disks. As the 680x0 class of
//  of microprocssors is big-endian, we have to ensure that
//  we access data in this manner.
//  Also note that the internal storage of these structures is
//  ALWAYS in terms of "char". This is necessary so that that
//  compilers won't attempt to move the elements of the structures
//  out to word boundaries. Similiarly, none of the member functions
//  in these classes can be made virtual as this would imply a
//  vtable ptr, adding four bytes (probably) to the size of each
//  class
//
*/

#ifndef _ENDIAN_H

#define _ENDIAN_H

//////////////////////////////////////////////////////////
//
//  Base machine dependent types
//

typedef unsigned long ULONG;    // 32 bit unsigned
typedef          long LONG;   // 32 bit signed
typedef unsigned int  UINT;   // 16 bit unsigned
typedef          int  INT;    // 16 bit signed
typedef unsigned char BYTE;   // 8 bit unsigned
typedef          char SBYTE;    // 8 bit signed
typedef          int  BOOL;   // boolean value

//////////////////////////////////////////////////////////
//
//  Macintosh unsigned 32 bit (bigendian)
//

class MULONG {
  private:
    BYTE data[4];

  public:
#if (LITTLEENDIAN)
    ULONG Get () const {
      return (ULONG)(((ULONG)data[0] << 24) |
                     ((ULONG)data[1] << 16) |
                     ((ULONG)data[2] << 8) |
                      (ULONG)data[3]);
    }
    void Set (ULONG val) {
      data[0] = (BYTE)(val >> 24);
      data[1] = (BYTE)(val >> 16);
      data[2] = (BYTE)(val >>  8);
      data[3] = (BYTE) val;
    }
    BOOL operator == (const ULONG & val) const {
      return Get() == val;
    }
#else
    inline ULONG Get         () const          { return *((ULONG *)&data); }
    inline void  Set         (const ULONG val) { *((ULONG *)&data) = val;  }
    inline BOOL operator ==  (const ULONG val) { return *((ULONG *)&data) == val; }
#endif
    inline operator ULONG () const { return Get (); };
};

//////////////////////////////////////////////////////////
//
//  Macintosh signed 32 bit (bigendian)
//

class MLONG {
  private:
    unsigned char data[4];

  public:
#if (LITTLEENDIAN)
    LONG Get () const {
      return (LONG)(((ULONG)data[0] << 24) |
                    ((ULONG)data[1] << 16) |
                    ((ULONG)data[2] << 8) |
                     (ULONG)data[3]);
    }
    void Set (LONG val) {
      data[0] = (BYTE)(val >> 24);
      data[1] = (BYTE)(val >> 16);
      data[2] = (BYTE)(val >>  8);
      data[3] = (BYTE) val;
    }
    BOOL operator == (const LONG & val) const {
      return Get() == val;
    }
#else
    inline LONG Get          () const               { return *((LONG *)&data); }
    inline void Set          (const LONG val)       { *((LONG *)&data) = val;  }
    inline BOOL operator ==  (const LONG val) const { return *((LONG *)&data) == val; }
#endif
    inline operator LONG () const { return Get (); };
};

//////////////////////////////////////////////////////////
//
//  Macintosh unsigned 16 bit (bigendian)
//

class MUINT {
  private:
    unsigned char data[2];

  public:
#if (LITTLEENDIAN)
    inline UINT Get () const {
      return (((UINT)data[0] << 8) | (UINT)data[1]);
    };
    void Set (UINT val) {
      data[0] = (BYTE)(val >> 8);
      data[1] = (BYTE) val;
    }
    BOOL operator == (const UINT & val) const {
      return Get() == val;
    }
#else
    inline INT  Get () const                       { return *((UINT *)&data); }
    inline void Set (const UINT val)               { *((UINT *)&data) = val;  }
    inline BOOL operator == (const UINT val) const { return *((UINT *)&data) == val; }
#endif
    inline operator UINT () const { return Get (); };
};


//////////////////////////////////////////////////////////
//
//  Macintosh signed 16 bit (bigendian)
//

class MINT {
  private:
    unsigned char data[2];

  public:
#if (LITTLEENDIAN)
    INT Get () const {
      return (INT)(((UINT)data[0] << 8) | (UINT)data[1]);
    }
    void Set (INT val) {
      data[0] = (BYTE)(val >> 8);
      data[1] = (BYTE) val;
    }
    BOOL operator == (const INT & val) const {
      return Get() == val;
    }
#else
    INT Get () const                              { return *((INT *)&data); }
    void Set (const val)                          { *((INT *)&data) = val;  }
    inline BOOL operator == (const INT val) const { return *((INT *)&data) == val; }
#endif
    inline operator INT () const { return Get (); };
};


//////////////////////////////////////////////////////////
//
//  Macintosh unsigned/signed 8 bit
//

#define MSBYTE  SBYTE
#define MBYTE BYTE

#endif
