
c
c     this program will strip off initial program comments
c     from a sequencial file. 
c     uses fortran 77 (works fine on unix)
c
c     comments should be directed to:
c
c        jack dongarra
c        mathematics and computer science division
c        argonne national laboratory
c        argonne, illinois 60439
c
c        phone: 312-972-7246
c        arpanet: dongarra@anl-mcs
c
c
      character*16 names,filen
      character line*80,line2*80
      integer cc
c
c      write(6,6969)
c 6969 format(' input the file name')
c      read(5,6968) filen
c 6968 format(a)
c      open(unit=9,file=filen,iostat=istat)
cc      write(6,*)' file open name=',filen
c      if( istat .ne. 0 ) write(6,10) istat
c   10 format(' error from call to file on unit 9 ',i4)
c      rewind 9
   11 continue
      read(5,40,end=999)line
      call out(line)
      read(5,40,end=999)line
      call out(line)
      if( line(6:6) .eq. 'X' ) then
         read(5,40,end=999)line
         call out(line)
      endif
      read(5,40,end=999)line
      call out(line)
      if( line(1:1) .ne. 'C' ) then
   15    continue
         read(5,40,end=999)line
         call out(line)
         if( line(1:1) .eq. 'C' ) go to 30
         go to 15
      end if    
   30 continue
      read(5,40,end=999)line
      read(5,40,end=999)line2
   31 continue
         if( line2(1:1) .eq. 'C' ) then
            call out(line)
            line = line2
            read(5,40,end=999)line2
            go to 31
         end if
      write(6,40)'C'
   35 continue
         read(5,40,end=999)line
   40    format(a)
         if ( line(7:9) .eq. 'END' ) go to 11
      go to 35
  999 continue
      stop
      end
      subroutine out(line)
      character line*(*)
c
      i = 73
   10 continue
         i = i - 1
         if( line(i:i) .eq. ' ' ) go to 10
      write(6,40)line(1:i)
   40 format(a)
      return
      end
