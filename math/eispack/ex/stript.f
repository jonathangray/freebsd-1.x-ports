c
c     this program will strip off subroutines and functions
c     from a sequencial file. 
c     this program will create files with the names of the 
c     subprograms as the file names. 
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
      integer istat
      character line*80,blank*1,parn*1,endu*3,endl*3
      character funl*8,funu*8,sl*1,su*1
      character name*20,for*9
c
      character*16 names,filen
c
      name = 'usr:main.f          '
      for = '.f       '
      sl = 's'
      su = 'S'
      funl = 'function'
      funu = 'FUNCTION'
      endl = 'end'
      endu = 'END'
      parn = '('
      blank = ' '
      write(6,6969)
 6969 format(' input the file name')
      read(5,6968) filen
 6968 format(a)
      open(unit=9,file=filen,iostat=istat)
      write(6,*)' file open name=',filen
      if( istat .ne. 0 ) write(6,10) istat
   10 format(' error from call to file on unit 9 ',i4)
      rewind 9
      go to 3000
   30 continue
      read(9,40,end=999)(line(i:i),i=1,80)
   40 format(80a1)
c      write(6,41)(line(i:i),i=1,80)
c   41 format(' **',80a1)
      do 45 ib = 1,80
         j = 80 - ib + 1
         if( line(j:j) .ne. blank ) go to 46
   45 continue
   46 continue
      j = j + 1
      write(4,40)(line(i:i),i=1,j)
      if( line(10:10) .ne. blank ) go to 30
      if( line(7:7) .ne. endl(1:1) .and. 
     $    line(7:7) .ne. endu(1:1) ) go to 30
      if( line(8:8) .ne. endl(2:2) .and. 
     $    line(8:8) .ne. endu(2:2) ) go to 30
      if( line(9:9) .ne. endl(3:3) .and. 
     $    line(9:9) .ne. endu(3:3) ) go to 30
      close(unit=4)
 3000 continue
      read(9,40,end=999)(line(i:i),i=1,80)
c
c      check if subroutine
c
      if( line(7:7) .ne. sl .and. line(7:7) .ne. su ) go to 1111
      i1 = 18
      i2 = 23
      go to 49
c
c     look for a function
c
 1111 continue
      iscan = 7
      do 374 k = 1,8
         last = 62 + k
         do 372 i = iscan,last
            iscan = i + 1
            if( line(i:i) .eq. funl(k:k) .or. line(i:i) .eq. funu(k:k) ) 
     $            go to 374
  372    continue
c         write(6,373)(line(i:i),i=1,80)
c  373    format(' *****error line is not a function or  sub. after end'/
c     $           1x,80a1)
         go to 3000
  374 continue
      i1 = iscan + 1
      i2 = iscan + 6
   49 continue
      j = 4
      ij = 0
      do 50 i = i1,i2
         j = j + 1
         if( line(i:i) .eq. blank ) go to 60
         if( line(i:i) .eq. parn ) go to 60
         name(j:j) = line(i:i)
         ij = ij + 1
         names(ij:ij) = line(i:i)
   50 continue
      j = j + 1
   60 continue
      i2 = j - 1
      names(ij+1:ij+2) = '.f'
      ij = ij + 3
      do 61 i = ij,16
         names(i:i) = ' '
   61 continue
      do 70 i = 1,9
         name(j:j) = for(i:i)
         j = j + 1
   70 continue
      write(6,88) names
   88 format(' processing ',a)
      close(unit=4)
      open(unit=4,file=names)
      rewind 4
      do 80 ib = 1,80
         j = 80 - ib + 1
         if( line(j:j) .ne. blank ) go to 85
   80 continue
   85 continue
      j = j + 1
      write(4,40)(line(i:i),i=1,j)
      go to 30
  999 continue
      write(6,1000)
 1000 format(' all done')
      stop
      end
