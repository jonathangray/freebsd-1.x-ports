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
      integer line(80),endu(3),endl(3),name(20),blank,parn,istat,
     $          for(9),funl(8),funu(8),sl,su
c
      character*16 names,filen
      data endl/1he,1hn,1hd/,
     $     endu/1hE,1HN,1HD/,
     $     blank/1h /,
     $     parn/1h(/,
     $     name/1hu,1hs,1hr,1h:,1hm,1ha,1hi,1hn,1h.,1hf,
     $          1ho,1hr,1h ,1h ,1h ,1h ,1h ,1h ,1h ,1h /,
     $     for/1h.,1hf,1h ,1h ,1h ,1h ,1h ,1h ,1h /
      data funl/1hf,1hu,1hn,1hc,1ht,1hi,1ho,1hn/,
     $     funu/1HF,1HU,1HN,1HC,1HT,1HI,1HO,1HN/,
     $     sl/1hs/,
     $     su/1hS/
c
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
      read(9,40,end=999)(line(i),i=1,80)
   40 format(80a1)
c      write(6,41)(line(i),i=1,80)
c   41 format(' **',80a1)
      do 45 ib = 1,80
         j = 80 - ib + 1
         if( line(j) .ne. blank ) go to 46
   45 continue
   46 continue
      j = j + 1
      write(4,40)(line(i),i=1,j)
      IF( LINE(10) .NE. BLANK ) GO TO 30
      if( line(7) .ne. endl(1) .and. line(7) .ne. endu(1) ) go to 30
      if( line(8) .ne. endl(2) .and. line(8) .ne. endu(2) ) go to 30
      if( line(9) .ne. endl(3) .and. line(9) .ne. endu(3) ) go to 30
      close(unit=4)
 3000 continue
      read(9,40,end=999)(line(i),i=1,80)
c
c      check if subroutine
c
      if( line(7) .ne. sl .and. line(7) .ne. su ) go to 1111
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
            if( line(i) .eq. funl(k) .or. line(i) .eq. funu(k) ) 
     $            go to 374
  372    continue
c         write(6,373)(line(i),i=1,80)
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
         if( line(i) .eq. blank ) go to 60
         if( line(i) .eq. parn ) go to 60
         name(j) = line(i)
         ij = ij + 1
         names(ij:ij) = char(line(i))
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
         name(j) = for(i)
         j = j + 1
   70 continue
      write(6,88) names
   88 format(' processing ',a)
      close(unit=4)
      open(unit=4,file=names)
      rewind 4
      do 80 ib = 1,80
         j = 80 - ib + 1
         if( line(j) .ne. blank ) go to 85
   80 continue
   85 continue
      j = j + 1
      write(4,40)(line(i),i=1,j)
      go to 30
  999 continue
      write(6,1000)
 1000 format(' all done')
      stop
      end
