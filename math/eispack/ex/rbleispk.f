c
c     this driver tests  eispack  for the class of real
c     band matrices exhibiting the use of  eispack  to find the
c     solution to the equation  a*x = b .
c
c     this driver is catalogued as  eispdrv4(rbleispk).
c
c     the dimension of  a  and  ahold  should be  nm  by  mbb.
c     the dimension of  x,b,  and  bhold  should be  nm  by  nm.
c     the dimension of  resdul  should be  nm.
c     the dimension of  rv  should be  nm*mbb.
c     the dimension of  rv1  and  rv6  should be  nm.
c     here  nm = 20  and  mbb = 39.
c
c
      double precision a( 20, 39),x( 20, 20),b( 20, 20),rv( 780),
     x        resdul( 20),rv1( 20),rv6( 20),machep,det,tcrit,
     x        resmax,e21
      double precision ahold( 20, 39),bhold( 20, 20)
      integer error
      data ireadc/5/,iwrite/6/
c
c     .......... machep is a machine dependent parameter specifying
c                the relative precision of floating point arithmetic.
      machep = 1.0d0
    1 machep = 0.5d0*machep
      if (1.0d0 + machep .gt. 1.0d0 ) go to 1
      machep = 2.0d0*machep
c
c
      nm = 20
      mbb = 39
   10 call  rmatin(nm,mbb,n,mbw,a,ahold,bhold,0,0)
      call  rmatin(nm,nm,n,ip,x,ahold,bhold,1,0)
      read(ireadc,15) ie21
   15 format(i6)
      e21 = dfloat(ie21)
c
      if( e21 .eq. 1.0d0 ) go to 19
      write(iwrite,17) n,mbw
   17 format(53h1the highest superdiagonal to the lowest subdiagonal ,
     x       31hof the band matrix  a  of order ,i4,2x,
     x       19hand full band width,i4,4h  is )
      mbh = (mbw + 1)/2 - 1
      do 18 i = 1,mbh
         ibw = mbw - i + 1
         k = n - (mbh - i + 1)
         write(iwrite,30)(a(j,ibw),j=1,k)
   18 continue
      mbh = mbh + 1
      go to 23
   19 write(iwrite,20) n,mbw
   20 format(40h1the diagonal to the lowest subdiagonal ,
     x       41hof the band symmetric matrix  a  of order ,i4,2x,
     x       19hand half band width,i4,4h  is )
   22 mbh = mbw
   23 do 40 jj = 1,mbh
         j = mbh -jj + 1
         write(iwrite,30)(a(i,j),i=jj,n)
   30    format(/(1x,1p5d24.16))
   40 continue
      write(iwrite,50) n,ip
   50 format(///35h the matrix  b  with row dimension  ,i4,
     x       23h  and column dimension  ,i4,18h  printed by rows.)
      do 70 i = 1,n
         write(iwrite,60) (x(i,j),j=1,ip)
   60    format(/(1x,1p5d24.16))
   70 continue
c
c     rbl  using  bandv
c
      do 905 i = 1,n
         rv1(i) = 0.0d0
  905 continue
      call  bandv(nm,n,mbw,a,e21,ip,rv1,x,error,780,rv,rv6)
      write(iwrite,91) error
   91 format(//30h *****error from bandv***** = ,i4)
      det = 1.0d0
      do 92 i = 1,n
         det = dabs(rv(i))*det
   92 continue
      write(iwrite,93) det
   93 format(//46h the unsigned determinant of the matrix  a  = ,
     x       1pd24.16)
      call  rmatin(nm,mbb,n,mbw,a,ahold,bhold,0,1)
      call  rmatin(nm,nm,n,ip,b,ahold,bhold,1,1)
      call  rblres(nm,n,mbw,ip,a,b,x,resdul,e21)
      resmax = 0.0d0
      do 935 i = 1,ip
         if( resdul(i) .gt. resmax ) resmax = resdul(i)
  935 continue
      tcrit = resmax/(machep*dfloat(n))
      write(iwrite,94) tcrit,resmax
   94 format(///49h as a guide to evaluating the performance of the ,
     x   47h eispack  code  bandv  for the solution of band /
     x   46h linear equations, a number  x, defined as the,
     x   47h maximum of a set of numbers  r  related to the /
     x   43h 1-norms  of the separate residual vectors, ,
     x   51h may be normalized to the number  y  by dividing by /
     x   46h the product of the machine precision  machep ,
     x   51h and the order of the matrix.  bandv  has performed /
     x   56h (well,satisfactorily,poorly) according as the ratio  y ,
     x   54h is (less than  1, less than  100, greater than  100)./
     x   23h for this run,  y  is =,1pd14.6,11h  with  x =,d14.6)
      do 96 i = 1,ip
         write(iwrite,945) i,(x(j,i),j=1,n) 
  945    format(///44h the solution vector corresponding to column ,
     x          i3,15h of  b  follows /(1x,1p5d24.16))
         write(iwrite,95) resdul(i)
   95    format(//21h for this vector r = ,1pd14.6)
   96 continue
      go to 10
      end
