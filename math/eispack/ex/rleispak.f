c
c     this driver tests  eispack  for the class of real matrices
c     exhibiting the use of  eispack  to find the singular values
c     and the solution to the equation  a*x = b .
c
c     this driver is catalogued as  eispdrv4(rleispak).
c
c     the dimension of  a,u,  and  v  should be  nm  by  nm.
c     the dimension of  sigma  and  rv1  should be  nm.
c     the dimension of  ahold  should be  nm  by  nm.
c     here  nm = 20.
c
      double precision a( 20, 20),u( 20, 20),v( 20, 20),
     x        sigma( 20),rv1( 20),ahold( 20, 20),machep,
     x        resdul,tcrit
      integer error
      data iwrite/6/
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
   10 call  rmatin(nm,nm,m,n,a,ahold,ahold,0,0)
      ip = m
      write(iwrite,20) m,n
   20 format(30h1the matrix  a , row dimension  ,i4,
     x         22h  and column dimension  ,i4,
     x         18h, printed by rows.)
      do 40 i = 1,m
         write(iwrite,30) (a(i,j),j=1,n) 
   30    format(/(1x,1p5d24.16))
   40 continue
      write(iwrite,50) m,ip
   50 format(///30h the matrix  b , row dimension  ,i4,
     x       22h  and column dimension  ,i4,
     x       25h, is the identity matrix.)
c
      do 230 icall = 1,2
         if( icall .eq. 1 ) go to 90
         call  rmatin(nm,nm,m,n,a,ahold,ahold,0,1)
         go to 100
c
c     rl  using  svd
c
   90    write(iwrite,91)
   91    format(//43h1all the singular values of the real matrix,
     x          44h follow.  the path involving  svd  was used. )
         call  svd(nm,m,n,a,sigma,.true.,u,.true.,v,error,rv1)
         write(iwrite,92) error
   92    format(//28h *****error from svd***** = ,i4)
         go to 200
c
c     rl  using  minfit
c
  100    if( ip .eq. 0 ) go to 230
         write(iwrite,101)
  101    format(44h1all the singular values of the real matrix ,
     x          46hfollow.  the path involving  minfit  was used.)
         do 103 i = 1,m
            do 102 j = 1,ip
               v(i,j) = 0.0d0
  102       continue
            v(i,i) = 1.0d0
  103    continue
         call  minfit(nm,m,n,a,sigma,ip,v,error,rv1)
         write(iwrite,104) error
  104    format(//31h *****error from minfit***** = ,i4)
         do 107 i = 1,m
            do 106 j = 1,n
               u(i,j) = v(j,i)
  106       continue
  107    continue
         do 109 i = 1,n
            do 108 j = 1,n
               v(i,j) = a(i,j)
  108       continue
  109    continue
  200    write(iwrite,201) (sigma(i),i=1,n)
  201    format(//26h the singular values of a /3(1pd24.16,5x))
         call  rmatin(nm,nm,m,n,a,ahold,ahold,0,1)
         call  rlres(nm,m,n,a,u,sigma,v,rv1,resdul)
         tcrit = resdul/(machep*dfloat(10*max0(m,n)))
         write(iwrite,202) tcrit,resdul
  202    format(///49h as a guide to evaluating the performance of the ,
     x      47h eispack  codes  svd  and  minfit, a number  x, /
     x      47h related to the  1-norm  of the residual matrix,
     x      44h for the singular value decomposition of  a, /
     x      48h may be normalized to the number  y  by dividing,
     x      48h by the product of the machine precision  machep /
     x      41h and the larger of the matrix dimensions.,
     x      36h  the  eispack  codes have performed /
     x      56h (well,satisfactorily,poorly) according as the ratio  y ,
     x      54h is (less than  1, less than  100, greater than  100)./
     x      23h for this run,  y  is =,1pd14.6,11h  with  x =,d14.6)
         write(iwrite,203) m,n
  203    format(///30h the matrix  u , row dimension ,i4,
     x          21h and column dimension ,i4,
     x          10h, follows. )
         do 205 i = 1,m
            write(iwrite,204) (u(i,j),j=1,n)
  204       format(/(1x,1p5d24.16))
  205    continue
         write(iwrite,206) n
  206    format(///25h the matrix  v , of order ,i4,
     x          10h, follows. )
         do 207 i = 1,n
            write(iwrite,204) (v(i,j),j=1,n)  
  207    continue
  230 continue
      go to 10
      end
