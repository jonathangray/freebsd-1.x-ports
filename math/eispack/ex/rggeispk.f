c
c     this driver tests  eispack  for the class of real generalized
c     matrix systems exhibiting the use of  eispack  to find all
c     the eigenvalues and eigenvectors or only all the eigenvalues for
c     the eigenproblem   a*x = (lambda)*b*x .
c
c     this driver is catalogued as  eispdrv4(rggeispk).
c
c     the dimension of  a ,  b  and  z  should be  nm  by  nm.
c     the dimension of  alfr,alfi,beta,  and  norm  should be  nm.
c     the dimension of  ahold  and  bhold  should be  nm  by  nm.
c     here nm = 20.
c
      double precision a( 20, 20),b( 20, 20),z( 20, 20),
     x        alfr( 20),alfi( 20),beta( 20),norm( 20),
     x        machep,tcrit,resdul,eps1
      double precision ahold( 20, 20),bhold( 20, 20)
      integer  error
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
      open(unit=1,file='rggdata1')
      open(unit=2,file='rggdata2')
      rewind 1
      rewind 2
   10 call  rmatin(nm,n,a,b,ahold,bhold,0)
      write(iwrite,20) n
   20 format(30h1the full matrix  a   of order,
     x       i4,22h  is (printed by rows)  )
      do 30 i = 1,n
   30    write(iwrite,40) (a(i,j),j=1,n)
   40    format(/(1x,1p5d24.16))
      write(iwrite,41) n 
   41 format(/////30h the full matrix  b   of order,
     x       i4,22h  is (printed by rows)  )
      do 42 i = 1,n
   42    write(iwrite,43) (b(i,j),j=1,n)
   43    format(/(1x,1p5d24.16))
      do  510  icall = 1,2
         if( icall  .ne. 1 )  call   rmatin(nm,n,a,b,ahold,bhold,1)
         go to  (70,75), icall
c
c     rggwz  using  qzit  and  qzvec
c     invoked from driver subroutine  rgg.
c
   70    write(iwrite,71)
   71    format(42h1all of the eigenvalues and corresponding ,
     x     40heigenvectors of the real general system /
     x     34h follow.  the path involving  qzit,
     x     12h  was used. )
         eps1 = 0.0d0
         call  rgg(nm,n,a,b,alfr,alfi,beta,1,z,error)
         write(iwrite,72) error
   72    format(//28h *****error from qzit***** = ,i4)
         go to  130
c
c     rggw  using  qzit
c     invoked from driver subroutine  rgg.
c
   75    write(iwrite,76)
   76    format(24h1all of the eigenvalues ,
     x     26hof the real general system /
     x     46h follow.  the path involving  qzit  was used.  )
         eps1 = 0.0d0
         call  rgg(nm,n,a,b,alfr,alfi,beta,0,a,error)
         write(iwrite,77) error
   77    format(//28h *****error from qzit***** = ,i4)
c
  130    write(iwrite,291)
  291    format(//50h each eigenvalue of  a*x-l*b*x  can be found from ,
     x   59h alfr,  alfi,  and  beta  by dividing  beta  into  alfr  to/
     x   59h produce the real part and dividing  beta  into  alfi  to p,
     x   26hroduce the imaginary part.)
         write(iwrite,292)
  292    format(///14x,7halfr(i),20x,7halfi(i),20x,7hbeta(i))
         do 295 i = 1,n
            write(iwrite,293) i,alfr(i),alfi(i),beta(i)
  293       format(i4,3(1pd24.16,3x))
  295    continue
         if( icall .gt. 1 ) go to 510
         call   rmatin(nm,n,a,b,ahold,bhold,1)
         call   rggwzr(nm,n,a,b,alfr,alfi,beta,z,norm,resdul)
         tcrit = resdul/(dfloat(10*n)*machep)
         write(iwrite,300) tcrit,resdul
  300    format(///48h as a guide to evaluating the performance of the,
     x     55h  eispack  codes, a number  x, related to the  1-norm  /
     x     56h of the residual matrix, may be normalized to the number,
     x     56h  y  by dividing by the product of the machine precision/
     x     57h machep  and  10*n  where  n  is the order of the matrix.,
     x     36h  the  eispack  codes have performed /
     x     56h (well,satisfactorily,poorly) according as the ratio  y ,
     x     54h is (less than  1, less than  100, greater than  100)./
     x     23h for this run,  y  is =,1pd16.8,11h  with  x =,d16.8)
         nm2 = n-2
         k = 1
  310    if( k .gt. n )  go to  510
         if( alfi(k) .eq. 0.0d0 )  go to  360
         if( k .gt. nm2 )  go to  340
         if( alfi(k+2) .eq. 0.0d0 )  go to  330
c
c        both eigenvectors are complex.
c
         write(iwrite,320) alfr(k),alfi(k),beta(k),alfr(k+2),alfi(k+2),
     x     beta(k+2),norm(k),norm(k+2),(z(i,k),z(i,k+1),z(i,k+2),
     x     z(i,k+3),i=1,n)
  320    format(///1x,2(7x,4halfr,14x,4halfi,14x,4hbeta,7x)
     x       /1x,6(1pd16.8,2x)
     x       //1x,2(8x,39h1-norm of corresponding residual vector,7x)//
     x       1x,2(7x,21h!!beta*a*x-alfa*b*x!!,26x)/
     x       2x,2(33h---------------------------------,1x,1h=,d16.8,3x)/
     x       1x,2(2x,31h!!x!!*(beta*!!a!!+!alfa!*!!b!!),21x)//
     x       1x,2(14x,25hcorresponding eigenvector,15x)/
     x       (2d24.16,6x,2d24.16,6x))
         k = k+4
         go to  310
c
c        one complex eigenvector and one real eigenvector in that
c        order.
c
  330    write(iwrite,320)alfr(k+2),alfi(k+2),beta(k+2),alfr(k),alfi(k),
     x       beta(k),norm(k+2),norm(k),(z(i,k+2),alfi(k+2),z(i,k),
     x       z(i,k+1),i=1,n )
         k = k+3
         go to  310
c
c        one complex eigenvector.
c
  340    write(iwrite,350) alfr(k),alfi(k),beta(k),norm(k),(z(i,k),
     x               z(i,k+1),i=1,n)
  350    format(///8x,4halfr,14x,4halfi,14x,4hbeta,
     x       23x/1x,3(1pd16.8,2x)//
     x       1x,8x,39h1-norm of corresponding residual vector,7x//
     x       1x,7x,21h!!beta*a*x-alfa*b*x!!/
     x       2x,33(1h-),1x,1h=,d16.8,8x/
     x       1x,2x,31h!!x!!*(beta*!!a!!+!alfa!*!!b!!)//
     x       1x,14x,25hcorresponding eigenvector,15x/
     x       (2d24.16,6x))
         go to  510
  360    if( k .eq. n )  go to  380
         if( alfi(k+1) .eq. 0.0d0 )  go to  370
c
c        one real eigenvector and one complex eigenvector in that
c        order.
c
         write(iwrite,320) alfr(k),alfi(k),beta(k),alfr(k+1),alfi(k+1),
     x                beta(k+1),norm(k),norm(k+1),
     x                (z(i,k),alfi(k),z(i,k+1),z(i,k+2),i=1,n)
         k = k+3
         go to  310
c
c        both eigenvectors are real.
c
  370    write(iwrite,320) alfr(k),alfi(k),beta(k),alfr(k+1),alfi(k+1),
     x          beta(k+1),norm(k),norm(k+1),
     x                (z(i,k),alfi(k),z(i,k+1),alfi(k+1),i=1,n)
         k = k+2
         go to  310
c
c        one real eigenvector.
c
  380    write(iwrite,350) alfr(k),alfi(k),beta(k),norm(k),(z(i,k),
     x          alfi(k),i=1,n)
         go to  510
  510 continue
      go to  10
      end
