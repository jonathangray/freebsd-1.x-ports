c
c     this driver tests  eispack  for the class of real symmetric gener-
c     alized matrix systems exhibiting the use of  eispack  to find all
c     the eigenvalues and eigenvectors, only all the eigenvalues, some
c     of the eigenvalues and the corresponding eigenvectors, or only
c     some of the eigenvalues for the eigenproblem  a*b*x = (lambda)*x.
c
c     this driver is catalogued as  eispdrv4(rsgabpk).
c
c     the dimension of  a,b  and  z  should be  nm  by  nm.
c     the dimension of  w,norm,d,e,e2,ind,rv1,rv2,rv3,rv4,rv5,rv6,  and
c     dl  should be  nm.
c     the dimension of  ahold  and  bhold  should be  nm  by  nm.
c     here nm = 20.
c
      double precision a( 20, 20),z( 20, 20),w( 20),d( 20),e( 20),
     x        e2( 20),rv1( 20),rv2( 20),rv3( 20),rv4( 20),
     x        rv5( 20),rv6( 20),norm( 20),tcrit,machep,resdul,
     x        maxeig,maxdif,eigdif,u,lb,ub,eps1,b( 20, 20),dl( 20)
      double precision ahold( 20, 20),bhold( 20, 20) 
      integer  ind( 20),error
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
   10 call rmatin(nm,n,a,b,ahold,bhold,0)
      write(iwrite,20) n
   20 format(40h1the full symmetric matrix  a   of order,
     x       i4,22h  is (printed by rows)  )
      do  30  i = 1,n
   30    write(iwrite,40) (a(i,j),j=1,n) 
   40    format(/(1x,1p5d24.16))
      write(iwrite,41) n
   41 format(/////40h the full symmetric matrix  b   of order,
     x       i4,22h  is (printed by rows)   )
      do 42 i = 1,n
   42    write(iwrite,43) (b(i,j),j=1,n)
   43    format(/(1x,1p5d24.16))
      read(ireadc,50) mm,lb,ub,m11,no
   50 format(i4,2d24.16,2(4x,i4))
c
c     mm,lb,ub,m11, and  no  are read from sysin after the matrix is
c     generated.  mm,lb,  and  ub specify to  bisect  the maximum
c     number of eigenvalues and bounds for the interval which is to
c     be searched.  m11  and  no  specify to  tridib  the lower boundary
c     index and the number of desired eigenvalues.
c
      do  230  icall = 1,10
         if( icall .ne. 1 )  call  rmatin(nm,n,a,b,ahold,bhold,1)
c
c     if  tqlrat  path (label 80) is taken then  tql2  path (label 70)
c     must also be taken in order that the measure of performance be
c     meaningful.
c     if  imtql1  path (label 85) is taken then  imtql2  path (label 75)
c     must also be taken in order that the measure of performance be
c     meaningful.
c
         go to  (70,75,80,85,89,90,95,100,110,115),  icall
c
c     rsgabwz  using  tql2
c     invoked from driver subroutine  rsgab.
c
   70    write(iwrite,71)
   71    format(42h1all of the eigenvalues and corresponding ,
     x     49heigenvectors of the real symmetric system follow./
     x     36h the path involving  tql2  was used.  )
         call  rsgab(nm,n,a,b,w,1,z,e,e2,error)
         write(iwrite,715) error
  715    format(//30h *****error from rsgab***** = ,i4)
         if( error .eq. 7*n + 1 ) go to 230
         do 72 i = 1,n
            rv6(i) = w(i)
   72    continue
         m = n
         if( error .ne. 0 ) m = error - 1
         go to  130
c
c     rsgabwz  using  imtql2
c
   75    write(iwrite,76)
   76    format(42h1all of the eigenvalues and corresponding ,
     x     49heigenvectors of the real symmetric system follow./
     x     38h the path involving  imtql2  was used.  )
         call  reduc2(nm,n,a,b,dl,error)
         write(iwrite,765) error
  765    format(//31h *****error from reduc2***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  tred2(nm,n,a,w,e,z)
         call  imtql2(nm,n,w,e,z,error)
         write(iwrite,77) error
   77    format(//31h *****error from imtql2***** = ,i4)
         do 78 i = 1,n
            rv5(i) = w(i)
   78    continue
         m = n
         if( error .ne. 0 ) m = error - 1
         call  rebak(nm,n,b,dl,m,z)
         go to  130
c
c     rsgabw  using  tqlrat
c     invoked from driver subroutine  rsgab.
c
   80    write(iwrite,805)
  805    format(24h1all of the eigenvalues ,
     x     36hof the real symmetric system follow./
     x     38h the path involving  tqlrat  was used.  )
         call  rsgab(nm,n,a,b,w,0,a,e,e2,error)
         write(iwrite,806) error
  806    format(//30h *****error from rsgab***** = ,i4)
         if( error .eq. 7*n + 1 ) go to 230
         maxeig = 0.0d0
         maxdif = 0.0d0
         m = n
         if( error .ne. 0 ) m = error - 1
         if( m .eq. 0 ) go to 230
         do 81 i = 1,m
            if( dabs(w(i)) .gt. maxeig ) maxeig = dabs(w(i))
            u = dabs(rv6(i) - w(i))
            if( u .gt. maxdif ) maxdif = u
   81    continue
         if( maxeig .eq. 0.0d0 ) maxeig = 1.0d0
         eigdif = maxdif/(maxeig*machep*dfloat(10*n))
         write(iwrite,82) eigdif
   82    format(//49h comparison of the eigenvalues from  tqlrat  with,
     x          52h those from  tql2  gives the normalized difference  ,
     x          1pd16.8)
         go to  130
c
c     rsgabw  using  imtql1
c
   85    write(iwrite,855)
  855    format(24h1all of the eigenvalues ,
     x     36hof the real symmetric system follow./
     x     38h the path involving  imtql1  was used.  )
         call  reduc2(nm,n,a,b,dl,error)
         write(iwrite,856) error
  856    format(//31h *****error from reduc2***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  tred1(nm,n,a,w,e,e)
         call  imtql1(n,w,e,error)
         write(iwrite,857) error
  857    format(//31h *****error from imtql1***** = ,i4)
         maxeig = 0.0d0
         maxdif = 0.0d0
         m = n
         if( error .ne. 0 ) m = error - 1
         if( m .eq. 0 ) go to 230
         do 86 i = 1,m
            if( dabs(w(i)) .gt. maxeig ) maxeig = dabs(w(i))
            u = dabs(rv5(i) - w(i))
            if( u .gt. maxdif ) maxdif = u
   86    continue
         if( maxeig .eq. 0.0d0 ) maxeig = 1.0d0
         eigdif = maxdif/(maxeig*machep*dfloat(10*n))
         write(iwrite,87) eigdif
   87    format(//49h comparison of the eigenvalues from  imtql1  with,
     x          52h those from  imtql2  gives the normalized difference,
     x          1pd16.8)
         go to  130
c
c     rsgabw1z  ( usage here computes all the eigenvectors )
c
   89    write(iwrite,890)
  890    format(43h1some of the eigenvalues and corresponding ,
     x     49heigenvectors of the real symmetric system follow./
     x     38h the path involving  imtqlv  was used.  )
         call  reduc2(nm,n,a,b,dl,error)
         write(iwrite,893) error
  893    format(//31h *****error from reduc2***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  tred1(nm,n,a,d,e,e2)
         call  imtqlv(n,d,e,e2,w,ind,error,rv1)
         write(iwrite,895) error
  895    format(//31h *****error from imtqlv***** = ,i4)
         m = n
         if( error .ne. 0 ) m = error - 1
         call  tinvit(nm,n,d,e,e2,m,w,ind,z,error,rv1,rv2,rv3,rv4,rv6)
         write(iwrite,98) error
         call  trbak1(nm,n,a,e,m,z)
         call  rebak(nm,n,b,dl,m,z)
         go to 130
c
c     rsgab1w1z  using  tsturm
c
   90    write(iwrite,91)
   91    format(43h1some of the eigenvalues and corresponding ,
     x     49heigenvectors of the real symmetric system follow./
     x     38h the path involving  tsturm  was used.  )
         eps1 = 0.0d0
         call  reduc2(nm,n,a,b,dl,error)
         write(iwrite,915) error
  915    format(//31h *****error from reduc2***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  tred1(nm,n,a,d,e,e2)
         call  tsturm(nm,n,eps1,d,e,e2,lb,ub,mm,m,w,z,error,rv1,rv2,
     x                rv3,rv4,rv5,rv6)
         write(iwrite,92) error
   92    format(//31h *****error from tsturm***** = ,i4)
         if( error .eq. 3*n + 1 ) go to 230
         if( error .ne. 0 ) m = error - 4*n - 1
         call  trbak1(nm,n,a,e,m,z)
         call  rebak(nm,n,b,dl,m,z)
         go to  150
c
c     rsgab1w1z  using  bisect  and  tinvit
c
   95    write(iwrite,96)
   96    format(43h1some of the eigenvalues and corresponding ,
     x     49heigenvectors of the real symmetric system follow./
     x     38h the path involving  bisect  was used.  )
         eps1 = 0.0d0
         call  reduc2(nm,n,a,b,dl,error)
         write(iwrite,965) error
  965    format(//31h *****error from reduc2***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  tred1(nm,n,a,d,e,e2)
         call  bisect(n,eps1,d,e,e2,lb,ub,mm,m,w,ind,error,rv4,rv5)
         write(iwrite,97) error
   97    format(//31h *****error from bisect***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  tinvit(nm,n,d,e,e2,m,w,ind,z,error,rv1,rv2,rv3,rv4,rv6)
         write(iwrite,98) error
   98    format(//31h *****error from tinvit***** = ,i4)
         call  trbak1(nm,n,a,e,m,z)
         call  rebak(nm,n,b,dl,m,z)
         go to  150
c
c     rsgab1w  using  bisect
c
  100    write(iwrite,101)
  101    format(25h1some of the eigenvalues ,
     x     36hof the real symmetric system follow. )
         eps1 = 0.0d0
         call  reduc2(nm,n,a,b,dl,error)
         write(iwrite,1015)error
 1015    format(//31h *****error from reduc2***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  tred1(nm,n,a,d,e,e2)
         call  bisect(n,eps1,d,e,e2,lb,ub,mm,m,w,ind,error,rv4,rv5)
         write(iwrite,102) error
  102    format(//31h *****error from bisect***** = ,i4)
         go to  150
c
c     rsgab1w1z  using  tridib  and  tinvit
c
  110    write(iwrite,111)
  111    format(43h1some of the eigenvalues and corresponding ,
     x     49heigenvectors of the real symmetric system follow./
     x     38h the path involving  tridib  was used.  )
         eps1 = 0.0d0
         call  reduc2(nm,n,a,b,dl,error)
         write(iwrite,1115) error
 1115    format(//31h *****error from reduc2***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  tred1(nm,n,a,d,e,e2)
         call  tridib(n,eps1,d,e,e2,lb,ub,m11,no,w,ind,error,rv4,rv5)
         write(iwrite,112) error
  112    format(//31h *****error from tridib***** = ,i4)
         if( error .ne. 0 )  go to  230
         m = no
         write(iwrite,113) lb,ub
  113    format(//49h the eigenvalues as determined by  tridib  are in
     x          ,13h the interval, 1pd16.8,5h  to ,d16.8)
         call  tinvit(nm,n,d,e,e2,m,w,ind,z,error,rv1,rv2,rv3,rv4,rv6)
         write(iwrite,114) error
  114    format(//31h *****error from tinvit***** = ,i4)
         call  trbak1(nm,n,a,e,m,z)
         call  rebak(nm,n,b,dl,m,z)
         go to  150
c
c     rsgab1w  using  tridib
c
  115    write(iwrite,101)
         eps1 = 0.0d0
         call  reduc2(nm,n,a,b,dl,error)
         write(iwrite,1165) error
 1165    format(//31h *****error from reduc2***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  tred1(nm,n,a,d,e,e2)
         call  tridib(n,eps1,d,e,e2,lb,ub,m11,no,w,ind,error,rv4,rv5)
         write(iwrite,117) error
  117    format(//31h *****error from tridib***** = ,i4)
         m = no
         if( error .ne. 0 )  go to  230
         write(iwrite,113) lb,ub
         go to 150
c
  130    write(iwrite,140) (i,w(i),i=1,n) 
  140    format(///18h eigenvalues of  a/3(i3,2x,1pd24.16,3x))
         if( icall .eq. 3 .or. icall .eq. 4 )  go to  230
         go to  190
  150    write(iwrite,160) m,lb,ub
  160    format(///4h the ,i4,29h  eigenvalues of  a  between ,1pd24.16,
     x        6h  and ,d24.16)
         if( m .eq. 0 )  go to  230
         write(iwrite,163) (i,w(i),i=1,m)
  163    format(//3(i3,2x,1pd24.16,3x))
         if( icall .eq. 8 .or. icall .eq. 10 )  go to  230
  190    call  rmatin(nm,n,a,b,ahold,bhold,1)
         call  rsgabr(nm,n,m,a,b,w,z,norm,resdul,rv2)
         tcrit = resdul/(dfloat(10*n)*machep)
         write(iwrite,200) tcrit,resdul
  200    format(///48h as a guide to evaluating the performance of the,
     x     53h  eispack  codes, a number  x, related to the  1-norm /
     x     56h of the residual matrix, may be normalized to the number,
     x     56h  y  by dividing by the product of the machine precision/
     x     57h machep  and  10*n  where  n  is the order of the matrix.,
     x     36h  the  eispack  codes have performed /
     x     56h (well,satisfactorily,poorly) according as the ratio  y ,
     x     54h is (less than  1, less than  100, greater than  100)./
     x     23h for this run,  y  is =,1pd14.6,11h  with  x =,d14.6)
         do  220  kk = 1,m,3
            kk1 = min0(kk+2,m)
            irs = kk1-kk+1
            go to  (204,208,212),  irs
  204       write(iwrite,205) (w(k),k=kk,kk1),(norm(k),k=kk,kk1),
     x           ((z(i,k),k=kk,kk1),i=1,n)
  205    format(///1x,1(5x,24heigenvalue of a*b-lambda,3x)/
     x   1x,1(4x,1pd24.16,4x)//
     x   1x,1(5x,23h1-norm of corresponding,4x)/
     x   1x,1(9x,15hresidual vector,8x)/
     x   1x,1(7x,19h!!a*b*z-z*diag(w)!!,6x)/
     x   1x,1(5x,22h----------------------,5x)/
     x   1x,1(8x,17h!!a!!*!!b!!*!!z!!,7x)//
     x   1x,1(8x,d16.8,8x)//
     x   1x,1(4x,26hcorresponding eigenvector ,2x)/1(5x,d24.16,3x))
            go to 220
  208       write(iwrite,209) (w(k),k=kk,kk1),(norm(k),k=kk,kk1),
     x           ((z(i,k),k=kk,kk1),i=1,n)
  209    format(///1x,2(5x,24heigenvalue of a*b-lambda,3x)/
     x   1x,2(4x,1pd24.16,4x)//
     x   1x,2(5x,23h1-norm of corresponding,4x)/
     x   1x,2(9x,15hresidual vector,8x)/
     x   1x,2(7x,19h!!a*b*z-z*diag(w)!!,6x)/
     x   1x,2(5x,22h----------------------,5x)/
     x   1x,2(8x,17h!!a!!*!!b!!*!!z!!,7x)//
     x   1x,2(8x,d16.8,8x)//
     x   1x,2(4x,26hcorresponding eigenvector ,2x)/(5x,d24.16,3x,
     x   5x,d24.16,3x))
            go to 220
  212       write(iwrite,213) (w(k),k=kk,kk1),(norm(k),k=kk,kk1),
     x           ((z(i,k),k=kk,kk1),i=1,n)
  213    format(///1x,3(5x,24heigenvalue of a*b-lambda,3x)/
     x   1x,3(4x,1pd24.16,4x)//
     x   1x,3(5x,23h1-norm of corresponding,4x)/
     x   1x,3(9x,15hresidual vector,8x)/
     x   1x,3(7x,19h!!a*b*z-z*diag(w)!!,6x)/
     x   1x,3(5x,22h----------------------,5x)/
     x   1x,3(8x,17h!!a!!*!!b!!*!!z!!,7x)//
     x   1x,3(8x,d16.8,8x)//
     x   1x,3(4x,26hcorresponding eigenvector ,2x)/(5x,d24.16,3x,
     x   5x,d24.16,3x,5x,d24.16,3x))
  220    continue
  230 continue
      go to  10
      end
