c
c     this driver tests  eispack  for the class of real symmetric tri-
c     diagonal matrices exhibiting the use of  eispack  to find all the
c     eigenvalues and eigenvectors, only all the eigenvalues, some of
c     the eigenvalues and the corresponding eigenvectors, or only some
c     of the eigenvalues.
c
c     this driver is catalogued as  eispdrv4(rsteispk).
c
c     the dimension of  st  should be  nm  by  2.
c     the dimension of  z  should be  nm  by  nm.
c     the dimension of  w,norm,e,e2,ind,rv1,rv2,rv3,rv4,rv5,  and  rv6
c     should be  nm.
c     the dimension of  sthold  should be  nm  by  2.
c     here nm = 20.
c
      double precision z(20,20),st(20,2),sthold(20,2),w(20),e(20),
     x        e2(20),rv1( 20),rv2( 20),rv3( 20),rv4( 20),rv5( 20),  
     x        rv6( 20),norm( 20),tcrit,machep,resdul,maxeig,
     x        maxdif,eigdif,u,lb,ub,eps1
      integer  ind( 20),error
      data ireadc/5/,iwrite/6/
c
c     .......... machep is a machine dependent parameter specifying
c                the relative precision of floating point arithmetic.
c
      machep = 1.0d0
    1 machep = 0.5d0*machep
      if (1.0d0 + machep .gt. 1.0d0 ) go to 1
      machep = 2.0d0*machep
c
      nm = 20
      mmb = 2
   10 call rmatin(nm,mmb,n,mb,st,sthold,0)
      write(iwrite,20) n
   20 format(33h1the subdiagonal and diagonal in ,
     x   49hthat order of the tridiagonal symmetric matrix  a,
     x   10h  of order,i4,4h  is)
      do  30  jj = 1,2
         j = 3-jj
   30    write(iwrite,40) (st(i,jj),i=j,n)
   40    format(/(1x,1p5d24.16))
      read(ireadc,50) mm,lb,ub,m11,no 
   50 format(i4,2d24.16,2(4x,i4))
c
c     mm,lb,ub,m11,  and  no  are read from sysin after the matrix is
c     generated.  mm,lb,  and  ub  specify to  bisect  the maximum
c     number of eigenvalues and bounds for the interval which is to
c     be searched. m11  and  no  specify to  tridib  the lower boundary
c     index and the number of desired eigenvalues.
c
      do  230  icall = 1,10
         if( icall .ne. 1 )  call  rmatin(nm,mmb,n,mb,st,sthold,1)
c
c
c     if  tql1  path (label 80) is taken then  tql2  path (label 70)
c     must also be taken in order that the measure of performance be
c     meaningful.
c     if  imtql1  path (label 85) is taken then  imtql2  path (label 75)
c     must also be taken in order that the measure of performance be
c     meaningful.
c
         go to  (70,75,80,85,89,90,95,100,110,115),  icall
c
c     rstwz  using  tql2
c
   70    write(iwrite,705)
  705    format(42h1all of the eigenvalues and corresponding ,
     x     53heigenvectors of the real symmetric tridiagonal matrix /
     x     45h follow.  the path involving  tql2  was used.  )
         do  72  i = 1,n
            do  71  j = 1,n
   71         z(i,j) = 0.0d0
            z(i,i) = 1.0d0
            e(i) = st(i,1)
   72       w(i) = st(i,2)
         call  tql2(nm,n,w,e,z,error)
         write(iwrite,725) error
  725    format(//29h *****error from tql2***** = ,i4)
         do 73 i = 1,n
            rv6(i) = w(i)
   73    continue
         m = n
         if( error .ne. 0 ) m = error - 1
         go to  130
c
c     rstwz  using  imtql2
c     invoked from driver subroutine  rst.
c
   75    write(iwrite,755)
  755    format(42h1all of the eigenvalues and corresponding ,
     x     53heigenvectors of the real symmetric tridiagonal matrix /
     x     47h follow.  the path involving  imtql2  was used.  )
         do  77  i = 1,n
            e(i) = st(i,1)
   77       w(i) = st(i,2)
         call  rst(nm,n,w,e,1,z,error)
         write(iwrite,775) error
  775    format(//31h *****error from imtql2***** = ,i4)
         do 78 i = 1,n
            rv5(i) = w(i)
   78    continue
         m = n
         if( error .ne. 0 ) m = error - 1
         go to  130
c
c     rstw  using  tql1
c
   80    write(iwrite,805)
  805    format(24h1all of the eigenvalues ,
     x     48hof the real symmetric tridiagonal matrix follow. /
     x     36h the path involving  tql1  was used.  )
         do  81  i = 1,n
             e(i) = st(i,1)
   81        w(i) = st(i,2)
         call  tql1(n,w,e,error)
         write(iwrite,815) error
  815    format(//29h *****error from tql1***** = ,i4)
         maxeig = 0.0d0
         maxdif = 0.0d0
         m = n
         if( error .ne. 0 ) m = error - 1
         if( m .eq. 0 ) go to 230
         do 82 i = 1,m
            if( dabs(w(i)) .gt. maxeig ) maxeig = dabs(w(i))
            u = dabs(rv6(i) - w(i))
            if( u .gt. maxdif ) maxdif = u
   82    continue
         if( maxeig .eq. 0.0d0 ) maxeig = 1.0d0
         eigdif = maxdif/(maxeig*machep*dfloat(10*n))
         write(iwrite,83) eigdif
   83    format(//47h comparison of the eigenvalues from  tql1  with,
     x          52h those from  tql2  gives the normalized difference  ,
     x          1pd16.8)
         go to 130
c
c     rstw  using  imtql1
c     invoked from driver subroutine  rst.
c
   85    write(iwrite,855)
  855    format(24h1all of the eigenvalues ,
     x     48hof the real symmetric tridiagonal matrix follow. /
     x     38h the path involving  imtql1  was used.  )
         do  86  i = 1,n
   86       w(i) = st(i,2)
         call  rst(nm,n,w,st(1,1),0,z,error)
         write(iwrite,865) error
  865    format(//31h *****error from imtql1***** = ,i4)
         maxeig = 0.0d0
         maxdif = 0.0d0
         m = n
         if( error .ne. 0 ) m = error - 1
         if( m .eq. 0 ) go to 230
         do 87 i = 1,m
            if( dabs(w(i)) .gt. maxeig ) maxeig = dabs(w(i))
            u = dabs(rv5(i) - w(i))
            if( u .gt. maxdif ) maxdif = u
   87    continue
         if( maxeig .eq. 0.0d0 ) maxeig = 1.0d0
         eigdif = maxdif/(maxeig*machep*dfloat(10*n))
         write(iwrite,88) eigdif
   88    format(//49h comparison of the eigenvalues from  imtql1  with,
     x          52h those from  imtql2  gives the normalized difference,
     x          1pd16.8)
         go to  130
c
c     rstw1z  ( usage here computes all the eigenvectors )
c
   89    write(iwrite,891)
  891    format(43h1some of the eigenvalues and corresponding ,
     x     53heigenvectors of the real symmetric tridiagonal matrix /
     x     47h follow.  the path involving  imtqlv  was used.  )
         do 892 i = 1,n
  892       e2(i) = st(i,1) ** 2
         call imtqlv(n,st(1,2),st(1,1),e2,w,ind,error,rv1)
         write(iwrite,893) error
  893    format(//31h *****error from imtqlv***** = ,i4)
         m = n
         if( error .ne. 0 ) m = error - 1
         call tinvit(nm,n,st(1,2),st(1,1),e2,m,w,ind,z,error,rv1,rv2,
     x               rv3,rv4,rv6)
         write(iwrite,99) error
         go to 130
c
c     rst1w1z  using  tsturm
c
   90    write(iwrite,91)
   91    format(43h1some of the eigenvalues and corresponding ,
     x     53heigenvectors of the real symmetric tridiagonal matrix /
     x     47h follow.  the path involving  tsturm  was used.  )
         eps1 = 0.0d0
         do  92  i = 2,n
   92       e2(i) = st(i,1)**2
         call  tsturm(nm,n,eps1,st(1,2),st(1,1),e2,lb,ub,mm,m,w,z,
     x                error,rv1,rv2,rv3,rv4,rv5,rv6)
         write(iwrite,93) error
   93    format(//31h *****error from tsturm***** = ,i4)
         if( error .eq. 3*n + 1 ) go to 230
         if( error .gt. 4*n ) m = error - 4*n - 1
         go to  150
c
c     rst1w1z  using  bisect  and  tinvit
c
   95    write(iwrite,96)
   96    format(43h1some of the eigenvalues and corresponding ,
     x     53heigenvectors of the real symmetric tridiagonal matrix /
     x     47h follow.  the path involving  bisect  was used.  )
         eps1 = 0.0d0
         do  97  i = 2,n
   97       e2(i) = st(i,1)**2
         call  bisect(n,eps1,st(1,2),st(1,1),e2,lb,ub,mm,m,w,ind,error,
     x                rv4,rv5)
         write(iwrite,98) error
   98    format(//31h *****error from bisect***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  tinvit(nm,n,st(1,2),st(1,1),e2,m,w,ind,z,
     x                error,rv1,rv2,rv3,rv4,rv6)
         write(iwrite,99) error
   99    format(//31h *****error from tinvit***** = ,i4)
         go to  150
c
c     rst1w  using  bisect
c
  100    write(iwrite,101)
  101    format(25h1some of the eigenvalues ,
     x     48hof the real symmetric tridiagonal matrix follow.)
         eps1 = 0.0d0
         do  102  i = 2,n
  102       e2(i) = st(i,1)**2
         call  bisect(n,eps1,st(1,2),st(1,1),e2,lb,ub,mm,m,w,ind,error,
     x                rv4,rv5)
         write(iwrite,103) error
  103    format(//31h *****error from bisect***** = ,i4)
         go to  150
c
c     rst1w1z  using  ratqr  and  tinvit
c
  110    write(iwrite,111)
  111    format(43h1some of the eigenvalues and corresponding ,
     x     53heigenvectors of the real symmetric tridiagonal matrix /
     x     46h follow.  the path involving  ratqr  was used.  )
         eps1 = 0.0d0
         do 112  i = 2,n
  112       e2(i) = st(i,1)**2
         call  ratqr(n,eps1,st(1,2),st(1,1),e2,no+m11-1,w,
     x               ind,rv1,.true.,0,error)
         do 1125 i = 1,no
            m = i + m11 - 1
            w(i) = w(m)
            ind(i) = ind(m)
 1125    continue
         write(iwrite,113) error
  113    format(//30h *****error from ratqr***** = ,i4)
         if( error .ne. 0 )  go to  230
         m = no
         call  tinvit(nm,n,st(1,2),st(1,1),e2,m,w,ind,z,error,
     x                rv1,rv2,rv3,rv4,rv6)
         write(iwrite,114) error
  114    format(//31h *****error from tinvit***** = ,i4)
         go to  150
c
c     rst1w  using  tridib
c
  115    write(iwrite,101)
         eps1 = 0.0d0
         do 117  i = 2,n
  117       e2(i) = st(i,1)**2
         call  tridib(n,eps1,st(1,2),st(1,1),e2,lb,ub,m11,no,w,ind,
     x                error,rv4,rv5)
         write(iwrite,118) error
  118    format(//31h *****error from tridib***** = ,i4)
         m = no
         go to 150
c
  130    write(iwrite,140) (i,w(i),i=1,n)
  140    format(///18h eigenvalues of  a/3(i4,2x,1pd24.16,3x))
         if( icall .eq. 3 .or. icall .eq. 4 )  go to  230
         go to  190
  150    write(iwrite,160) m,lb,ub
  160    format(///4h the ,i4,29h  eigenvalues of  a  between ,1pd24.16,
     x        6h  and ,d24.16)
         if( m .eq. 0 )  go to  230
         write(iwrite,163) (i,w(i),i=1,m)
  163    format(//3(i3,2x,1pd24.16,3x))
         if( icall .eq. 8 .or. icall .eq. 10 ) go to 230
  190    call  rsbwzr(nm,n,m,2,st,w,z,norm,resdul)
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
  205    format(///1x,1(9x,15heigenvalue of a,8x)/1x,1(4x,1pd24.16,4x)//
     x   1x,1(5x,23h1-norm of corresponding,4x)/
     x   1x,1(9x,15hresidual vector,8x)/
     x   1x,1(4x,24h!!ax-l*x!!/(!!x!!*!!a!!),4x)/
     x   1x,1(8x,d16.8,8x)//
     x   1x,1(4x,26hcorresponding eigenvector ,2x)/1(5x,d24.16,3x))
            go to 220
  208       write(iwrite,209) (w(k),k=kk,kk1),(norm(k),k=kk,kk1), 
     x           ((z(i,k),k=kk,kk1),i=1,n)
  209    format(///1x,2(9x,15heigenvalue of a,8x)/1x,2(4x,1pd24.16,4x)//
     x   1x,2(5x,23h1-norm of corresponding,4x)/
     x   1x,2(9x,15hresidual vector,8x)/
     x   1x,2(4x,24h!!ax-l*x!!/(!!x!!*!!a!!),4x)/
     x   1x,2(8x,d16.8,8x)//
     x   1x,2(4x,26hcorresponding eigenvector ,2x)/(5x,d24.16,3x,
     x   5x,d24.16,3x))
            go to 220
  212       write(iwrite,213) (w(k),k=kk,kk1),(norm(k),k=kk,kk1),
     x           ((z(i,k),k=kk,kk1),i=1,n)
  213    format(///1x,3(9x,15heigenvalue of a,8x)/1x,3(4x,1pd24.16,4x)//
     x   1x,3(5x,23h1-norm of corresponding,4x)/
     x   1x,3(9x,15hresidual vector,8x)/
     x   1x,3(4x,24h!!ax-l*x!!/(!!x!!*!!a!!),4x)/
     x   1x,3(8x,d16.8,8x)//
     x   1x,3(4x,26hcorresponding eigenvector ,2x)/(5x,d24.16,3x,
     x   5x,d24.16,3x,5x,d24.16,3x))
  220    continue
  230 continue
      go to  10
      end
