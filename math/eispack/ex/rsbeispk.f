c
c     this driver tests  eispack  for the class of real symmetric
c     band matrices exhibiting the use of  eispack  to find all the
c     eigenvalues and eigenvectors, only all the eigenvalues, some of
c     the eigenvalues and the corresponding eigenvectors, or only some
c     of the eigenvalues.
c
c     this driver is catalogued as  eispdrv4(rsbeispk).
c
c     the dimension of  st  and  aa  should be  nm  by  mmb.
c     the dimension of  z  should be  nm  by  nm.
c     the dimension of  w,d,e,e2,rv4,rv5,rv6,norm, and
c     ind  should be  nm.
c     the dimension of  sthold  should be  nm  by  mmb.
c     the dimension of  rv1  should be  2*mmb**2+4*mmb-3 .
c     the dimension of  rv2  should be  nm*(2*mmb-1) .
c     here nm = 44  and  mmb = 5.
c
      double precision st( 44, 5),z( 44, 44),aa( 44, 5),
     x        sthold( 44, 5),w( 44),d( 44),e( 44),e2( 44),
     x        rv1( 67),rv2( 396),rv4( 44),rv5( 44),rv6( 44),
     x        norm( 44),tcrit,machep,resdul,maxeig,maxdif,eigdif,u,lb,
     x        ub,eps1,t,r,tstart
      integer  ind( 44),error,mb
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
      nm = 44
      mmb = 5
      nv1 = 67
      nv2 = 396
   10 call rmatin(nm,mmb,n,mb,st,sthold,0)
      write(iwrite,20) n,mb
   20 format(40h1the lowest subdiagonal to the diagonal ,
     x   41hof the symmetric band matrix  a  of order,i4,2x,
     x   19hand half band width,i4,4h  is )
      do  30  jj = 1,mb
         j = mb - jj + 1
   30    write(iwrite,40) (st(i,jj),i=j,n)
   40    format(/(1x,1p5d24.16))
      read(ireadc,50) mm,lb,ub,m11,no,mbqr,tstart
   50 format(i4,2d24.16,3i4,f8.4)
c
c     mm,lb,ub,m11,no,mbqr,  and  tstart  are read from sysin after the
c     matrix is generated.  mm,lb,  and  ub  specify to  bisect  the
c     maximum number of eigenvalues and the bounds for the interval
c     which is to be searched.  m11  and  no  specify to  tridib  the
c     boundary indices for the desired eigenvalues.
c     mbqr  and  tstart  specify to  bqr  the number of eigenvalues
c     and the value to which they are desired closest.
c
      do  230  icall = 1,10
         if( icall .ne. 1 )  call  rmatin(nm,mmb,n,mb,st,sthold,1)
c
c     if  tqlrat  path (label 80) is taken then  tql2  path (label 70)
c     must also be taken in order that the measure of performance be
c     meaningful.
c     if  imtql1  path (label 85) is taken then  imtql2  path (label 75)
c     must also be taken in order that the measure of performance be
c     meaningful.
c     if  tql2  (imtql2)  path fails, then  tqlrat  (imtql1)  path is
c     omitted and printout flagged with  -1.0.
c
         go to  (70,75,80,85,89,120,100,125,115,130),  icall
c
c     rsbwz  using  tql2
c     invoked from driver subroutine  rsb.
c
   70    write(iwrite,71)
   71    format(42h1all of the eigenvalues and corresponding ,
     x     46heigenvectors of the real symmetric band matrix /
     x     45h follow.  the path involving  tql2  was used.  )
         call  rsb(nm,n,mb,st,w,1,z,e,e,error)
         write(iwrite,72) error
   72    format(//29h *****error from tql2***** = ,i4)
         do 73 i = 1,n
            rv6(i) = w(i)
   73    continue
         m = n
         if( error .ne. 0 ) m = error - 1
         go to  140
c
c     rsbwz  using  imtql2
c
   75    write(iwrite,76)
   76    format(42h1all of the eigenvalues and corresponding ,
     x     46heigenvectors of the real symmetric band matrix /
     x     47h follow.  the path involving  imtql2  was used.  )
         call  bandr(nm,n,mb,st,w,e,e,.true.,z)
         call  imtql2(nm,n,w,e,z,error)
         write(iwrite,79) error
   79    format(//31h *****error from imtql2***** = ,i4)
         do 78 i= 1,n
            rv5(i) = w(i)
   78    continue
         m = n
         if( error .ne. 0 ) m = error - 1
         go to  140
c
c     rsbw  using  tqlrat
c     invoked from driver subroutine  rsb.
c
   80    write(iwrite,805)
  805    format(24h1all of the eigenvalues ,
     x     41hof the real symmetric band matrix follow. /
     x     38h the path involving  tqlrat  was used.  )
         call  rsb(nm,n,mb,st,w,0,z,e,e2,error)
         write(iwrite,807) error
  807    format(//31h *****error from tqlrat***** = ,i4)
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
         go to  140
c
c     rsbw  using  imtql1
c
   85    write(iwrite,855)
  855    format(24h1all of the eigenvalues ,
     x     41hof the real symmetric band matrix follow. /
     x     38h the path involving  imtql1  was used.  )
         call  bandr(nm,n,mb,st,w,e,e,.false.,z)
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
         go to  140
c
c     rsbw1z  ( usage here computes all the eigenvectors )
c
   89    write(iwrite,893)
  893    format(43h1some of the eigenvalues and corresponding ,
     x     46heigenvectors of the real symmetric band matrix /
     x     57h follow.  the path involving  imtqlv and bandv  was used.)
         do 894 i = 1,mb
            do 894 j = 1,n
  894          aa(j,i) = st(j,i)
         call  bandr(nm,n,mb,st,d,e,e2,.false.,z)
         call  imtqlv(n,d,e,e2,w,ind,error,rv1)
         write(iwrite,895) error
  895    format(//31h *****error from imtqlv***** = ,i4)
         m = n
         if( error .ne. 0 ) m = error - 1
         call  bandv(nm,n,mb,aa,0.0d0,m,w,z,error,nv2,rv2,rv6)
         write(iwrite,98) error
   98    format(//30h *****error from bandv***** = ,i4)
         go to 140
c
c     rsb1w  using  bisect
c
  100    write(iwrite,101)
  101    format(25h1some of the eigenvalues ,
     x     41hof the real symmetric band matrix follow.)
         eps1 = 0.0d0
         call  bandr(nm,n,mb,st,d,e,e2,.false.,z)
         call  bisect(n,eps1,d,e,e2,lb,ub,mm,m,w,ind,error,rv4,rv5)
         write(iwrite,103) error
  103    format(//31h *****error from bisect***** = ,i4)
         go to  150
c
c     rsb1w  using  tridib
c
  115    write(iwrite,101)
         eps1 = 0.0d0
         call  bandr(nm,n,mb,st,d,e,e2,.false.,z)
         call  tridib(n,eps1,d,e,e2,lb,ub,m11,no,w,ind,error,rv4,rv5)
         write(iwrite,118) error
  118    format(//31h *****error from tridib***** = ,i4)
         write(iwrite,119) lb,ub
  119    format(//49h the eigenvalues as determined by  tridib  are in
     x          , 13h the interval,1pd16.8,5h  to ,d16.8)
         if( error .ne. 0 ) go to 230
         go to 150
c
c     rsb1w1z  using  bisect  and  bandv
c
  120    write(iwrite,121)
  121    format(43h1some of the eigenvalues and corresponding ,
     x     46heigenvectors of the real symmetric band matrix /
     x     47h follow.  the path involving  bisect and bandv ,
     x     11h was used.  )
         eps1 = 0.0d0
         do 122 i=1,mb
           do 122 j=1,n
  122        aa(j,i) = st(j,i)
         call  bandr(nm,n,mb,st,d,e,e2,.false.,z)
         call  bisect(n,eps1,d,e,e2,lb,ub,mm,m,w,ind,error,rv1,rv2)
         write(iwrite,123) error
  123    format(//31h *****error from bisect***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  bandv(nm,n,mb,aa,0.0d0,m,w,z,error,nv2,rv2,rv6)
         write(iwrite,124) error
  124    format(//30h *****error from bandv***** = ,i4)
         go to 150
c
c     rsb1w1z  using  tridib  and  bandv
c
  125    write(iwrite,126)
  126    format(43h1some of the eigenvalues and corresponding ,
     x     46heigenvectors of the real symmetric band matrix /
     x     57h follow.  the path involving  tridib and bandv  was used.)
         eps1 = 0.0d0
         do 127 i=1,mb
           do 127 j=1,n
  127        aa(j,i) = st(j,i)
         call  bandr(nm,n,mb,st,d,e,e2,.false.,z)
         call  tridib(n,eps1,d,e,e2,lb,ub,m11,no,w,ind,error,rv4,rv5)
         write(iwrite,128) error
  128    format(//31h *****error from tridib***** = ,i4)
         if( error .ne. 0 ) go to 230
         m = no
         call  bandv(nm,n,mb,aa,0.0d0,m,w,z,error,nv2,rv2,rv6)
         write(iwrite,129) error
  129    format(//30h *****error from bandv***** = ,i4)
         write(iwrite,119) lb,ub
         go to 150
c
c     rsb1w1z  using  bqr  and  bandv
c
  130    write(iwrite,131)
  131    format(43h1some of the eigenvalues and corresponding ,
     x     46heigenvectors of the real symmetric band matrix /
     x     44h follow.  the path involving  bqr and bandv ,
     x     11h was used.  )
         do 132 i = 1,mb
            do 132 j = 1,n
  132          aa(j,i) = st(j,i)
         n1 = n
         t = tstart
         r = 0.0d0
         do 133 j = 1,n
  133       st(j,mb) = st(j,mb) - t
         do 138 i = 1,mbqr
            call  bqr(nm,n1,mb,st,t,r,error,nv1,rv1)
            if( error .eq. 0 ) go to 134
            m = i - 1
            go to 139
  134       if( i .eq. 1 ) go to 136
            do 135 jj = 2,i
               j = i + 2 - jj
               if( t .gt. w(j - 1) ) go to 137
               w(j) = w(j - 1)
  135       continue
  136       j = 1
  137       w(j) = t
            n1 = n1 - 1
  138    continue
         m = mbqr
  139    write(iwrite,1395) error
 1395    format(//28h *****error from bqr***** = ,i4)
         call  bandv(nm,n,mb,aa,0.0d0,m,w,z,error,nv2,rv2,rv6)
         write(iwrite,129) error
c
  140    write(iwrite,145) (i,w(i),i=1,m)
  145    format(///18h eigenvalues of  a/3(i4,2x,1pd24.16,3x))
         if( icall .eq. 3 .or. icall .eq. 4 ) go to 230
         go to 195
  150    write(iwrite,160) m,lb,ub
  160    format(///4h the ,i4,29h  eigenvalues of  a  between ,1pd24.16,
     x        6h  and ,d24.16)
         if( m .eq. 0 )  go to  230
         write(iwrite,163) (i,w(i),i=1,m)
  163    format(//3(i3,2x,1pd24.16,3x))
         if( icall .eq. 7 .or. icall .eq. 9 ) go to 230
  195    call  rmatin(nm,mmb,n,mb,st,sthold,1)
         call  rsbwzr(nm,n,m,mb,st,w,z,norm,resdul)
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
