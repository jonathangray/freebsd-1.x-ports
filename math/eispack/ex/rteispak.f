c
c     this driver tests  eispack  for the class of real tri-
c     diagonal matrices exhibiting the use of  eispack  to find all the
c     eigenvalues and eigenvectors, only all the eigenvalues, some of
c     the eigenvalues and the corresponding eigenvectors, or only some
c     of the eigenvalues.
c
c     this driver is catalogued as  eispdrv4(rteispak).
c
c     the dimension of  a  should be  nm  by  3.
c     the dimension of  z  should be  nm  by  nm.
c     the dimension of  w,norm,d,e,e2,rv1,rv2,rv3,rv4,rv5,rv6,  and
c     ind  should be  nm.
c     here nm = 20.
c
      double precision z( 20, 20),a( 20, 3),w( 20),d( 20),e( 20),
     x        e2( 20),rv1( 20),rv2( 20),rv3( 20),rv4( 20),rv5( 20),
     x        rv6( 20),norm( 20),tcrit,machep,resdul,maxeig,maxdif,
     x        eigdif,u,lb,ub,eps1
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
   10 call rmatin(nm,n,a)
      write(iwrite,20) n
   20 format(48h1the subdiagonal, diagonal and superdiagonal in ,
     x   53hthat order of the tridiagonal non-symmetric matrix  a,
     x   10h  of order,i4,4h  is)
      do  30  j = 1,3
         is = max0(1,3-j)
         ie = min0(n,n+2-j)
   30    write(iwrite,40) (a(i,j),i=is,ie)
   40    format(/(1x,1p5d24.16))
      read(ireadc,50) mm,lb,ub,m11,no  
   50 format(i4,2d24.16,2(4x,i4))
c
c     mm,lb,ub,m11,  and  no  are read from sysin after the matrix is
c     generated.  mm,lb,  and  ub  specify to  bisect  the maximum
c     number of eigenvalues and bounds for the interval which is to
c     be searched.  m11  and  no  specify to  tridib  the lower
c     boundary index and the number of desired eigenvalues.
c
      do  230  icall = 1,10
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
c     rtwz  using  tql2
c
   70    write(iwrite,71)
   71    format(42h1all of the eigenvalues and corresponding ,
     x     51heigenvectors of the real tridiagonal matrix follow./
     x     36h the path involving  tql2  was used.  )
         call  figi2(nm,n,a,w,e,z,error)
         write(iwrite,72) error
   72    format(//30h *****error from figi2***** = ,i4)
         if( error .ne. 0 )  go to  230
         call  tql2(nm,n,w,e,z,error)
         write(iwrite,725) error
  725    format(//29h *****error from tql2***** = ,i4)
         do 73 i = 1,n
   73       rv6(i) = w(i)
         m = n
         if( error .ne. 0 ) m = error - 1
         go to  130
c
c     rtwz  using  imtql2
c     invoked from driver subroutine  rt.
c
   75    write(iwrite,76)
   76    format(42h1all of the eigenvalues and corresponding ,
     x     51heigenvectors of the real tridiagonal matrix follow./
     x     38h the path involving  imtql2  was used.  )
         call  rt(nm,n,a,w,1,z,e,error)
         write(iwrite,77) error
   77    format(//30h *****error from figi2***** = ,i4)
         if( error .gt. 0 )  go to  230
         write(iwrite,775) error
  775    format(//31h *****error from imtql2***** = ,i4)
         do 78 i = 1,n
            rv5(i) = w(i)
   78    continue
         m = n
         if( error .ne. 0 ) m = error - 1
         go to  130
c
c     rtw  using  tqlrat
c
   80    write(iwrite,805)
  805    format(24h1all of the eigenvalues ,
     x     38hof the real tridiagonal matrix follow./
     x     38h the path involving  tqlrat  was used.  )
         call  figi(nm,n,a,w,e,e2,error)
         write(iwrite,807) error
  807    format(//29h *****error from figi***** = ,i4)
         if( error .gt. 0 )  go to  230
         call  tqlrat(n,w,e2,error)
         write(iwrite,809) error
  809    format(//31h *****error from tqlrat***** = ,i4)
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
c     rtw  using  imtql1
c     invoked from driver subroutine  rt.
c
   85    write(iwrite,855)
  855    format(24h1all of the eigenvalues ,
     x     38hof the real tridiagonal matrix follow./
     x     38h the path involving  imtql1  was used.  )
         call  rt(nm,n,a,w,0,z,e,error)
         if( error .gt. 0 )  go to  230
         write(iwrite,859) error
  859    format(//31h *****error from imtql1***** = ,i4)
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
c     rtw1z  ( usage here computes all the eigenvectors )
c
   89    write(iwrite,891)
  891    format(43h1some of the eigenvalues and corresponding ,
     x     51heigenvectors of the real tridiagonal matrix follow./
     x     38h the path involving  imtqlv  was used. )
         call  figi(nm,n,a,d,e,e2,error)
         write(iwrite,892) error
  892    format(//29h *****error from figi***** = ,i4)
         if( error .ne. 0 ) go to 230
         call imtqlv(n,d,e,e2,w,ind,error,rv1)
         write(iwrite,893) error
  893    format(//31h *****error from imtqlv***** = ,i4)
         m = n
         if( error .ne. 0 ) m = error - 1
         call tinvit(nm,n,d,e,e2,m,w,ind,z,error,rv1,rv2,rv3,rv4,rv6)
         write(iwrite,99) error
         call  bakvec(nm,n,a,e,m,z,error)
         write(iwrite,894) error
  894    format(//31h *****error from bakvec***** = , i4)
         go to 130
c
c     rt1w1z  using  tsturm
c
   90    write(iwrite,91)
   91    format(43h1some of the eigenvalues and corresponding ,
     x     51heigenvectors of the real tridiagonal matrix follow./
     x     38h the path involving  tsturm  was used.  )
         eps1 = 0.0d0
         call  figi(nm,n,a,d,e,e2,error)
         write(iwrite,92) error
   92    format(//29h *****error from figi***** = ,i4)
         if( error .ne. 0 )  go to  230
         call  tsturm(nm,n,eps1,d,e,e2,lb,ub,mm,m,w,z,error,rv1,
     x         rv2,rv3,rv4,rv5,rv6)
         if( error .eq. 3*n + 1 ) go to 230
         if( error .gt. 4*n ) m = error - 1
         write(iwrite,93) error
   93    format(//31h *****error from tsturm***** = ,i4)
         call  bakvec(nm,n,a,e,m,z,error)
         write(iwrite,894) error
         go to  150
c
c     rt1w1z  using  bisect  and  tinvit
c
   95    write(iwrite,96)
   96    format(43h1some of the eigenvalues and corresponding ,
     x     51heigenvectors of the real tridiagonal matrix follow./
     x     38h the path involving  bisect  was used.  )
         eps1 = 0.0d0
         call  figi(nm,n,a,d,e,e2,error)
         write(iwrite,97) error
   97    format(//29h *****error from figi***** = ,i4)
         if( error .ne. 0 )  go to  230
         call  bisect(n,eps1,d,e,e2,lb,ub,mm,m,w,ind,error,rv4,rv5)
         write(iwrite,98) error
   98    format(//31h *****error from bisect***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  tinvit(nm,n,d,e,e2,m,w,ind,z,error,rv1,rv2,rv3,rv4,rv6)
         write(iwrite,99) error
   99    format(//31h *****error from tinvit***** = ,i4)
         call  bakvec(nm,n,a,e,m,z,error)
         write(iwrite,894) error
         go to  150
c
c     rt1w  using  bisect
c
  100    write(iwrite,101)
  101    format(25h1some of the eigenvalues ,
     x     38hof the real tridiagonal matrix follow. )
         eps1 = 0.0d0
         call  figi(nm,n,a,d,e,e2,error)
         write(iwrite,102) error
  102    format(//29h *****error from figi***** = ,i4)
         if( error .gt. 0 )  go to  230
         call  bisect(n,eps1,d,e,e2,lb,ub,mm,m,w,ind,error,rv4,rv5)
         write(iwrite,103) error
  103    format(//31h *****error from bisect***** = ,i4)
         go to  150
c
c     rt1w1z  using  tridib  and  tinvit
c
  110    write(iwrite,111)
  111    format(43h1some of the eigenvalues and corresponding ,
     x     51heigenvectors of the real tridiagonal matrix follow./
     x     38h the path involving  tridib  was used. )
         eps1 = 0.0d0
         call  figi(nm,n,a,d,e,e2,error)
         write(iwrite,112) error
  112    format(//29h *****error from figi***** = ,i4)
         if( error .ne. 0 )  go to  230
         call  tridib(n,eps1,d,e,e2,lb,ub,m11,no,w,ind,error,rv4,rv5)
         write(iwrite,113) error
  113    format(//31h *****error from tridib***** = ,i4)
         if( error .ne. 0 )  go to  230
         m = no
         write(iwrite,1135) lb,ub
 1135    format(//49h the eigenvalues as determined by  tridib  are in,
     x          14h the interval ,1pd16.8,5h  to ,d16.8)
         call  tinvit(nm,n,d,e,e2,m,w,ind,z,error,rv1,rv2,rv3,rv4,rv6)
         write(iwrite,114) error
  114    format(//31h *****error from tinvit***** = ,i4)
         call bakvec(nm,n,a,e,m,z,error)
         write(iwrite,894) error
         go to  150
c
c     rt1w  using   tridib
c
  115    write(iwrite,101)
         eps1 = 0.0d0
         call  figi(nm,n,a,d,e,e2,error)
        ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ