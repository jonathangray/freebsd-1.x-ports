c
c     this driver tests  eispack  for the class of complex hermitian
c     matrices exhibiting the use of  eispack  to find all the
c     eigenvalues and eigenvectors, only all the eigenvalues, some of
c     the eigenvalues and the corresponding eigenvectors, or only some
c     of the eigenvalues.
c
c     this driver is catalogued as  eispdrv4(cheispak).
c
c     the dimension of  ar,ai,zr,  and  zi  should be  nm  by  nm.
c     the dimension of  w,norm,d,e,e2,ind,rv1,rv2,rv3,rv4,rv5,  and
c       rv6  should be  nm.
c     the dimension of  tau  should be  2  by  nm.
c     the dimension of  arhold  and  aihold  should be  nm  by  nm.
c     here nm = 20.
c
      double precision ar( 20, 20),ai( 20, 20),zr( 20, 20),zi( 20, 20),
     x        tau( 2, 20),w( 20),d( 20),
     x        e( 20),e2( 20),rv1( 20),rv2( 20),rv3( 20),rv4( 20),
     x        rv5( 20),rv6( 20),norm( 20),tcrit,machep,resdul,
     x        maxeig,maxdif,eigdif,u,lb,ub,eps1
      double precision arhold( 20, 20),aihold( 20, 20)
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
   10 call cmatin(nm,n,ar,ai,arhold,aihold,0)
      write(iwrite,20) n
   20 format(52h1the complex hermitian matrix  a = (ar,ai)  of order,
     x       i4,22h  is (printed by rows)    )
      do  30  i = 1,n
   30    write(iwrite,40) (ar(i,j),ai(i,j),j=1,n)
   40    format(/3(1p2d18.10,4x))
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
         if( icall .ne. 1 )  call  cmatin(nm,n,ar,ai,arhold,aihold,1)
c
c
c     if  tqlrat  path (label 80) is taken then  tql2    path (label 70)
c     must also be taken in order that the normalized difference be
c     meaningful.
c     if  imtql1  path (label 85) is taken then  imtql2  path (label 75)
c     must also be taken in order that the normalized difference be
c     meaningful.
c
         go to  (70,75,80,85,89,90,95,100,110,115),  icall
c
c     chwz  using  tql2
c     invoked from driver subroutine  ch.
c
   70    write(iwrite,705)
  705    format(42h1all of the eigenvalues and corresponding ,
     x     52heigenvectors of the complex hermitian matrix follow. /
     x     36h the path involving  tql2  was used.  )
         call  ch(nm,n,ar,ai,w,1,zr,zi,e,e,tau,error)
         write(iwrite,725) error
  725    format(//29h *****error from tql2***** = ,i4)
         m = error - 1
         if( error .ne. 0 ) go to 74
         m = n
         do 73 i = 1,n
            rv6(i) = w(i)
   73    continue
   74    go to  130
c
c     chwz  using  imtql2
c
   75    write(iwrite,755)
  755    format(42h1all of the eigenvalues and corresponding ,
     x     52heigenvectors of the complex hermitian matrix follow. /
     x     38h the path involving  imtql2  was used.  )
         do  77  i = 1,n
            do  76  j = 1,n
   76         zr(i,j) = 0.0d0
   77       zr(i,i) = 1.0d0
         call  htridi(nm,n,ar,ai,w,e,e,tau)
         call  imtql2(nm,n,w,e,zr,error)
         write(iwrite,775) error
  775    format(//31h *****error from imtql2***** = ,i4)
         m = error - 1
         if( error .ne. 0 ) go to 79
         do 78 i = 1,n
   78       rv5(i) = w(i)
         m = n
   79    call  htribk(nm,n,ar,ai,tau,m,zr,zi)
         go to  130
c
c     chw  using  tqlrat
c     invoked from driver subroutine  ch.
c
   80    write(iwrite,805)
  805    format(24h1all of the eigenvalues ,
     x     39hof the complex hermitian matrix follow. /
     x     38h the path involving  tqlrat  was used.  )
         call  ch(nm,n,ar,ai,w,0,ar,ai,e,e2,tau,error)
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
         go to 130
c
c     chw  using  imtql1
c
   85    write(iwrite,855)
  855    format(24h1all of the eigenvalues ,
     x     39hof the complex hermitian matrix follow. /
     x     38h the path involving  imtql1  was used.  )
         call  htridi(nm,n,ar,ai,w,e,e,tau)
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
c     chw1z  ( usage here computes all the eigenvectors )
c
   89    write(iwrite,890)
  890    format(43h1some of the eigenvalues and corresponding ,
     x     52heigenvectors of the complex hermitian matrix follow./
     x     39h the path involving  imtqlv  was used.  )
         do 892 i = 2,n
            im1 = i - 1
            do 891 j = 1,im1
               ar(j,i) = ai(i,j)
  891       continue
  892    continue
         call  htrid3(nm,n,ar,d,e,e2,tau)
         call  imtqlv(n,d,e,e2,w,ind,error,rv1)
         write(iwrite,893) error
  893    format(//31h *****error from imtqlv***** = ,i4)
         m = n
         if( error .ne. 0 ) m = error - 1
         call  tinvit(nm,n,d,e,e2,m,w,ind,zr,error,rv1,rv2,rv3,rv4,rv6)
         write(iwrite,98) error
         call  htrib3(nm,n,ar,tau,m,zr,zi)
         call  cmatin(nm,n,ar,ai,arhold,aihold,1)
         go to 130
c
c     ch1w1z  using  tsturm
c
   90    write(iwrite,91)
   91    format(43h1some of the eigenvalues and corresponding ,
     x     52heigenvectors of the complex hermitian matrix follow. /
     x     38h the path involving  tsturm  was used.  )
         eps1 = 0.0d0
         call  htridi(nm,n,ar,ai,d,e,e2,tau)
         call  tsturm(nm,n,eps1,d,e,e2,lb,ub,mm,m,w,zr,error,rv1,
     x             rv2,rv3,rv4,rv5,rv6)
         write(iwrite,92) error
   92    format(//31h *****error from tsturm***** = ,i4)
         if( error .eq. 3*n + 1 ) go to 230
         if( error .gt. 4*n ) m = error - 4*n - 1
         call htribk(nm,n,ar,ai,tau,m,zr,zi)
         go to  150
c
c     ch1w1z  using  bisect  and  tinvit
c
   95    write(iwrite,96)
   96    format(43h1some of the eigenvalues and corresponding ,
     x     52heigenvectors of the complex hermitian matrix follow. /
     x     38h the path involving  bisect  was used.  )
         eps1 = 0.0d0
         call  htridi(nm,n,ar,ai,d,e,e2,tau)
         call  bisect(n,eps1,d,e,e2,lb,ub,mm,m,w,ind,error,rv4,rv5)
         write(iwrite,97) error
   97    format(//31h *****error from bisect***** = ,i4)
         if( error .ne. 0 ) go to 230
         call  tinvit(nm,n,d,e,e2,m,w,ind,zr,error,rv1,rv2,rv3,rv4,rv6)
         write(iwrite,98) error
   98    format(//31h *****error from tinvit***** = ,i4)
         call  htribk(nm,n,ar,ai,tau,m,zr,zi)
         go to  150
c
c     ch1w  using  bisect
c
  100    write(iwrite,101)
  101    format(25h1some of the eigenvalues ,
     x     39hof the complex hermitian matrix follow. )
         eps1 = 0.0d0
         call  htridi(nm,n,ar,ai,d,e,e2,tau)
         call  bisect(n,eps1,d,e,e2,lb,ub,mm,m,w,ind,error,rv4,rv5)
         write(iwrite,102) error
  102    format(//31h *****error from bisect***** = ,i4)
         go to  150
c
c     ch1w1z  using  tridib  and  tinvit
c
  110    write(iwrite,111)
  111    format(43h1some of the eigenvalues and corresponding ,
     x     52heigenvectors of the complex hermitian matrix follow./
     x     38h the path involving  tridib  was used.  )
         eps1 = 0.0d0
         call  htridi(nm,n,ar,ai,d,e,e2,tau)
         call  tridib(n,eps1,d,e,e2,lb,ub,m11,no,w,ind,error,rv4,rv5)
         write(iwrite,112) error
  112    format(//31h *****error from tridib***** = ,i4)
         if( error .ne. 0 )  go to  230
         m = no
         write(iwrite,113) lb,ub
  113    format(//49h the eigenvalues as determined by  tridib  are in
     x         ,14h the interval ,1pd16.8,4h  to ,d16.8)
         call  tinvit(nm,n,d,e,e2,m,w,ind,zr,error,rv1,rv2,rv3,rv4,rv6)
         write(iwrite,114) error
  114    format(//31h *****error from tinvit***** = ,i4)
         call  htribk(nm,n,ar,ai,tau,m,zr,zi)
         go to  150
c
c     ch1w  using  tridib
c
  115    write(iwrite,101)
         eps1 = 0.0d0
         call  htridi(nm,n,ar,ai,d,e,e2,tau)
         call  tridib(n,eps1,d,e,e2,lb,ub,m11,no,w,ind,error,rv4,rv5)
         write(iwrite,117) error
  117    format(//31h *****error from tridib***** = ,i4)
         if( error .ne. 0 )  go to  230
         m = no
         write(iwrite,113) lb,ub
         go to 150
c
  130    write(iwrite,140) (i,w(i),i=1,n)
  140    format(///18h eigenvalues of  a/3(i3,2x,1pd24.16,3x))
         if( icall .eq. 3 .or. icall .eq. 4 )  go to  230
         go to  190
c
  150    write(iwrite,160) m,lb,ub
  160    format(///4h the ,i4,29h  eigenvalues of  a  between ,1pd24.16,
     x        6h  and ,d24.16)
         if( m .eq. 0 )  go to  230
         write(iwrite,163) (i,w(i),i=1,m)
  163    format(//3(i3,2x,1pd24.16,3x))
         if( icall .eq. 8 .or. icall .eq. 10 ) go to 230
c
  190    do 195 i = 1,n
            ai(i,i) = 0.0d0
  195    continue
         call  chwzr(nm,n,m,ar,ai,w,zr,zi,norm,resdul)
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
         do  225  kk = 1,m,2
           if( kk .eq. m )  go to  220
           kk1 = kk+1
           write(iwrite,210) w(kk),w(kk1),norm(kk),norm(kk1),
     x             ((zr(i,k),zi(i,k),k=kk,kk1),i=1,n)
  210      format(///1x,2(19x,15heigenvalue of a,20x)/1x,2(1pd36.16,18x)
     x      //1x,2(8x,39h1-norm of corresponding residual vector,7x)/
     x      1x,2(6x,26h!!ax-l*x!!/(!!x!!*!!a!!) =,d16.8,6x)//1x,
     x      2(14x,25hcorresponding eigenvector,15x)/
     x      (1x,2d24.16,6x,2d24.16,6x))
           go to  225
  220      write(iwrite,222) w(kk),norm(kk),
     x           ((zr(i,k),zi(i,k),k=kk,kk),i=1,n)
  222      format(///1x,19x,15heigenvalue of a,20x/1x,1pd36.16,18x//
     x      1x,8x,39h1-norm of corresponding residual vector,7x/
     x      1x,6x,26h!!ax-l*x!!/(!!x!!*!!a!!) =,d16.8,6x//
     x      1x,14x,25hcorresponding eigenvector,15x/(1x,2d24.16,6x))
  225   continue
  230 continue
      go to  10
      end
