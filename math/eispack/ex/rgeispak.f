c
c     this driver tests  eispack  for the class of real general matrices
c     exhibiting the use of  eispack  to find all the eigenvalues and
c     eigenvectors, only all the eigenvalues, or all the eigenvalues and
c     some of the corresponding eigenvectors, using orthogonal or
c     elementary similarity transformations to reduce the full matrix to
c     hessenberg form, with and without balancing of the full matrix.
c
c     this driver is catalogued as  eispdrv4(rgeispak).
c
c     the dimension of  a,z,rm1, and  asave  should be  nm  by  nm.
c     the dimension of  wr,wi,select,slhold,int,scale,ort,norm,rv1,
c     and  rv2  should be  nm.
c     the dimension of  ahold  should be  nm  by  nm.
c     here nm = 20.
c
      double precision a(20,20),z(20,20),asave(20,20),rm1(20,20), 
     x        ahold( 20, 20),ort( 20),wr( 20),wi(20),
     x        scale( 20),rv1( 20),rv2( 20),norm( 20),
     x        tcrit,machep,resdul
      integer  int( 20),low,upp,error,cp,cp1,cp2
      logical  select( 20),slhold( 20),cplx
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
      open(unit=1,file='rgdatai')
      open(unit=5,file='rgdatas')
      rewind 1
      rewind 5
   10 call rmatin(nm,n,a,ahold,0)
      write(iwrite,20) n
   20 format(37h1the real general matrix  a  of order,
     x       i4,23h  is (printed by rows)  )
      do 30 i = 1,n
   30    write(iwrite,40) (a(i,j),j=1,n)
   40    format(/(1x,1p5d24.16))
      read(ireadc,50) mm,(select(i),i=1,n)
   50 format(i4/(72l1))
      do  60  i = 1,n
   60 slhold(i) = select(i)
c
c     mm  and  select  are read from sysin after the matrix is
c     generated.  mm  specifies to  invit  the maximum number of
c     eigenvectors that will be computed.  select  contains information
c     about which eigenvectors are desired.
c
      do  510  icall = 1,12
         icall2 = mod(icall,3)
         if( icall2 .ne. 0 ) go to 80
         do  70  i = 1,n
   70    select(i) = slhold(i)
   80    if( icall .ne. 1 )  call  rmatin(nm,n,a,ahold,1)
         go to  (90,100,110,120,130,140,160,165,170,175,180,185), icall
c
c     rgewz
c     invoked from driver subroutine  rg.
c
   90    write(iwrite,903)
  903    format(41h1the eigenvalues and eigenvectors of the ,
     x     50hreal general matrix follow.  this matrix has been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27helementary transformations.  )
         call  rg(nm,n,a,wr,wi,1,z,int,scale,error)
         write(iwrite,905) error
  905    format(//28h *****error from hqr2***** = ,i4)
         go to  230
c
c     rgew
c     invoked from driver subroutine  rg.
c
  100    write(iwrite,1003)
 1003    format(24h1the eigenvalues of the ,
     x     50hreal general matrix follow.  this matrix has been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27helementary transformations.  )
         call  rg(nm,n,a,wr,wi,0,a,int,scale,error)
         write(iwrite,1005) error
 1005    format(//27h *****error from hqr***** = ,i4)
         go to  230
c
c     rgew1z
c
  110    write(iwrite,1103)
 1103    format(50h1the eigenvalues and selected eigenvectors of the ,
     x     50hreal general matrix follow.  this matrix has been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27helementary transformations.  )
         call  balanc(nm,n,a,low,upp,scale)
         call  elmhes(nm,n,low,upp,a,int)
         do  112  i = 1,n
            do  112  j = 1,n
  112         asave(i,j) = a(i,j)
         call  hqr(nm,n,low,upp,a,wr,wi,error)
         write(iwrite,113) error,low,upp
  113    format(//35h *****error from hqr,low,upp***** = ,3i4)
         call  invit(nm,n,asave,wr,wi,select,mm,m,z,error,rm1,rv1,rv2)
         write(iwrite,114) error
  114    format(//29h *****error from invit***** = ,i4)
         call  elmbak(nm,low,upp,asave,int,m,z)
         call  balbak(nm,n,low,upp,scale,m,z)
         go to  230
c
c     rgnewz
c
  120    write(iwrite,1203)
 1203    format(41h1the eigenvalues and eigenvectors of the ,
     x     54hreal general matrix follow.  this matrix has not been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27helementary transformations.  )
         call  elmhes(nm,n,1,n,a,int)
         call  eltran(nm,n,1,n,a,int,z)
         call  hqr2(nm,n,1,n,a,wr,wi,z,error)
         write(iwrite,1205) error
 1205    format(//28h *****error from hqr2***** = ,i4)
         go to  230
c
c     rgnew
c
  130    write(iwrite,1303)
 1303    format(24h1the eigenvalues of the ,
     x     54hreal general matrix follow.  this matrix has not been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27helementary transformations.  )
         call  elmhes(nm,n,1,n,a,int)
         call  hqr(nm,n,1,n,a,wr,wi,error)
         write(iwrite,1305) error
 1305    format(//27h *****error from hqr***** = ,i4)
         go to  230
c
c     rgnew1z
c
  140    write(iwrite,1403)
 1403    format(50h1the eigenvalues and selected eigenvectors of the ,
     x     54hreal general matrix follow.  this matrix has not been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27helementary transformations.  )
         call  elmhes(nm,n,1,n,a,int)
         do  142  i = 1,n
            do  142  j = 1,n
  142         asave(i,j) = a(i,j)
         call  hqr(nm,n,1,n,a,wr,wi,error)
         write(iwrite,143) error
  143    format(//27h *****error from hqr***** = ,i4)
         call  invit(nm,n,asave,wr,wi,select,mm,m,z,error,rm1,rv1,rv2)
         write(iwrite,144) error
  144    format(//29h *****error from invit***** = ,i4)
         call  elmbak(nm,1,n,asave,int,m,z)
         go to 230
c
c     rgowz
c
  160    write(iwrite,1603)
 1603    format(41h1the eigenvalues and eigenvectors of the ,
     x     50hreal general matrix follow.  this matrix has been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27horthogonal transformations.  )
         call  balanc(nm,n,a,low,upp,scale)
         call  orthes(nm,n,low,upp,a,ort)
         call  ortran(nm,n,low,upp,a,ort,z)
         call  hqr2(nm,n,low,upp,a,wr,wi,z,error)
         write(iwrite,1605) error,low,upp
 1605    format(//36h *****error from hqr2,low,upp***** = ,3i4)
         call  balbak(nm,n,low,upp,scale,n,z)
         go to  230
c
c     rgow
c
  165    write(iwrite,1653)
 1653    format(24h1the eigenvalues of the ,
     x     50hreal general matrix follow.  this matrix has been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27horthogonal transformations.  )
         call  balanc(nm,n,a,low,upp,scale)
         call  orthes(nm,n,low,upp,a,ort)
         call  hqr(nm,n,low,upp,a,wr,wi,error)
         write(iwrite,1655) error,low,upp
 1655    format(//35h *****error from hqr,low,upp***** = ,3i4)
         go to  230
c
c     rgow1z
c
  170    write(iwrite,1703)
 1703    format(50h1the eigenvalues and selected eigenvectors of the ,
     x     50hreal general matrix follow.  this matrix has been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27horthogonal transformations.  )
         call  balanc(nm,n,a,low,upp,scale)
         call  orthes(nm,n,low,upp,a,ort)
         do  172  i = 1,n
            do  172  j = 1,n
  172         asave(i,j) = a(i,j)
         call  hqr(nm,n,low,upp,a,wr,wi,error)
         write(iwrite,173) error,low,upp
  173    format(//35h *****error from hqr,low,upp***** = ,3i4)
         call  invit(nm,n,asave,wr,wi,select,mm,m,z,error,rm1,rv1,rv2)
         write(iwrite,174) error
  174    format(//29h *****error from invit***** = ,i3)
         call  ortbak(nm,low,upp,asave,ort,m,z)
         call  balbak(nm,n,low,upp,scale,m,z)
         go to  230
c
c     rgnowz
c
  175    write(iwrite,1753)
 1753    format(41h1the eigenvalues and eigenvectors of the ,
     x     54hreal general matrix follow.  this matrix has not been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27horthogonal transformations.  )
         call  orthes(nm,n,1,n,a,ort)
         call  ortran(nm,n,1,n,a,ort,z)
         call  hqr2(nm,n,1,n,a,wr,wi,z,error)
         write(iwrite,1755) error
 1755    format(//28h *****error from hqr2*****  ,i4)
         go to  230
c
c     rgnow
c
  180    write(iwrite,1803)
 1803    format(24h1the eigenvalues of the ,
     x     54hreal general matrix follow.  this matrix has not been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27horthogonal transformations.  )
         call  orthes(nm,n,1,n,a,ort)
         call  hqr(nm,n,1,n,a,wr,wi,error)
        write(iwrite,1805) error
 1805    format(//27h *****error from hqr***** = ,i3)
         go to  230
c
c     rgnow1z
c
  185    write(iwrite,1853)
 1853    format(50h1the eigenvalues and selected eigenvectors of the ,
     x     54hreal general matrix follow.  this matrix has not been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27horthogonal transformations.  )
         call  orthes(nm,n,1,n,a,ort)
         do  187  i = 1,n
            do  187  j = 1,n
  187         asave(i,j) = a(i,j)
         call  hqr(nm,n,1,n,a,wr,wi,error)
         write(iwrite,188) error
  188    format(//27h *****error from hqr***** = ,i3)
         call  invit(nm,n,asave,wr,wi,select,mm,m,z,error,rm1,rv1,rv2)
         write(iwrite,189) error
  189    format(//29h *****error from invit***** = ,i3)
         call  ortbak(nm,1,n,asave,ort,m,z)
c
  230    if( icall2 .eq. 0 )  go to  250
         write(iwrite,240) (i,wr(i),wi(i),i=1,n)
  240    format(//29h all the eigenvalues computed/
     x        2(i3,3x,1p2d24.16,2x))
         go to  270
  250    write(iwrite,260) (i,select(i),wr(i),wi(i),i=1,n)
  260    format(//51h all the eigenvalues computed and those eigenvalues
     x            ,9h selected/2(i3,1x,l1,1x,1p2d24.16,2x))
  270    if( error .gt. 0 )  go to  510
         if( icall2 .eq. 2 )  go to  510
         call  rmatin(nm,n,a,ahold,1)
         if( icall2 .ne. 0 )  go to  280
         call  rgw1zr(nm,n,a,wr,wi,select,m,z,norm,resdul)
         go to  290
  280    call  rgwzr(nm,n,a,wr,wi,z,norm,resdul)
  290    tcrit = resdul/(dfloat(10*n)*machep)
         write(iwrite,300) tcrit,resdul
  300    format(///48h as a guide to evaluating the performance of the,
     x     53h  eispack  codes, a number  x, related to the  1-norm /
     x     56h of the residual matrix, may be normalized to the number,
     x     56h  y  by dividing by the product of the machine precision/
     x     57h machep  and  10*n  where  n  is the order of the matrix.,
     x     36h  the  eispack  codes have performed /
     x     56h (well,satisfactorily,poorly) according as the ratio  y ,
     x     54h is (less than  1, less than  100, greater than  100)./
     x     23h for this run,  y  is =,1pd14.6,11h  with  x =,d14.6)
         nm2 = n-2
         k = 1
         if( icall2 .eq. 0 )  go to  390
  310      if( k .gt. n )  go to  510
           if( wi(k) .eq. 0.0d0 )  go to  360
           if( k .gt. nm2 )  go to  340
          if( wi(k+2) .eq. 0.0d0 )  go to  330
c          both digenvectors are complex.
           write(iwrite,320)
     x      wr(k),wi(k),wr(k+2),wi(k+2),norm(k),norm(k+2), 
     x      (z(i,k),z(i,k+1),z(i,k+2),z(i,k+3),i=1,n)
  320      format(///1x,2(19x,15heigenvalue of a,20x)/1x,2(1p2d24.16,6x)
     x       //1x,2(8x,39h1-norm of corresponding residual vector,7x)/
     x       1x,2(6x,26h!!ax-l*x!!/(!!x!!*!!a!!) =,d16.8,6x)//
     x       1x,2(14x,25hcorresponding eigenvector,15x)/(1x,
     x       2d24.16,6x,2d24.16,6x))
           k = k+4
           go to  310
c          one complex eigenvector and one real eigenvector in that
c          order.
  330      write(iwrite,320)
     x                wr(k+2),wi(k+2),wr(k),wi(k),norm(k+2),norm(k), 
     x                (z(i,k+2),wi(k+2),z(i,k),z(i,k+1),i=1,n)
           k = k+3
           go to  310
c          one complex eigenvector.
  340      write(iwrite,350) wr(k),wi(k),norm(k),(z(i,k),z(i,k+1),i=1,n)
  350      format(///1x,19x,15heigenvalue of a,20x/1x,1p2d24.16,6x//
     x       1x,8x,39h1-norm of corresponding residual vector,7x/
     x       1x,6x,26h!!ax-l*x!!/(!!x!!*!!a!!) =,d16.8,6x//
     x       1x,14x,25hcorresponding eigenvector,15x/(1x,2d24.16,6x))
           go to  510
  360      if( k .eq. n )  go to  380
           if( wi(k+1) .eq. 0.0d0 )  go to  370
c          one real eigenvector and one complex eigenvector in that
c          order.
           write(iwrite,320)
     x                wr(k),wi(k),wr(k+1),wi(k+1),norm(k),norm(k+1), 
     x                (z(i,k),wi(k),z(i,k+1),z(i,k+2),i=1,n)
           k = k+3
           go to  310
c          both eigenvectors are real.
  370      write(iwrite,320)
     x                wr(k),wi(k),wr(k+1),wi(k+1),norm(k),norm(k+1), 
     x                (z(i,k),wi(k),z(i,k+1),wi(k+1),i=1,n)
           k = k+2
           go to  310
c          one real eigenvector.
  380      write(iwrite,350) wr(k),wi(k),norm(k),(z(i,k),wi(k),i=1,n)
           go to  510
  390    l = 1
         cplx = .false.
  400    cp1 = -1
  410      if( k .gt. n ) if( cp1 )  510, 490, 500
           if( .not. cplx )  go to  412
             cplx = .false.
             go to  415
  412      if( wi(k) .ne. 0.0d0 )  cplx = .true.
  415      kold = k
           k = k+1
           if( .not. select(kold) )  go to  410
           lold = l
           l = l+1
           cp = 0
           if( wi(kold) .eq. 0.0d0 )  go to  420
             cp = 1
             if( cplx )  k = k+1
             l = l+1
  420      if( cp1 )  430, 440, 440
  430        lp1 = lold
             kp1 = kold
             cp1 = cp
             go to  410
  440        lp2 = lold
             kp2 = kold
             cp2 = cp
           cp = cp1*2 + cp2 + 1
           go to  (450,460,470,480),  cp
  450      write(iwrite,320) wr(kp1),wi(kp1),wr(kp2),wi(kp2),norm(kp1),
     x         norm(kp2),(z(i,lp1),wi(kp1),z(i,lp2),wi(kp2),i=1,n)
           go to  400
  460      write(iwrite,320) wr(kp1),wi(kp1),wr(kp2),wi(kp2),norm(kp1),
     x        norm(kp2),(z(i,lp1),wi(kp1),z(i,lp2),z(i,lp2+1),i=1,n)
           go to  400
  470      kold = kp1
           kp1 = kp2
           kp2 = kold
           lold = lp1
           lp1 = lp2
           lp2 = lold
           go to  460
  480      write(iwrite,320) wr(kp1),wi(kp1),wr(kp2),wi(kp2),norm(kp1),
     x     norm(kp2),(z(i,lp1),z(i,lp1+1),z(i,lp2),z(i,lp2+1),i=1,n)
           go to  400
  490      write(iwrite,350) wr(kp1),wi(kp1),norm(kp1),
     x                 (z(i,lp1),wi(kp1),i=1,n)
           go to  510
  500      write(iwrite,350) wr(kp1),wi(kp1),norm(kp1),
     x                 (z(i,lp1),z(i,lp1+1),i=1,n)
  510 continue
      go to  10
      end
