c
c     this driver tests  eispack  for the class of complex general ma-
c     trices exhibiting the use of  eispack  to find all the eigenvalues
c     and eigenvectors, only all the eigenvalues, or all the eigen-
c     values and some of the corresponding eigenvectors, using unitary
c     or elementary similarity transformations to reduce the full matrix
c     to hessenberg form, with and without balancing of the full matrix.
c
c     this driver is catalogued as  eispdrv4(cgeispak).
c
c     the dimension of  ar,ai,zr,zi,asaver,asavei,rm1, and  rm2
c     should be  nm  by  nm.
c     the dimension of  wr,wi,select,slhold,int,scale,ortr,orti,norm,
c     rv1,  and  rv2  should be  nm.
c     the dimension of  arhold  and  aihold  should be  nm  by  nm.
c     here nm = 20.
c
      double precision ar( 20, 20),ai( 20, 20),zr( 20, 20),zi( 20, 20),
     x        asaver( 20, 20),asavei( 20, 20),rm1( 20, 20),rm2( 20,20),
     x        ortr( 20),orti( 20),wr( 20),wi( 20),
     x        scale( 20),rv1( 20),rv2( 20),norm( 20),
     x        tcrit,machep,resdul
      double precision arhold( 20, 20),aihold( 20, 20)      
      integer  int( 20),upp,error
      logical  select( 20),slhold( 20)
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
c
      nm = 20
   10 call cmatin(nm,n,ar,ai,arhold,aihold,0)
      write(iwrite,20) n 
   20 format(50h1the complex general matrix  a = (ar,ai)  of order,
     x       i4,22h  is (printed by rows)  )
      do  30  i = 1,n
   30    write(iwrite,40) (ar(i,j),ai(i,j),j=1,n)
   40    format(/2(1p2d24.16,4x))
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
      do  300  icall = 1,12
         icall2 = mod(icall,3)
         if( icall2 .ne. 0 ) go to 80
         do  70  i = 1,n
   70    select(i) = slhold(i)
   80    if( icall .ne. 1 )  call  cmatin(nm,n,ar,ai,arhold,aihold,1)
         go to  (90,100,110,120,130,140,160,165,170,175,180,185), icall
c
c     cgewz
c
   90    write(iwrite,903)
  903    format(41h1the eigenvalues and eigenvectors of the ,
     x     53hcomplex general matrix follow.  this matrix has been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27helementary transformations.  )
         call  cbal(nm,n,ar,ai,low,upp,scale)
         call  comhes(nm,n,low,upp,ar,ai,int)
         call  comlr2(nm,n,low,upp,int,ar,ai,wr,wi,zr,zi,error)
         write(iwrite,905) error,low,upp 
  905    format(//38h *****error from comlr2,low,upp***** =,3i4)
         call  cbabk2(nm,n,low,upp,scale,n,zr,zi)
         go to  190
c
c     cgew
c
  100    write(iwrite,1003)
 1003    format(24h1the eigenvalues of the ,
     x     53hcomplex general matrix follow.  this matrix has been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27helementary transformations.  )
         call  cbal(nm,n,ar,ai,low,upp,scale)
         call  comhes(nm,n,low,upp,ar,ai,int)
         call  comlr(nm,n,low,upp,ar,ai,wr,wi,error)
         write(iwrite,1005) error,low,upp  
 1005    format(//37h *****error from comlr,low,upp***** = ,3i4)
         go to  190
c
c     cgew1z
c
  110    write(iwrite,1103)
 1103    format(50h1the eigenvalues and selected eigenvectors of the ,
     x     53hcomplex general matrix follow.  this matrix has been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27helementary transformations.  )
         call  cbal(nm,n,ar,ai,low,upp,scale)
         call  comhes(nm,n,low,upp,ar,ai,int)
         do  112  i = 1,n
            do  112  j = 1,n
              asaver(i,j) = ar(i,j)
  112         asavei(i,j) = ai(i,j)
         call  comlr(nm,n,low,upp,ar,ai,wr,wi,error)
         write(iwrite,113) error,low,upp  
  113    format(//37h *****error from comlr,low,upp***** = ,3i4)
         call  cinvit(nm,n,asaver,asavei,wr,wi,select,mm,m,zr,zi,
     x                error,rm1,rm2,rv1,rv2)
         write(iwrite,114) error
  114    format(//30h *****error from cinvit***** = ,i4)
         call  combak(nm,low,upp,asaver,asavei,int,m,zr,zi)
         call  cbabk2(nm,n,low,upp,scale,m,zr,zi)
         go to  190
c
c     cgnewz
c
  120    write(iwrite,1203)
 1203    format(41h1the eigenvalues and eigenvectors of the ,
     x     57hcomplex general matrix follow.  this matrix has not been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27helementary transformations.  )
         call  comhes(nm,n,1,n,ar,ai,int)
         call  comlr2(nm,n,1,n,int,ar,ai,wr,wi,zr,zi,error)
         write(iwrite,1205) error
 1205    format(//30h *****error from comlr2***** = ,i4)
         go to  190
c
c     cgnew
c
  130    write(iwrite,1303)
 1303    format(24h1the eigenvalues of the ,
     x     57hcomplex general matrix follow.  this matrix has not been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27helementary transformations.  )
         call  comhes(nm,n,1,n,ar,ai,int)
         call  comlr(nm,n,1,n,ar,ai,wr,wi,error)
         write(iwrite,1305) error
 1305    format(//29h *****error from comlr***** = ,i4)
         go to  190
c
c     cgnew1z
c
  140    write(iwrite,1403)
 1403    format(50h1the eigenvalues and selected eigenvectors of the ,
     x     57hcomplex general matrix follow.  this matrix has not been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     27helementary transformations.  )
         call  comhes(nm,n,1,n,ar,ai,int)
         do  142  i = 1,n
            do  142  j = 1,n
              asaver(i,j) = ar(i,j)
  142         asavei(i,j) = ai(i,j)
         call  comlr(nm,n,1,n,ar,ai,wr,wi,error)
         write(iwrite,143) error
  143    format(//29h *****error from comlr***** = ,i4)
         call  cinvit(nm,n,asaver,asavei,wr,wi,select,mm,m,zr,zi,
     x                error,rm1,rm2,rv1,rv2)
         write(iwrite,144) error
  144    format(//30h *****error from cinvit***** = ,i4)
         call  combak(nm,1,n,asaver,asavei,int,m,zr,zi)
         go to  190
c
c     cguwz
c     invoked from driver subroutine  cg.
c
  160    write(iwrite,1603)
 1603    format(41h1the eigenvalues and eigenvectors of the ,
     x     53hcomplex general matrix follow.  this matrix has been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     24hunitary transformations.  )
         call  cg(nm,n,ar,ai,wr,wi,1,zr,zi,scale,ortr,orti,error)
         write(iwrite,1605) error
 1605    format(//30h *****error from comqr2***** = ,i4)
         go to  190
c
c     cguw
c     invoked from driver subroutine  cg.
c
  165    write(iwrite,1653)
 1653    format(24h1the eigenvalues of the ,
     x     53hcomplex general matrix follow.  this matrix has been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     24hunitary transformations.  )
         call  cg(nm,n,ar,ai,wr,wi,0,ar,ai,scale,ortr,orti,error)
         write(iwrite,1655) error
 1655    format(//29h *****error from comqr***** = ,i4)
         go to  190
c
c     cguw1z
c
  170    write(iwrite,1703)
 1703    format(50h1the eigenvalues and selected eigenvectors of the ,
     x     53hcomplex general matrix follow.  this matrix has been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     24hunitary transformations.  )
         call  cbal(nm,n,ar,ai,low,upp,scale)
         call  corth(nm,n,low,upp,ar,ai,ortr,orti)
         do  172  i = 1,n
            do  172  j = 1,n
              asaver(i,j) = ar(i,j)
  172         asavei(i,j) = ai(i,j)
         call  comqr(nm,n,low,upp,ar,ai,wr,wi,error)
         write(iwrite,173) error,low,upp
  173    format(//37h *****error from comqr,low,upp***** = ,3i4)
         call  cinvit(nm,n,asaver,asavei,wr,wi,select,mm,m,zr,zi,
     x                error,rm1,rm2,rv1,rv2)
         write(iwrite,174) error
  174    format(//30h *****error from cinvit***** = ,i3)
         call  cortb(nm,low,upp,asaver,asavei,ortr,orti,m,zr,zi)
         call  cbabk2(nm,n,low,upp,scale,m,zr,zi)
         go to  190
c
c     cgnuwz
c
  175    write(iwrite,1753)
 1753    format(41h1the eigenvalues and eigenvectors of the ,
     x     57hcomplex general matrix follow.  this matrix has not been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     24hunitary transformations.  )
         call  corth(nm,n,1,n,ar,ai,ortr,orti)
         call  comqr2(nm,n,1,n,ortr,orti,ar,ai,wr,wi,zr,zi,error)
         write(iwrite,1755) error
 1755    format(//30h *****error from comqr2***** = ,i4)
         go to  190
c
c     cgnuw
c
  180    write(iwrite,1803)
 1803 format(24h1the eigenvalues of the ,
     x  57hcomplex general matrix follow.  this matrix has not been /
     x  56h balanced and has been reduced to hessenberg form using ,
     x  24hunitary transformations.  )
         call  corth(nm,n,1,n,ar,ai,ortr,orti)
         call  comqr(nm,n,1,n,ar,ai,wr,wi,error)
         write(iwrite,1805) error
 1805    format(//29h *****error from comqr***** = ,i3)
         go to  190
c
c     cgnuw1z
c
  185    write(iwrite,1853)
 1853    format(50h1the eigenvalues and selected eigenvectors of the ,
     x     57hcomplex general matrix follow.  this matrix has not been /
     x     56h balanced and has been reduced to hessenberg form using ,
     x     24hunitary transformations.  )
         call  corth(nm,n,1,n,ar,ai,ortr,orti)
         do  187  i = 1,n
            do  187  j = 1,n
              asaver(i,j) = ar(i,j)
  187         asavei(i,j) = ai(i,j)
         call  comqr(nm,n,1,n,ar,ai,wr,wi,error)
         write(iwrite,188) error
  188    format(//29h *****error from comqr***** = ,i3)
         call  cinvit(nm,n,asaver,asavei,wr,wi,select,mm,m,zr,zi,
     x                error,rm1,rm2,rv1,rv2)
         write(iwrite,189) error
  189    format(//30h *****error from cinvit***** = ,i3)
         call  cortb(nm,1,n,asaver,asavei,ortr,orti,m,zr,zi)
c
  190    if( icall2 .eq. 0 ) write(iwrite,195)
     x                                   (i,select(i),wr(i),wi(i),i=1,n)
  195    format(//51h all the eigenvalues computed and those eigenvalues
     x          ,9h selected/2(i3,1x,l1,1x,1p2d24.16,2x))
         if( icall2 .ne. 0 ) write(iwrite,200)(i,wr(i),wi(i),i=1,n)
  200    format(//29h all the eigenvalues computed/
     x          2(i3,3x,1p2d24.16,2x))
         if( error .gt. 0 )  go to  300
         if( icall2 .eq. 2 )  go to  300
         call  cmatin(nm,n,ar,ai,arhold,aihold,1)
         if( icall2 .eq. 0 ) call  cgw1zr(nm,n,ar,ai,wr,wi,select,
     x                                    m,zr,zi,norm,resdul)
         if( icall2 .eq. 1 ) call  cgwzr(nm,n,ar,ai,wr,wi,zr,zi,
     x                                   norm,resdul)
         tcrit = resdul/(dfloat(10*n)*machep)
         write(iwrite,220) tcrit,resdul
  220    format(///48h as a guide to evaluating the performance of the,
     x     53h  eispack  codes, a number  x, related to the  1-norm /
     x     56h of the residual matrix, may be normalized to the number,
     x     56h  y  by dividing by the product of the machine precision/
     x     57h machep  and  10*n  where  n  is the order of the matrix.,
     x     36h  the  eispack  codes have performed /
     x     56h (well,satisfactorily,poorly) according as the ratio  y ,
     x     54h is (less than  1, less than  100, greater than  100)./
     x     23h for this run,  y  is =,1pd14.6,11h  with  x =,d14.6)
         if( icall2 .eq. 0 )  go to  270
         do  260  k = 1,n,2
            if( k .eq. n ) go to  240
            write(iwrite,230)
     x                    wr(k),wi(k),wr(k+1),wi(k+1),norm(k),norm(k+1),
     x            (zr(i,k),zi(i,k),zr(i,k+1),zi(i,k+1),i=1,n)
  230      format(///1x,2(19x,15heigenvalue of a,20x)/1x,2(1p2d24.16,6x)
     x       //1x,2(8x,39h1-norm of corresponding residual vector,7x)/
     x       1x,2(6x,26h!!ax-l*x!!/(!!x!!*!!a!!) =,d16.8,6x)//
     x       1x,2(14x,25hcorresponding eigenvector,15x)/( 1x,
     x       2d24.16,6x,2d24.16,6x))
            go to  260
  240       write(iwrite,250)wr(k),wi(k),norm(k),(zr(i,k),zi(i,k),i=1,n)
  250      format(///1x,19x,15heigenvalue of a,20x/1x,1p2d24.16,6x//
     x       1x,8x,39h1-norm of corresponding residual vector,7x/
     x       1x,6x,26h!!ax-l*x!!/(!!x!!*!!a!!) =,d16.8,6x//
     x       1x,14x,25hcorresponding eigenvector,15x/(1x,2d24.16,6x))
  260       continue
         go to  300
  270    l = 1
         k = 0
         do  290  kk = 1,n
           if( .not. select(kk) )  go to  290
           if( k .ne. 0 )  go to  280
           k = kk
           go to  290
  280      write(iwrite,230) wr(k),wi(k),wr(kk),wi(kk),norm(k),norm(kk),
     x                   (zr(i,l),zi(i,l),zr(i,l+1),zi(i,l+1),i=1,n)
           k = 0
           l = l+2
  290      continue
         if( k .ne. 0 )  write(iwrite,250) wr(k),wi(k),norm(k),
     x                       (zr(i,l),zi(i,l),i=1,n)
  300 continue
      go to  10
      end
