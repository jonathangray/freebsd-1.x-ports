      subroutine elmbak(nm,low,igh,a,int,m,z)
c
      integer i,j,m,la,mm,mp,nm,igh,kp1,low,mp1
      double precision a(nm,igh),z(nm,m)
      double precision x
      integer int(igh)
c
c     this subroutine is a translation of the algol procedure elmbak,
c     num. math. 12, 349-368(1968) by martin and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
c
c     this subroutine forms the eigenvectors of a real general
c     matrix by back transforming those of the corresponding
c     upper hessenberg matrix determined by  elmhes.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        low and igh are integers determined by the balancing
c          subroutine  balanc.  if  balanc  has not been used,
c          set low=1 and igh equal to the order of the matrix.
c
c        a contains the multipliers which were used in the
c          reduction by  elmhes  in its lower triangle
c          below the subdiagonal.
c
c        int contains information on the rows and columns
c          interchanged in the reduction by  elmhes.
c          only elements low through igh are used.
c
c        m is the number of columns of z to be back transformed.
c
c        z contains the real and imaginary parts of the eigen-
c          vectors to be back transformed in its first m columns.
c
c     on output
c
c        z contains the real and imaginary parts of the
c          transformed eigenvectors in its first m columns.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      if (m .eq. 0) go to 200
      la = igh - 1
      kp1 = low + 1
      if (la .lt. kp1) go to 200
c     .......... for mp=igh-1 step -1 until low+1 do -- ..........
      do 140 mm = kp1, la
         mp = low + igh - mm
         mp1 = mp + 1
c
         do 110 i = mp1, igh
            x = a(i,mp-1)
            if (x .eq. 0.0d0) go to 110
c
            do 100 j = 1, m
  100       z(i,j) = z(i,j) + x * z(mp,j)
c
  110    continue
c
         i = int(mp)
         if (i .eq. mp) go to 140
c
         do 130 j = 1, m
            x = z(i,j)
            z(i,j) = z(mp,j)
            z(mp,j) = x
  130    continue
c
  140 continue
c
  200 return
      end
