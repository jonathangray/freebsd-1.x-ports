      subroutine rblres(nm,n,mbw,ip,a,b,x,norm,e21)
c
      double precision a(nm,mbw),b(nm,ip),x(nm,ip),norm(n)
      double precision norma,normx,normr,sum,suma,e21
c
c     this subroutine forms the 1-norm of the residual matrix
c     a*x-b  where  a  is a real band matrix, which may be symmetric,
c     x  is a real  n  by  ip  matrix, and   b is a real
c     n  by  ip  matrix.
c
c     this subroutine is catalogued as eispdrv4(rblres).
c
c     input.
c        nm is the row dimension of two-dimensional array parameters
c           as declared in the calling program dimension statement;
c
c        n is the row dimension of the matrix  a;
c
c        mbw is the number of columns of the array a used to store the
c           band matrix.  if the matrix is symmetric, mbw is its (half)
c           band width, denoted mb and defined as the number of adjacent
c           diagonals, including the principal diagonal, required to
c           specify the non-zero portion of the lower triangle of the
c           matrix.  if the matrix is non-symmetric it must have the sam
c           number of adjacent diagonals above the main diagonal as
c           below.  in this case  mbw = 2*mb-1;
c
c        ip  is the column dimension of  b  and  x;
c
c        a(n,mbw) is an array which contains in its columns the
c           subdiagonals and diagonal of the symmetric band
c           matrix. if  a  is non-symmetric it also contains
c           the super-diagonals;
c
c        b  is a real full  n  by  ip  matrix;
c
c        x  is a real full  n  by  ip  matrix;
c
c        e21 tells if the matrix is symmetric.  if e21 equals one the
c           matrix is symmetric and if e21 equals minus one the
c           matrix is non-symmetric.
c
c     output.
c
c        norm(n)  is an array such that for each k,
c               norm(k) = !!a*x(k)-b(k)!!/(!!a!!*!!x(k)!!) .
c
c     ------------------------------------------------------------------
c
      if( e21 .eq. 1.0d0 ) go to 75
      mbwt = mbw
      mb = mbw/2 + 1
      norma = 0.0d0
c
      do 35 i = 1,n
         l = max0(mb - i + 1,1)
         m = min0(mb - i + n,mbwt)
         suma = 0.0d0
c
         do 34 j = l,m
            suma = suma + dabs(a(i,j))
   34    continue
c
         norma = dmax1(norma,suma)
   35 continue
c
      do 70 ipp = 1,ip
         normr = 0.0d0
         normx = 0.0d0
         do 50 i = 1,n
            l = max0(mb - i + 1,1)
            m = min0(mb - i + n,mbwt)
            k = max0(-mb + 1 + i,1)
            sum = -b(i,ipp)
            suma = 0.0d0
c
            do 40 j = l,m
               sum = sum + a(i,j)*x(k,ipp)
               k = k + 1
               suma = suma + dabs(a(i,j))
   40       continue
c
            normr = normr + dabs(sum)
            normx = normx + dabs(x(i,ipp))
   50    continue
c
         if( normx .eq. 0.0d0 ) normx = 1.0d0
         if( norma .eq. 0.0d0 ) norma = 1.0d0
         norm(ipp) = normr/(norma*normx)
   70 continue
c
      return
c
   75 mb = mbw
      norma = 0.0d0
      mb1 = mb - 1
c
      do 110 i = 1,n
         suma = 0.0d0
         if( i .eq. 1 ) go to 90
         lstart = max0(1,mb + 1 - i)
c
         do 80 l = lstart,mb1
            suma = suma + dabs(a(i,l))
   80    continue
c
   90    lstop = min0(mb,n + 1 - i)
         j = i
c
         do 100 l = 1,lstop
            l1 = mb + 1 - l
            suma = suma + dabs(a(j,l1))
            j = j + 1
  100    continue
c
         norma = dmax1(norma,suma)
  110 continue
c
      do 170 i = 1,ip
         normr = 0.0d0
         normx = 0.0d0
c
         do 150 l = 1,n
            sum = -b(l,i)
            j = max0(0,l - mb)
            if( l .eq. 1 ) go to 130
            kstart = max0(1,mb + 1 - l)
c
            do 120 k = kstart,mb1
               j = j + 1
               sum = sum + a(l,k)*x(j,i)
  120       continue
c
  130       kstop = min0(mb,n + 1 - l)
c
            do 140 k = 1,kstop
               j = j + 1
               k1 = mb + 1 - k
               sum = sum + a(j,k1)*x(j,i)
  140       continue
c
            normr = normr + dabs(sum)
  150    continue
c
         do 160 k = 1,n
            normx = normx + dabs(x(k,i))
  160    continue
c
         if( normx .eq. 0.0d0 ) normx = 1.0d0
         if( norma .eq. 0.0d0 ) norma = 1.0d0
         norm(i) = normr/(norma*normx)
  170 continue
      return
c
      end
