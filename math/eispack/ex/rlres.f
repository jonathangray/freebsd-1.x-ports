      subroutine rlres(nm,m,n,a,u,s,v,norm,resdul)
c
      double precision a(nm,n),u(nm,n),v(nm,n),s(n),norm(n)
      double precision sum1,sum2,x,resdul,norma,suma,sumuv
c
c     this subroutine performs a residual test for the decomposition
c                              t
c     of a matrix  a  into  usv .
c
c     this subroutine is catalogued as eispdrv4(rlres).
c
c     input.
c
c       nm  is the row dimension of two-dimensional array parameters
c         as declared in the calling program dimension statement;
c
c       m  is the row dimension of the matrices  a  and  u.
c
c       n  is the column dimension of the matrices  a  and u , and
c         the order of  v;
c
c       a  is a real general matrix of dimemsion  m  by  n;
c
c       u  is a matrix with orthogonal columns of dimension  m  by  n;
c
c       s  is a diagonal matrix stored as a vector of dimension  n;
c
c       v  is an orthogonal matrix of dimension  n  by  n.
c
c     output.
c
c       norm  is an array such that for each k,
c                                              t
c         norm(k) = !!a*v(k)-s(k)*u(k)!!  + !!a *u(k)-s(k)*v(k)!!
c                   ---------------------------------------------
c                           !!a!!*(!!u(k)!! + !!v(k)!!)
c
c       resdul  is the largest element of  norm.
c
c     ------------------------------------------------------------------
c
      resdul = 0.0d0
      norma = 0.0d0
c
      do 20 i = 1,m
         suma = 0.0d0
c
         do 10 j = 1,n
            suma = suma + dabs(a(i,j))
   10    continue
c
         norma = dmax1(norma,suma)
   20 continue
c
      if( norma .eq. 0.0d0 ) norma = 1.0d0
c
      do 70 k = 1,n
         sumuv = 0.0d0
         sum1 = 0.0d0
c
         do 40 i = 1,m
            sumuv = sumuv + dabs(u(i,k))
            x = -s(k)*u(i,k)
c
            do 30 j = 1,n
               x = x + a(i,j)*v(j,k)
   30       continue
c
            sum1 = sum1 + dabs(x)
   40    continue
c
         sum2 = 0.0d0
c
         do 60 i = 1,n
            sumuv = sumuv + dabs(v(i,k))
            x = -s(k)*v(i,k)
c
            do 50 j = 1,m
               x = x + a(j,i)*u(j,k)
   50       continue
c
            sum2 = sum2 + dabs(x)
   60    continue
c
         norm(k) = (sum1 + sum2)/(sumuv*norma)
         resdul = dmax1(norm(k),resdul)
   70 continue
c
      return
      end
