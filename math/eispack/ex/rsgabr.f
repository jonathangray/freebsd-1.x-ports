      subroutine rsgabr(nm,n,m,a,b,w,z,norm,resdul,r)
c
      double precision norm(m),w(m),a(nm,n),z(nm,m),norma,tnorm,s,sum, 
     x       sumz, suma, resdul, b(nm,n), normb, sumb, r(n)
c
c     this subroutine forms the 1-norm of the residual matrix
c     a*b*z-z*diag(w)  where  a  is a real symmetric matrix,
c     b  is a real symmetric matrix ,  w  is a vector which
c     contains  m  eigenvalues of the eigenproblem, a*b*z-z*diag(w),
c     and  z  is an array which contains the  m  corresponding
c     eigenvectors of the eigenproblem. all norms appearing in
c     the comments below are 1-norms.
c
c     this subroutine is catalogued as eispdrv4(rsgabr).
c
c     input.
c
c        nm is the row dimension of two-dimensional array parameters
c           as declared in the calling program dimension statement;
c
c        m is the number of eigenvectors for which residuals are
c           desired;
c
c        n is the order of the matrix  a;
c
c        a(nm,n) is a real symmetric matrix.  only the full upper
c           triangle need be supplied;
c
c        b(nm,n) is a real symmetric matrix.  only the full upper
c           triangle need be supplied;
c
c        z(nm,m) is a real matrix whose first  m  columns contain the
c          approximate eigenvectors of the eigenproblem;
c
c        w(m) is a vector whose first  m   components contain the
c           approximate eigenvalues of the eigenproblem.  w(i) is
c           associated with the i-th  column of  z.
c
c
c     output.
c
c        z(nm,m) is an array which contains  m  normalized
c           approximate eigenvectors of the eigenproblem. the
c           eigenvectors are normalized using the 1-norm in such a way
c           that the first element whose magnitude is larger than the
c           norm of the eigenvector divided by  n  is positive;
c
c        a(nm,n) is altered by making the lower triangle of  a
c           symmetric with the upper triangle of  a;
c
c        b(nm,n) is altered by making the lower triangle of  b
c           symmetric with the upper triangle of  b;
c
c        norm(m) is an array such that for each  k,
c           norm(k) = !!a*b*z(k)-z(k)*w(k)!!/(!!a!!*!!b!!*!!z(k)!!)
c           where  z(k)  is the k-th eigenvector;
c
c        resdul is the real number
c           !!a*b*z-z*diag(w)!!/(!!a!!*!!b!!*!!z!!);
c
c        r(n) is a temporary storage array used to store the product
c           b*z.
c
c     ----------------------------------------------------------------
c
      resdul = 0.0d0
      if( m .eq. 0 ) return
      norma = 0.0d0
      normb = 0.0d0
c
      do 40 i=1,n
         sumb = 0.0d0
         suma = 0.0d0
         if(i .eq. 1) go to 20
c
         do 10 l=2,i
           a(i,l-1) =a(l-1,i)
           b(i,l-1) =b(l-1,i)
           sumb =sumb + dabs(b(l-1,i))
   10      suma =suma + dabs(a(l-1,i))
c
   20    do 30 l=i,n
           suma =suma + dabs(a(i,l))
   30      sumb =sumb + dabs(b(i,l))
c
         norma = dmax1(norma,suma)
   40    normb = dmax1(normb,sumb)
c
      norma = norma*normb
      if(norma .eq. 0.0d0) norma = 1.0d0
c
      do 100 i=1,m
         s = 0.0d0
         sumz = 0.0d0
         do 55 l = 1,n
           sum = 0.0d0
           do 50 k = 1,n
   50        sum = sum + b(l,k)*z(k,i)
           sumz = sumz + dabs(z(l,i))
   55      r(l) = sum
c
         do 65 l = 1,n
           sum = - w(i)*z(l,i)
           do 60 k = 1,n
   60        sum = sum + r(k)*a(l,k)
   65      s = s + dabs(sum)
         norm(i) = sumz
         if(sumz .eq. 0.0d0) go to 100
c        ..........this loop will never be completed since there
c                  will always exist an element in the vector z(i)
c                  larger than !!z(i)!!/n..........
         do 70 l=1,n
            if(dabs(z(l,i)) .ge. norm(i)/dfloat(n)) go to 80
   70       continue
c
   80    tnorm = dsign(norm(i),z(l,i))
c
         do 90 l=1,n
   90       z(l,i) = z(l,i)/tnorm
c
         norm(i) = s/(norm(i)*norma)
  100    resdul = dmax1(norm(i), resdul)
c
      return
      end
