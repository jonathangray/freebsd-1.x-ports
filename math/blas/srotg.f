      subroutine srotg(sa,sb,c,s)
c
c     construct givens plane rotation.
c     jack dongarra, linpack, 3/11/78.
c                    modified 9/27/86.
c
      real sa,sb,c,s,roe,scale,r,z
c
      roe = sb
      if( abs(sa) .gt. abs(sb) ) roe = sa
      scale = abs(sa) + abs(sb)
      if( scale .ne. 0.0 ) go to 10
         c = 1.0
         s = 0.0
         r = 0.0
         go to 20
   10 r = scale*sqrt((sa/scale)**2 + (sb/scale)**2)
      r = sign(1.0,roe)*r
      c = sa/r
      s = sb/r
   20 z = s
      if( abs(c) .gt. 0.0 .and. abs(c) .le. s ) z = 1.0/c
      sa = r
      sb = z
      return
      end
