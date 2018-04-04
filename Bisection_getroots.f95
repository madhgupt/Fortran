real function getroots(xLow , xHigh , tolerance)
      implicit none

      real , external :: getFunction
      logical , external :: oppositeSign

      real , intent(inout) :: xLow , xHigh
      real , intent(in) :: tolerance

      real :: fLow , fHigh
      real :: xMid , fMid

      real :: error
      integer :: iter = 0

      fLow = getFunction(xLow)
      fHigh = getFunction(xHigh)

      if ( fLow == 0 ) then
            getroots = xLow
            return
      end if

      if ( fHigh  == 0 ) then
            getroots = xHigh
            return
      end if

      if ( xLow > xHigh ) then
            call swapNumbers(xLow,xHigh)
            call swapNumbers(fLow,fHigh)
      end if

      if ( oppositeSign(fLow , fHigh) .eqv.  .false. ) then
            stop "error (Bisection)"
      end if

      error = (xHigh - xLow) / 2

      xMid = (xLow + xHigh) / 2
      fMid = getFunction(xMid)

      write(*,10) "iter" , " | " , "xLow" , " | " , "xHigh" , " | " , "xMid"
      10 format ( a4 , a3 , a7 , a3 , a7 ,a3 , a7 ,a3 , a7)

      do while ( error > tolerance )

            iter = iter + 1

            write(*,20) iter ,  " | " , xLow , " | " , xHigh , " | " , xMid
            20 format ( i4 ,a3 , f7.2, a3 , f7.2,a3 , f7.2,a3 )

            xMid = (xLow + xHigh) / 2
            fMid = getFunction(xMid)

            if ( oppositeSign(fMid, fLow) ) then
                  xHigh = xMid
                  fHigh = fMid
            else if ( oppositeSign(fMid,fHigh) ) then
                        xLow = xMid
                        fLow = fMid
            else
                  exit
            end if

            error = (xHigh - xLow) / 2

      end do

      getroots = xMid

end function getroots
