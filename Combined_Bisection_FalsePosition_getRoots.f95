real function getrootcombined(xLow , xHigh , tolerance)
      implicit none

      real , external :: getFunction
      logical , external :: oppositeSign

      real , intent(inout) :: xLow , xHigh
      real , intent(in) :: tolerance

      real :: fLow , fHigh , fIntercept
      real :: xMid , fMid , xIntercept , xInterceptprevious
      real :: error

      integer :: iteration = 0

      xInterceptprevious = 0

      fLow = getFunction(xLow)
      fHigh = getFunction(xHigh)

      if ( fLow == 0 ) then
            getrootcombined = xLow
            return
      end if

      if ( fHigh  == 0 ) then
            getrootcombined = xHigh
            return
      end if

      if ( xLow > xHigh ) then
            call swapNumbers(xLow,xHigh)
            call swapNumbers(fLow,fHigh)
      end if

      if ( oppositeSign(fLow , fHigh) .eqv.  .false. ) then
            stop "error (-Root is not in the given domain-)"
      end if

      error = (xHigh - xLow) / 2

      xMid = (xLow + xHigh) / 2
      fMid = getFunction(xMid)

      write(*,10) "iteration" , " | " , "xLow" , " | " , "xHigh" , " | " , "xMid" , "xIntercept"
      10 format ( a10 , a3 , a7 , a3 , a7 ,a3 , a7 ,a3 , a10)

      do while ( (error > tolerance) .or. (iteration <= 2) )

            iteration = iteration + 1

            xIntercept = xHigh - ((fHigh * (xHigh - xLow))/ (fHigh - fLow))
            fIntercept = getFunction(xIntercept)

            write(*,20) iteration ,  " | " , xLow , " | " , xHigh , " | " , xMid , " | " , xIntercept

            if ( oppositeSign(fLow , fIntercept) ) then
                  xHigh = xIntercept
                  fHigh = fIntercept
            else if ( oppositeSign(fHigh , fIntercept) ) then
                  xLow = xIntercept
                  fLow = fIntercept
            else
                  exit
            end if

            error = abs(xIntercept - xInterceptprevious)
            xInterceptprevious = xIntercept


            iteration = iteration + 1

            xIntercept = xHigh - ((fHigh * (xHigh - xLow))/ (fHigh - fLow))
            fIntercept = getFunction(xIntercept)

            write(*,20) iteration ,  " | " , xLow , " | " , xHigh , " | " , xMid , " | " , xIntercept
            20 format ( i4 ,a3 , f7.2, a3 , f7.2,a3 , f7.2 , a3 , f7.2 )

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

            error = abs((xHigh - xLow) / 2)



      end do

      getrootcombined = xMid

end function getrootcombined
