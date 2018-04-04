real function getrootposition(xLow, xHigh, tolerance)
      implicit none

      real , external :: getFunction
      logical , external :: oppositeSign

      real , intent(inout) :: xLow , xHigh
      real , intent(in) :: tolerance
      real :: fHigh , fLow , xIntercept , fIntercept , xInterceptprevious , error

      integer :: iteration = 0


      fLow = getFunction(xLow)
      fHigh = getFunction(xHigh)

      if ( fLow == 0 ) then
            getrootposition = xLow
            return
      end if

      if ( fHigh == 0 ) then
            getrootposition = xHigh
            return
      end if

      if ( oppositeSign(fLow,fHigh) .eqv. .false. ) then
                  stop "Error (False Position)"
      end if

      do while ((error > tolerance) .or. (iteration <= 2))

            iteration = iteration + 1

            xIntercept = xHigh - ((fHigh * (xHigh - xLow))/ (fHigh - fLow))
            fIntercept = getFunction(xIntercept)

            write(*,*) iteration , xLow , xHigh , xIntercept

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

      end do

      getrootposition = xIntercept

end function getrootposition
