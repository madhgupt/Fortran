logical function oppositeSign(number1, number2)
      implicit none

      real , intent(in) :: number1, number2

      oppositeSign = .false.
      
      if ( (number1*number2) < 0 ) then

            oppositeSign = .true.

      end if

end function oppositeSign
