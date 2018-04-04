subroutine swapNumbers(number1 , number2)
      implicit none

      real , intent(inout):: number1 , number2
      real :: swap

      swap = number1
      number1 = number2
      number2 = swap

end subroutine swapNumbers
