subroutine swapNumbers(number1 , number2)
      implicit none

      integer :: number1 , number2
      integer :: swap

      swap = number1
      number1 = number2
      number2 = swap

end subroutine swapNumbers
