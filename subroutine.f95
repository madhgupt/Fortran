subroutine statistics(number1 , number2 , sumOfnumbers , productOfnumbers , averageOfnumbers )
      implicit none

      real , intent(in) :: number1 , number2
      real , intent(in) :: sumOfnumbers , productOfnumbers , averageOfnumbers

      sumOfnumbers = number1 + number2
      productOfnumbers = number1 * number2
      averageOfnumbers = sumOfnumbers/2

end subroutine statistics
