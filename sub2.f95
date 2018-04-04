program mainProgram
      implicit none

      real :: number1 , number2 , sumOf , productOf , avgOf

      write(*,*) "Enter any two numbers"

      read(*,*) number1 ,number2

      call math( number1, number2, sumOf, productOf, avgOf)

      write(*,20) "sum of " , number1 , "and " , number2 , "is " , sumOf
      write(*,20) "product of " ,number1 , "and " , number2 , "is " , productOf
      write(*,20) "Average of " ,number1 , "and " , number2 , "is " , avgOf

      20 format(a11, f7.2 , a5 , f7.2 , a4 , f7.2 )

end program mainProgram

subroutine math( num1, num2 , sum , product , avg )
      implicit none

      real :: num1, num2, sum, product, avg

      sum = num1 + num2
      product = num1 * num2
      avg = sum / 2

end subroutine math
