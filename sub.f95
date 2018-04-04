program mainProgram
      implicit none

      !real , external :: sumOf
      !real , external :: productOf

      real :: number1, number2, sumOf, productOf , averageOf

      write (*,*) "Hey I am Fortran running on Madhur's MacBook Pro"

      read (*,*) number1 , number2

      call statistics(number1,number2)

      !sumOOf = sumOf(number1 , number2)
      !productOOf = productOf(number1, number2)

      write (*,10) "integer 1 = " , number1
      write (*,10) "integer 2 = " , number2
      10 format(a12, f4.2)

      !write(*,20) "sum of " , number1 , "and " , number2 , "is " , sumOfnumbers
      !write(*,20) "product of " ,number1 , "and " , number2 , "is " , productOfnumbers
      !write(*,20) "Average of " ,number1 , "and " , number2 , "is " , averageOfnumbers

      !write (*,*) sumOOf
      !write (*,*) productOf

 end program mainProgram

 subroutine statistics(a ,b)
       implicit none

       real , intent(in) :: a , b
       real :: sumOfnumbers , productOfnumbers , averageOfnumbers

       sumOfnumbers = a + b
       productOfnumbers = a * b
       averageOfnumbers = sumOfnumbers/2

       write(*,20) "sum of " , a , "and " , b , "is " , sumOfnumbers
       write(*,20) "product of " ,a , "and " , b , "is " , productOfnumbers
       write(*,20) "Average of " ,a , "and " , b , "is " , averageOfnumbers

       20 format(a11, f7.2 , a5 , f7.2 , a4 , f7.2 )


 end subroutine statistics
