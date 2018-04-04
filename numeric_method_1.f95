program mainProgram
  implicit none

    integer , external :: sumOf
    integer , external :: productOf

    integer :: number1, number2, sumOOf, productOOf

    write (*,*) "Hey I am Fortan running on Madhur's MacBook Pro"

    read (*,*) number1 , number2

    sumOOf = sumOf(number1 , number2)
    productOOf = productOf(number1, number2)

    write (*,10) "integer 1 = " , number1
    write (*,10) "integer 2 = " , number2
    10 format(a12, i1)

    write(*,20) "sum of " , number1 , "and " , number2 , "is " , sumOOf
    write(*,20) "product of " ,number1 , "and " , number2 , "is " , productOOf

    20 format(a11, i4 , a5 , i4 , a4 , i4 )

    !write (*,*) sumOOf
    !write (*,*) productOf

end program mainProgram
