subroutine polynomialroots(xGuess , aIn , degree , roots , tolerence)
            implicit none

            real , intent(in) :: xGuess
            integer , intent(in) :: degree
            real , dimension(degree + 1) , intent(in) :: aIn
            real , intent(in) :: tolerence

            real , dimension(degree) , intent(out) :: roots

            real :: x , xPrevious
            real :: error

            integer :: iteration
            integer :: counter

            integer :: unfoundroots

            real , dimension(degree + 1) :: a , b ,c

            a = aIn
            b = 0
            c = 0

            unfoundroots = degree

            do while ( unfoundroots > 0 )
                  do while ( (error > tolerence) .or. (iteration <= 2) )

                        iteration = iteration + 1

                        b(1) = a(1)

                        do counter = 2 , (degree + 1)
                              b(counter) = a(counter) + x * b(counter - 1)
                        end do

                        x = x - b(degree + 1)/c(degree)

                  end do

                  roots() = x

            end do

end subroutine polynomialroots
