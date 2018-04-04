program mainProgram
      implicit none

            integer, dimension(:), allocatable :: numbers
            integer :: u , i , t , c

            write(*,*) "How many numbers"
            read(*,*) u
            write(*,*) "Enter the numbers"

            allocate(numbers(u))

            do i = 1,u,1

                  read(*,*) numbers(i)

            end do

            do

                  do i = 1 , u-1 , 1

                        if ( numbers(i) > numbers(i+1) ) then

                              t = numbers(i)
                              numbers(i) = numbers(i+1)
                              numbers(i+1) = t

                              c = 1

                        end if

                  end do

                  if ( c == 0 ) then
                        exit
                  end if

                  c = 0

            end do

            write(*,*) "the accending order of the following numbers will be"

            do i = 1, u, 1

                  write(*,*) numbers(i)

            end do


end program mainProgram
