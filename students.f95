program mainProgram
      implicit none

            integer, dimension(:), allocatable :: numbers , numSort
            character(len=20 ), dimension(:), allocatable :: names , nSort
            integer :: u , i , t , c , q

            write(*,*) "How many Students"
            read(*,*) u

            allocate(numbers(u))
            allocate(names(u))
            allocate(numSort(u))
            allocate(nSort(u))

            !numSort = numbers

            do i = 1,u,1

                  write(*,*) "Enter the name of the student"
                  read(*,*) names(i)
                  write(*,*) "Enter the marks"
                  read(*,*) numbers(i)
                  numSort(i) = numbers(i)

            end do

            write(*,*) " Before Sorting"

            call printtwoarray(names,numbers,u)

            c = 0

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

            do q=1, u

                  do i = 1 , u , 1

                        if ( numbers(q) == numSort(i) ) then

                              nSort(q) = names(i)
                              numSort(i) = -1

                              c = 1

                              exit

                        end if

                  end do

                  if ( c == 0 ) then
                        exit
                  end if

                  c = 0

            end do

            write(*,*) "After Sorting"

            call printtwoarray(nSort,numbers,u)

            10 format (a10 , a3 , i4)

end program mainProgram
