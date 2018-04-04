program mainProgram
      implicit none

            integer, dimension(:), allocatable :: numbers , sorted
            integer :: u , i

            write(*,*) "How many numbers"
            read(*,*) u
            write(*,*) "Enter the numbers"

            allocate(numbers(u))
            allocate(sorted(u))

            do i = 1,u,1

                  read(*,*) numbers(i)

            end do

            call bubbleSort(numbers,u)

            write(*,*) "the accending order of the following numbers will be"

            !write(*,*) sorted

end program mainProgram
