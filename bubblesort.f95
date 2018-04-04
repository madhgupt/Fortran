subroutine bubbleSort(numbers , size)
      implicit none

      integer :: i , c
      integer, intent(in) :: size
      integer, intent(in), dimension(size) :: numbers
      integer, dimension(size) :: sorted

      sorted = numbers

      do

            do i = 1 , (size - 1) , 1

                  if ( sorted(i) > sorted(i + 1) ) then

                        call swapNumbers(sorted(i),sorted(i+1))
                        c = 1

                  end if

            end do

            if ( c == 0 ) then
                  exit
            end if

            c = 0

      end do

      write(*,*) sorted

end subroutine bubbleSort
