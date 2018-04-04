real function sumOfarray(numbers , size)
      implicit none

      integer :: i
      integer, intent(in) :: size
      real, dimension(size) , intent(in) :: numbers

      sumOfarray = 0

      do i = 1, size, 1

            sumOfarray = sumOfarray + numbers(i)

      end do

end function sumOfarray
