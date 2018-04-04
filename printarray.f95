subroutine printtwoarray(array1 , array2 , size)
      implicit none

      integer :: i
      integer , intent(in) :: size
      character(len=20), intent(in) , dimension(size) :: array1
      integer , intent(in) , dimension(size) :: array2

      do i = 1, size, 1

            write(*,*) array1(i) , " | " , array2(i)

      end do

      10 format(a10 , a3 , i4)

end subroutine printtwoarray
