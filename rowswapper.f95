subroutine rowsswapper(aIn , n , stepCount , maxrow)
      implicit none

      integer , intent(in) :: n
      real , intent(inout) , dimension(n , n+1) :: aIn
      integer , intent(in) :: stepCount , maxrow

      real :: temp
      integer :: i

      do i = stepCount , n+1 , 1
            temp = aIn(stepCount , i)
            aIn(stepCount , i) = aIn(maxrow, i)
            aIn(maxrow , i) = temp           
      end do

end subroutine rowsswapper
