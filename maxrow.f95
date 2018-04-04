integer function maxrow(aIn , n , pivotRow)
      implicit none

      integer , intent(in) :: n , pivotRow
      real , dimension(n,n+1) , intent(in) :: aIn
      integer :: t , i

      maxrow = pivotRow

      do i = pivotRow, n , 1
            if (abs(aIn(i , pivotRow)) > abs(aIn(maxrow , pivotRow))) then
                  maxrow = i
            end if
      end do

end function maxrow
