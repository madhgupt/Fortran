subroutine matrixProduct(a, b, rowA, columnA, rowB, columnB ,ab)
      implicit none

      integer, intent(in) :: rowA , columnA , rowB , columnB
      real , dimension(rowA, columnA), intent(in) :: a
      real , dimension(rowB, columnB), intent(in) :: b

      real , dimension(rowA, columnB), intent(out) :: ab
      integer :: i , j , u

      ab = 0

      if ( columnA /= rowB ) then
            stop "ERROR: multiplication not possible"
      end if

      do i = 1 , rowA ,1
            do j = 1, columnB, 1
                  do u = 1, columnA, 1
                        ab(i,j) = a(i,j) + (a(i,u)*b(u,j))
                  end do
            end do
      end do

end subroutine matrixProduct
