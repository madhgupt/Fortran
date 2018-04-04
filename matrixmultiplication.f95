subroutine matmula(a, b, n , c)
      implicit none

      integer, intent(in) :: n
      real , dimension(n,n) :: a , b
      real , dimension(n,n) , intent(out) :: c
      integer :: i , u , t

      c = 0

      do i = 1, n , 1
            do u = 1, n , 1
                  do t = 1 , n , 1
                        c(i , u) = c(i , u) + a(i , t)*b(t , u)
                  end do
            end do
      end do

      call printMatrix2D(c , n , n)

end subroutine matmula
