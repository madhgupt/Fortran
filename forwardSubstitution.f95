subroutine forwardSubstitution(L , n , b , x)
      implicit none

      integer , intent(in) :: n
      real , dimension(n,n) , intent(in) :: L
      real , dimension(n) , intent(in) :: b

      real , dimension(n) , intent(out) :: x
      real :: k
      integer :: i  , u

      x(1) = b(1)/L(1,1)

      do i = 2, n, 1
            k = 0

            do u = 1, (i-1), 1
                  k = k + L(i , u)*x(u)
            end do

            x(i) = (b(i) - k)/L(i,i)
      end do

end subroutine forwardSubstitution
