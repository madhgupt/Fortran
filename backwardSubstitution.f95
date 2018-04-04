subroutine backwardSubstitution(L , n , b , x)
      implicit none

      integer , intent(in) :: n
      real , dimension(n,n) , intent(in) :: U
      real , dimension(n) , intent(in) :: b

      real , dimension(n) , intent(out) :: x
      real :: k
      integer :: i  , t

      x(1) = b(1)/L(1,1)

      do i = 2, n, 1
            k = 0

            do t = 1, (i-1), 1
                  k = k + U(i , t)*x(t)
            end do

            x(i) = (b(i) - k)/U(i,i)
      end do

end subroutine backwardSubstitution
