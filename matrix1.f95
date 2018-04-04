program matrixmultiplication
      implicit none

      real , dimension(:,:)  , allocatable :: a 
      integer :: n , i , u , t

      read(*,*) n

      allocate(a(n , n))

      write(*,*) "enter the first matrix"

      do i = 1, n , 1
            do u = 1, n , 1
                  read(*,*) a(i , u)
            end do
      end do

      call printMatrix2D(a , n , n)

end program matrixmultiplication
