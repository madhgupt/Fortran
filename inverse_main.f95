program main
      implicit none

      integer :: n
      real, dimension(:,:), allocatable :: a
      real, dimension(:,:), allocatable :: x

      integer :: rowCount

      n = 5
      allocate(a(n,n))
      allocate(x(n,n))

      a(1,1) = 1
        a(1,2) = -1
        a(1,3) = 2
        a(1,4) = -3
        a(1,5) = 4

        a(2,1) = 2
        a(2,2) = 3
        a(2,3) = -1
        a(2,4) = 5
        a(2,5) = -2

        a(3,1) = -1
        a(3,2) = 3
        a(3,3) = 2
        a(3,4) = -5
        a(3,5) = 1

        a(4,1) = 1
        a(4,2) = 2
        a(4,3) = 1
        a(4,4) = 2
        a(4,5) = 3

        a(5,1) = -4
        a(5,2) = -6
        a(5,3) = -2
        a(5,4) = 8
        a(5,5) = -3

      x(1,1) = 1
      x(1,2) = 0
      x(1,3) = 0
      x(1,4) = 0
      x(1,5) = 0

      x(2,1) = 0
      x(2,2) = 1
      x(2,3) = 0
      x(2,4) = 0
      x(2,5) = 0

      x(3,1) = 0
      x(3,2) = 0
      x(3,3) = 1
      x(3,4) = 0
      x(3,5) = 0

      x(4,1) = 0
      x(4,2) = 0
      x(4,3) = 0
      x(4,4) = 1
      x(4,5) = 0

      x(5,1) = 0
      x(5,2) = 0
      x(5,3) = 0
      x(5,4) = 0
      x(5,5) = 1

      call inverse(a, n, x)

      write(*,*) "Inverse"

      call printMatrix2D(x,n,n)

end program main
