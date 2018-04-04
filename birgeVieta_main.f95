program main
      implicit none

      integer :: degree
      real , dimension(:) , allocatable :: a , roots

      real , parameter :: tolerance = 1.0e-6
      real :: xGuess

      integer :: i

      write(*,*) "Birge Vieta Method"
      write(*,*)

      write(*,*) "enter the degree of the polynomial"
      read(*,*) degree

      allocate(a(degree+1))
      allocate(roots(degree))

      do i = 1, degree+1 , 1
            write(*,10,advance='no')"enter the coeff. of "
            read(*,*) a(i)
      end do

      write(*,*)

      xGuess = 1000

      call birgeVieta(xGuess , a , degree , tolerance , roots)

      write(*,*) "Roots of polynomial:"

      do i = 1, degree, 1
            write(*,20,advance="no") roots(i)
      end do

      write(*,*)

      10 format(a22)
      20 format(f7.4)

end program main
