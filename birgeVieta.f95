subroutine birgeVieta(xGuess , aIn , degree , tolerance , roots)
      implicit none

      real , intent(in) :: xGuess
      integer , intent(in) :: degree
      real , intent(in) , dimension(degree+1) :: aIn

      real , intent(in) :: tolerance

      real , dimension(degree) , intent(out) :: roots

      real , dimension(:) , allocatable :: a , b , c
      real :: x , xP , error

      integer :: unfound , i , count

      i = 0

      allocate(a(degree+1))
      allocate(b(degree+1))
      allocate(c(degree))

      a = aIn
      b = 0
      c = 0

      unfound = degree

      do while ( unfound > 0 )
            write(*,*) "Finding Root " , (degree - unfound + 1)

            write(*,*) "Coeff. of P(x)"
            do count = 1, size(a), 1
                  write(*,*) a(count)
            end do

            i = 0
            x = xGuess
            do while ((error >= tolerance) .or. (i<=2))
                  i = i+1

                  b(1) = a(1)
                  do count = 2, size(b) , 1
                        b(count) = a(count) + (x*b(count-1))
                  end do

                  c(1) = b(1)
                  do count = 2, size(c), 1
                        c(count) = b(count) + (x*c(count-1))
                  end do

                  if ( c(size(c)) == 0 ) then
                        stop "--ERROR-- derivative in Newton Raphson zero"
                  end if

                  x = x - (b(size(b))/c(size(c)))

                  error = abs(x-xP)
                  xP = x
            end do

            unfound = unfound-1
            roots(degree - unfound) = x
            write(*,*)"Root " , (degree - unfound) , x

            deallocate(a)
            allocate(a(unfound+1))
            do count = 1, unfound+1, 1
                  a(count) = b(count)
            end do

            deallocate(b)
            deallocate(c)
            allocate(b(unfound+1))
            allocate(c(unfound))

            b = 0
            c = 0
      end do

      write(*,*)

end subroutine birgeVieta
