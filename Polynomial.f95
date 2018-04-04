program mainProgram
      implicit none

      real :: x , root
      real , parameter :: tolerence = 1.0e-5

      call polynomialroots( x , tolerence )

      write(*,*) "Root = " , root

end program mainProgram
