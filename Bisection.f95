program mainProgram
      implicit none

      real , external :: getroots

      real :: xLow , xHigh
      real , parameter :: tolerance = 1.0e-6
      real :: root

      xLow = 1
      xHigh = 2

      root = getroots(xLow,xHigh,tolerance)
      write(*,*) "Root = " , root

end program mainProgram
