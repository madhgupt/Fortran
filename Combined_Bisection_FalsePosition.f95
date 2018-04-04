program mainProgram
      implicit none

      real , external :: getrootcombined

      real :: xLow , xHigh
      real , parameter :: tolerance = 1.0e-6
      real :: root

      xLow = 1
      xHigh = 2
      root = getrootcombined(xLow,xHigh,tolerance)

      write(*,*) "Root by the False position method =" , root

end program mainProgram
