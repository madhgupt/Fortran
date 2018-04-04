subroutine inverse(aIn,n,x)
      implicit none

      integer , external :: maxrow

      integer , intent(in) :: n
      real , dimension(n,n) , intent(in) :: aIn
      real , dimension(n,n) , intent(inout) :: x

      real , dimension(n,n) :: a
      integer :: stepCount , pivotRow , pivotColumn , column , row , max , i
      real :: pivot , factor

      a = aIn

      do stepCount = 1, n , 1
            pivotRow = stepCount
            pivotColumn = stepCount

            if (a(pivotRow , pivotColumn) == 0) then
                  max = maxrow(a,n,pivotRow)
                  call rowsswapper(a,n,stepCount,max)
            end if

            pivot = a(pivotRow , pivotColumn)

            do row = 1, n, 1
                  if(row /= pivotRow) then
                        factor = (a(row , pivotColumn)/a(pivotRow , pivotColumn))

                        do column = 1, n , 1
                              a(row,column) = a(row,column) - (factor*a(pivotRow,column))
                              x(row,column) = x(row,column) - (factor*x(pivotRow,column))
                        end do
                  end if
            end do
      end do


      do stepCount = 1, n , 1
            pivotRow = stepCount
            pivotColumn = stepCount

            pivot = a(pivotRow , pivotColumn)

            factor = (1/pivot)
            a(pivotRow, pivotColumn) = a(pivotRow, pivotColumn)*factor
            do  i = 1, n, 1
                  x(stepCount, i) = x(stepCount, i)*factor
            end do
      end do

end subroutine inverse
