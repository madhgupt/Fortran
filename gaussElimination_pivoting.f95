subroutine gaussEliminationPivoting(aIn,n,x)
      implicit none

      integer , external :: maxrow

      integer , intent(in) :: n
      real , intent(in) , dimension(n , n+1) :: aIn
      real , intent(out) , dimension(n) :: x

      real , dimension(n, n+1) :: a
      integer :: pivotRow , pivotColumn , pivot , max , stepCount , column , row , i
      real :: factor = 0

      a = aIn

      do stepCount = 1, n-1 , 1
            pivotRow = stepCount
            pivotColumn = stepCount

            if (a(pivotRow , pivotColumn) == 0) then
                  max = maxrow(a,n,pivotRow)
                  call rowsswapper(a,n,stepCount,max)
            end if

            pivot = a(pivotRow , pivotColumn)

            do row = pivotRow+1, n, 1
                  factor = (a(row , pivotColumn)/a(pivotRow , pivotColumn))

                  do column = pivotColumn, n+1 , 1
                        a(row,column) = a(row,column) - (factor*a(pivotRow,column))
                  end do
            end do
      end do

      x(n) = a(n,n+1)/a(n,n)

      do stepCount = n-1, 1, -1
            factor = 0
            do i = stepCount+1 , n , 1
                  factor = factor + a(stepCount , i)*x(i)
            end do
            x(stepCount) = (a(stepCount , n+1) - factor)/a(stepCount, stepCount)
      end do

end subroutine gaussEliminationPivoting
