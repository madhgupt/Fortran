subroutine gaussEliminationNaive(aIn, n, x)
      implicit none

      integer, intent(in) :: n
      real, dimension(n, n+1), intent(in) :: aIn
      real, dimension(n), intent(out) :: x

      real, dimension(n, n+1) :: a

      integer :: stepCount, rowCount, columnCount

      integer :: pivotRow, pivotColumn
      real :: pivot
      real :: factor

      write(*,*)
      write(*,*) "System of Linear Algebraic Equations"
      write(*,*) "Method: Naive Gauss Elimination"
      write(*,*)

      a = aIn
      write(*,*) "The Augmented Matrix"
      call printMatrix2D(a, n, (n + 1))

      write(*,*) "Part 1: Forward Elimination"
      write(*,*) "(Reduce the coefficient matrix to upper triangular form)"
      write(*,*)
      do stepCount = 1, (n - 1)
            write(*,20) "Step #", stepCount
            write(*,30) "Eliminate sub-diagonal elements of column #", stepCount

            pivotRow = stepCount
            pivotColumn = stepCount
            pivot = a(pivotRow, pivotColumn)

            do rowCount = (pivotRow + 1), n
                  factor = a(rowCount, pivotColumn) / pivot
                  do columnCount = pivotColumn, (n + 1)
                        a(rowCount, columnCount) = a(rowCount, columnCount) - factor * a(pivotRow, columnCount)
                  end do
            end do
            call printMatrix2D(a, n, (n + 1))
      end do

      write(*,*) "Part 2: Back Substitution"
      write(*,50) "Evaluate x(", n, ")"
      x(n) = a(n, n+1) / a(n, n)
      write(*,40) "x(n) = ", x(n)
      write(*,*)

      do rowCount = (n - 1), 1, -1
            write(*,50) "Evaluate x(",rowCount, ")"
            factor = 0
            do stepCount = (rowCount + 1), n
                  factor = factor + a(rowCount, stepCount) * x(stepCount)
            end do
            x(rowCount) = (1 / a(rowCount, rowCount)) * (a(rowCount, n+1) - factor)
            write(*,60) "x(",rowCount, ") = ", x(rowCount)
            write(*,*)
      end do
      20 format(a6, i1)
      30 format(a43, i1)
      40 format(a6, f5.2)
      50 format(a11, i1, a1)
      60 format(a3, i1, a4, f5.2)
end subroutine gaussEliminationNaive
