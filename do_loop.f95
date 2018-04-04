program bugdet
      implicit none

      real , external :: sumOfarray

      character(Len = 20) , dimension(:), allocatable :: category
      real, dimension(:), allocatable :: expense
      real :: sum1

      integer :: u , i

      do

            write(*,*)"How many categories"
            read(*,*) u

            if (u > 0) then

                  exit

            end if

            write(*,*)"--ERROR--"

      end do

      allocate(category(u))
      allocate(expense(u))

      do i = 1 , u , 1

            write(*,30)"Enter category #" , i
            read(*,*) category(i)
            write(*,*)"Enter amount spent"
            read(*,*) expense(i)

      end do

      write(*,*) "My budget: August 2017"
      write(*,10) "category" , " | " , "spent"

      do i = 1 , u , 1

            write(*,20) category(i) , " | " , expense(i)

      end do

      sum1 = sumOfarray(expense,u)
      write(*,40) "your total expense is" , sum1
      write(*,40) "your total expense is" , sum(expense)

      10 format(a8, a3 , a7)
      20 format(a8, a3 , f7.2)
      30 format (a16 , i2)
      40 format (a21 , f10.2)

end program bugdet
