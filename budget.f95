program bugdet
      implicit none

      character(Len = 10) :: category1, category2, category3
      real :: expense1, expense2, expense3

      write(*,*) "Enter thre first category"
      read(*,*) category1
      write(*,*) "Enter the amount spent"
      read(*,*) expense1

      write(*,*) "Enter thre second category"
      read(*,*) category2
      write(*,*) "Enter the amount spent"
      read(*,*) expense1

      write(*,*) "Enter thre third category"
      read(*,*) category3
      write(*,*) "Enter the amount spent"
      read(*,*) expense1

      write(*,*) "My budget: August 2017"
      write(*,10) "category" , "      " , "spent"
      write(*,20) category1 , "      " , expense1
      write(*,20) category2 , "      " , expense2
      write(*,20) category3 , "      " , expense3

      10 format(a10, a3 , a7)
      20 format(a10, a3 , f7.2)

end program bugdet
