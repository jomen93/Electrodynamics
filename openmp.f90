program parallel
use omp_lib
use mpi

 implicit none

 ! integer :: num_threads = 4
 integer, parameter :: m =100, n =1000 , o = 100 
 integer :: i,j,k
 real*8, dimension(1:m, 1:n) :: a = 1.0
 real*8, dimension(1:n, 1:o) :: b = 1.0
 real*8, dimension(1:m, 1:o) :: c = 0.0
 real*8 :: t1,t2,ep
 integer, parameter :: num_threads = 4
 !$ call omp_set_num_threads(num_threads)
 ! invoking a system timer here
 call cpu_time(t1) 

 !!$omp parallel do
 do i = 1, m
  do j = 1, o
   do k = 1, n
    c(i,j) = c(i,j) + a(i,k)*b(k,j)
    write(*,*) c(i,j)
   end do
  end do
 end do
  ! !$omp end parallel do
 call cpu_time(t2)
 ep = t2-t1 
 write(*,*) ep, "tiempo"

 !$ ep = ep/num_threads

 !$ print*, "Parallel Mode on !"

end program parallel
