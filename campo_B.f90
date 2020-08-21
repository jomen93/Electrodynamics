program campo_B
use omp_lib

 implicit none
 ! iterators 
 integer :: i,j, icount 

 ! grid parameters
 integer, parameter :: imax = 128
 integer, parameter :: jmax = 128
 ! step 
 real, parameter :: h = 1.0
 ! Constants (7 significant digits)
 real, parameter :: pi = 3.141593

 ! arrays (physical variables)
 real, allocatable :: phi(:,:)
 integer,  allocatable :: flag(:,:)
 ! flag is a array to identify points in the grid 
 ! choose which equation use in the grid 
 
 ! iman location 
 integer, parameter :: l1 = 0
 
 integer, parameter :: i1 = 20
 integer, parameter :: i2 = 75
 integer, parameter :: j1 = 20

 ! magnetization 
 real, parameter :: m = 1000

 ! volumen 
 real, parameter :: v = 10

 ! sobre relaxation parameter 
 real, parameter :: alpha = -1.0 +4/(2+sqrt(4.0-(cos(pi/imax)+cos(pi/jmax))**2))

 write(*,*) "Allocating data arrays ..."
 ! allocate flag variables
 allocate(flag(imax,jmax))
 allocate(phi(imax,jmax))

 ! bottom boundary 
 do i = 1,imax
  flag(i,1) = 1
 end do 

 ! upper boundary 
 do i = 1, imax 
  flag(i,jmax) =2 
 end do 

 ! right boundary 
 do j=2, jmax-1
  flag(imax,j) = 3
 end do 
 ! left boundary
 do j=2, jmax-1
  flag(1,j) = 4
 end do

 ! side covers 
 do j =2, j1-1
  flag(i1,j) = 5
  flag(i2,j) = 5
 end do  

 !top cap
 do i = i1+1, i2-1
  flag(i,j1) = 6 
 end do 

 ! edge points 
 flag(i1,j1) = 7 
 flag(i2,j1) = 7 

 ! phi initialization 
 phi(:,:) = 1.0
 do i = 1, imax
  phi(i,1) = 0.0
  phi(i,jmax) = v*m/(4.0*pi*(i*h)**3)
 end do

 do j = 1, jmax
  phi(imax,j) = v*m/(4.0*pi*(imax*h)**3)
 end do

 ! solution of the equation , sobre relaxation method 
 !$omp parallel do

    do icount = 1, 100
     do i = 1, imax
      do j = 1, jmax  
        if (flag(i,j).eq.0)then
          ! A = 1.0 + 1.0/2.0/float(i)

        end if
      end do
     end do
    end do 

    
 !$OMP end parallel do

 
 write(*,*) alpha
 write(*,*) phi(1,1), flag(1,1)
 print*, "Hello from process: ", OMP_GET_THREAD_NUM()

 end program campo_B