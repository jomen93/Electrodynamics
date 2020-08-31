program campo_B
use omp_lib

 implicit none
 ! iterators
 integer :: i,j,icount,ifin
 ! auxiliar variables
 real :: a,b,new

 ! grid parameters
 integer, parameter :: imax = 256
 integer, parameter :: jmax = 256
 ! step
 real, parameter :: h = 1.0
 ! Constants (7 significant digits)
 real, parameter :: pi = 3.141593

 ! arrays (physical variables)
 real,     allocatable :: phi(:,:)
 real,     allocatable :: field_hx(:,:)
 real,     allocatable :: field_hy(:,:)
 real,     allocatable :: field_b(:,:)
 integer,  allocatable :: flag(:,:)
 ! flag is a array to identify points in the grid
 ! choose which equation use in the grid

 ! iman location 
 integer, parameter :: i1 = imax/2 - 20
 integer, parameter :: i2 = imax/2 + 20
 integer, parameter :: j1 = jmax/2

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
 allocate(field_b(imax,jmax))
 allocate(field_hx(imax,jmax))
 allocate(field_hy(imax,jmax))

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
 
 ifin = 1000
 ! solution of the equation , sobre relaxation method 
 ! $omp parallel do
    do icount = 1, ifin
     write(*,fmt="(a3,a,t21,f6.2,a)",advance="no") achar(13), "progreso completo:    ", (real(icount)/ifin)*100.0," %"

     do i = 1, imax
      do j = 1, jmax  
        if (flag(i,j).eq.0)then
          a = 1.0 + 1.0/2.0/float(i)
          b = 1.0 - 1.0/2.0/float(i)
          new = (a*phi(i+1,j)+b*phi(i-1,j)+phi(i,j+1)+phi(i,j-1))/4.0
        else if (flag(i,j) == 1 .or. flag(i,j) == 2 .or. flag(i,j) == 3)then
          new = phi(i,j)
        else if (flag(i,j) == 4)then
          new = (4.0*phi(2,j)+phi(1,j+1)+phi(1,j-1))/6.0
        else if (flag(i,j) == 5)then
          new = (phi(i+1,j)+phi(i-1,j))/2.0
        else if (flag(i,j) == 6)then
          new = (m*h+phi(i,j+1)+phi(i,j-1))/2.0
        else if (flag(i,j) == 7)then
          new = (m*h+phi(i,j+1)+phi(i,j-1)+phi(i+1,j)+phi(i-1,j))/4.0
        else
          stop "The point isn't indexed"
        end if
      phi(i,j) = new + alpha*(new-phi(i,j))
      end do
     end do
    end do 
 ! $omp end parallel do

 ! Magnetic potential save
 open(10,file = "Mag_pot.dat")
 write(10,*) phi
 close(10)

 ! compute the h field
 do i = 2, imax-1
  do j = 2, jmax -1
    field_hx(i,j) = (phi(i+1,j)-phi(i-1,j))/h/2.0
    field_hy(i,j) = (phi(i,j+1)-phi(i,j-1))/h/2.0
  end do
 end do
 
 open(20,file = "field_hx.dat")
 write(20,*) field_hx
 close(20)
 open(20,file = "field_hy.dat")
 write(20,*) field_hy
 close(20)

 
 write(*,*) alpha

 end program campo_B
