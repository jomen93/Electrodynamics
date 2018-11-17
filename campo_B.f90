program campo_B
 implicit real*8(a-h, o-z)
 parameter (imax = 125, jmax = 125)
 dimension phi(imax, jmax)
 integer flag(imax,jmax)
 real*8 m
 pi = 4.d0*datan(1.d0)
 h = 1.d0
 i1 = 6 *5
 i2 = 15 *5
 j1 = 4 *5
 m = 1000.
 v = 10.d0
 alpha = -1.d0+4.d0/(2.d0+dsqrt(4.d0-(dcos(pi/imax)+dcos(pi/jmax))**2))

 do i = 2,imax-1
  do j = 2,jmax-1
   flag(i,j) = 0

  end do
 end do

 do i = 1 ,imax
  flag(i,1) = 1
  flag(i,jmax) = 2
 end do 

 do j = 2, jmax-1
  flag(imax, j) = 3
  flag(1, j) = 4
 end do 

 do j = 2, j1-1
  flag(i1,j) = 5
  flag(i2,j) = 5
 end do 

 do i = i1+1, i2-1
  flag(i,j1) = 6
 end do 

 flag(i1,j1) = 7
 flag(i2,j1) = 7

 do i = 1, imax
  do j = 1, jmax
   phi(i,j) = 1
  end do 
 end do 


 do i = 1, imax
  phi(i,1) = 0.d0
  phi(i,jmax) = v*m/(4.d0*pi*(i*h)**3)
 end do
 write(6,*) 
 do j=1,jmax
  phi(imax,j) = v*m/(4.d0*pi*(imax*h)**3)
 end do


 ! Solucion de la ecuación por el metodo de sobrerelajación

 do icount = 1, 200
  do i = 1, imax -1
   do j = 1, jmax -1
    if(flag(i,j) .eq. 0) then
     a = 1.d0+1.d0/2.d0/float(i)
     b = 1.d0-1.d0/2.d0/float(i)
     nuevo = (a*phi(i+1,j)+b*phi(i-1,j)+phi(i,j-1))/4.d0
     else if(flag(i,j) .eq. 1 .or. flag(i,j).eq.2 .or. &
       flag(i,j) .eq. 3)then
      nuevo = phi(i,j)
     else if(flag(i,j) .eq. 4)then
      nuevo = (4.d0*phi(2,j)+phi(1,j+1)+phi(1,j-1))/6.d0
     else if(flag(i,j) .eq. 5)then
      nuevo = (phi(i+1,j)+phi(i-1,j))/2.d0
     else if(flag(i,j) .eq. 6)then
      nuevo = (m*h*phi(i,j+1)+phi(i,j-1))/2.d0
     else if(flag(i,j) .eq. 7)then
      nuevo = (m*h*phi(i,j+1)+phi(i,j-1)+phi(i+1,j)+phi(i-1,j))/4.d0
     else
      stop "... El punto de la región no está indexado"
     end if
    phi(i,j) = nuevo + alpha*(nuevo-phi(i,j))
   end do
  end do
 end do
 open(10, file = "pot_Mag.dat")
 do i=1, imax
  do j=1,jmax
   write(10,*) i,j,phi(i,j)
  end do
 end do
 close(10)

 open(11,file = "campo_h.dat")
 do i=2,imax-1,5
  do j=2,jmax-1,5
   write(11,*) i,j,(phi(i+1,j)-phi(i-1,j))/h/2.d0,(phi(i,j+1)-phi(i,j-1))/h/2.d0
  enddo
 enddo
 close(11)

 open(12, file = "campo_B.dat")
 do i=2,imax-1,5
  do j=2,jmax-1,5
   aa = 0.d0
   if (i .ge. i1 .and. i .le. i2 .and. j .le. j1) aa = m
    write(11,*) i,j,-(phi(i+1,j)-phi(i-1,j))/h/2.d0,aa-(phi(i,j+1)-phi(i,j-1))/h/2.d0
  enddo
 enddo
 close(12)
 end program campo_B