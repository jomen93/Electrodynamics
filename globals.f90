! Module to write the global variables in all code 
module globals 

 implicit none

 !simulation state globals
 integer :: it                                             !Iteration number
 integer :: i                                              !Iterators
 integer :: j
 integer :: icount
 integer :: ifin

 ! grid parameters
 integer :: imax
 integer :: jmax
 ! step
 real    :: h
 ! Constant
 real    :: pi
 ! Data arryas 
 real, allocatable :: phi(:,:)                             !< potential vector!
 real, allocatable :: field_hx(:,:)
 real, allocatable :: field_hy(:,:)
 real, allocatable :: field_b(:,:)
 real, allocatable :: flag(:,:)

end module globals



