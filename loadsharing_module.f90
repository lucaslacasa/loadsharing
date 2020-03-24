module loadsharing_module
implicit none
contains



!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function projected_ICU_demand(region)
integer, intent(in) :: region
integer :: projected_ICU_demand
double precision :: XX
integer :: i


!this is a test, to be fed by accurate data*******
call random_number(XX)
XX=(XX)*20
projected_ICU_demand = 20!int(XX*0.)



rewind(100)

!*************************************************

end function
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
subroutine global_stress(ICU_demand,x)
integer, intent(in) :: ICU_demand(:)
integer, intent(out) :: x
integer :: i

x=0.
do i=1,size(ICU_demand)
    if(ICU_demand(i) > 0) then
        x = x + ICU_demand(i)
    endif
enddo
end subroutine
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
subroutine sharing_availability(region,ICU_demand,available, neighborhood, destination,loadshare)
integer, intent(in) :: region
integer, intent(in) :: ICU_demand(:)
logical, intent(out) :: available
integer, intent(out) :: destination
integer, intent(out) :: loadshare
integer :: i, x_int,cont
double precision :: x_rand
double precision,intent(in) :: neighborhood(5,2) !this is a vector which enumerates all the 5
!regions which are adjacent to region under analysis, and for each neighbor, its integer label and distance to origin

!We need to load the vector neighborhood() with the 5 closest regions/hospitals
!This is a test*********************************
!do i=1,size(ICU_demand)!this is synthetic case
 !   neighborhood(i) = i
!enddo
!***********************************************

available = .FALSE.
do i=1,5
    if(ICU_demand(int(neighborhood(i,1))) < 0) then
        available = .TRUE. !at least a destination is not collapsed
    endif
enddo
if(available .eqv. .TRUE.) then
    do
        call random_number(x_rand) !I am choosing the destination at random
        x_int = int(x_rand*5) + 1
        destination = int(neighborhood(x_int,1))
        if(ICU_demand(destination) < 0) then
            !how much? either 1/2 of the availability or all the requested, whichever is smaller
            if(ICU_demand(region)< int(abs(ICU_demand(destination)*1./2))) then
                loadshare = ICU_demand(region)
            else
                loadshare = int(abs(ICU_demand(destination)*1./2))
            endif

            exit !I exit the LOOP AS SOON AS I DECIDED A RANDOM DESTINATION
        endif
        cont=cont+1
        if(cont>100000) then
            write(*,*) 'problem finding the adequate destination'
            stop
        endif
    enddo
else
    destination = region
    loadshare = 0
endif

end subroutine
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

end module
