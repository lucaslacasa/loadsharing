module loadsharing_module
implicit none
contains



!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function projected_ICU_demand(region)
!This function assigns the projected demand of ICU units for each region/hospital/trust
!To be fully developed, for now it sets all demand to 20 units as a test
integer, intent(in) :: region
integer :: projected_ICU_demand
double precision :: XX
integer :: i


!this is a test, to be fed by accurate data*******
call random_number(XX)
XX=(XX)*40
projected_ICU_demand = 20!int(XX*0.)

!We will have average values of ICU demand per LA code
!We will then sum up the demand at the level of trust,
!as several LA codes go to the same trust
!*************************************************

end function
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
subroutine global_stress(ICU_demand,x)
!This subroutines computes the global stress of the system, defined as
!the total number of ICU units which cannot be supplied given the
!current distribution of cases
!If =0 the system is not overwhelmed
integer, intent(in) :: ICU_demand(:)
integer, intent(out) :: x
integer :: i

x=0.
do i=1,size(ICU_demand)
    if(ICU_demand(i) > 0) then !I only count the units above capacity
        x = x + ICU_demand(i)
    endif
enddo
end subroutine
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
subroutine sharing_availability(region,ICU_demand,available, neighborhood, destination,loadshare,d_max)
!This subroutine gets a given region/hospital/trust which is projected to be collapsed as an input,
!checks whether its neighborhood can receive cases, and if it can, selects at random a destination
!and shares a certain load (not all necessarily)
integer, intent(in) :: region
integer, intent(in) :: ICU_demand(:)
logical, intent(out) :: available
integer, intent(out) :: destination
integer, intent(out) :: loadshare
double precision, intent(in) :: d_max !maximum allowed distance between trusts to accept a transfer
integer :: i, x_int,cont
double precision :: x_rand
double precision,intent(in) :: neighborhood(:,:) !this is a vector which enumerates all the 5
!regions which are adjacent to region under analysis, and for each neighbor, its integer label and distance to origin


available = .FALSE.
do i=1,5
    !if for at least one of the 5 neighbors, its demand is negative (positive capacity) and
    !it is a neighbor within the catchment area (distance<d_max), available is set to true
    if((ICU_demand(int(neighborhood(i,1))) < 0) .AND. (neighborhood(i,2)<d_max)) then
        available = .TRUE. !at least a destination is not collapsed
    endif
enddo

if(available .eqv. .TRUE.) then
    do
        !I am choosing a possible destination at random from its neighborhood
        call random_number(x_rand)
        x_int = int(x_rand*5) + 1
        destination = int(neighborhood(x_int,1))

        !if that destination has capacity, I share a certain load, and I exit the loop
        if(ICU_demand(destination) < 0) then
            !how much load is shared? either 1/2 of the availability or all the requested, whichever is smaller
            if(ICU_demand(region)< int(abs(ICU_demand(destination)*1./2))) then
                loadshare = ICU_demand(region)
            else
                loadshare = int(abs(ICU_demand(destination)*1./2))
            endif

            exit !I exit the LOOP AS SOON AS I DECIDED A RANDOM DESTINATION
        endif

        cont=cont+1
        if(cont>100000) then
            write(*,*) 'problem finding the adequate destination, stopping the simulation'
            stop
        endif

    enddo
else
    !if the region cannot share because its neighborhood is already collapsed
    destination = region
    loadshare = 0
endif

end subroutine
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

end module
