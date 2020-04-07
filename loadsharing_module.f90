module loadsharing_module
implicit none
contains



!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function projected_ICU_demand(region,ICU_beds)
!This function assigns the projected demand of ICU units for each region/hospital/trust
!To be fully developed, for now it sets all demand to 20 units as a test
integer, intent(in) :: region
integer, intent(in) :: ICU_beds(:)
integer :: projected_ICU_demand
double precision :: XX,z,pp
integer :: i


!this is a test, to be fed by accurate data*******
call random_number(XX)
!z=0.2
!pp=0.8
!XX=(XX)-0.5
!XX=XX*ICU_beds(region)*0.4

if(ICU_beds(region)>20) then
    projected_ICU_demand = ICU_beds(region) +  ICU_beds(region)*0.2
else
 projected_ICU_demand = ICU_beds(region) -  ICU_beds(region)*0.2
endif


!projected_ICU_demand = 80
!if(xx<pp) then !excess
!projected_ICU_demand = ICU_beds(region) + int(z*ICU_beds(region)) !80!int(XX*0.)
!else !relaxed
!projected_ICU_demand = ICU_beds(region) - int(z*ICU_beds(region)) !80!int(XX*0.)
!endif

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
integer :: i, x_int,cont,degree,cont2
double precision :: x_rand
double precision,intent(in) :: neighborhood(:,:) !this is a vector which enumerates all the
!regions which are adjacent to region under analysis, and for each neighbor, its integer label and distance to origin

degree = size(neighborhood(:,1)) !it will be 4 (or 5) in the regular geometric graph, and up to 9 in the contact one
available = .FALSE.
loadshare = 0
if(sum(neighborhood(:,1))<0.5) then
write(*,*) 'trying to reach node zero, meaning using a node without links in the sharing routine, abort'
stop
endif

do i=1,degree
    !if for at least one of the 5 neighbors, its demand is negative (positive capacity) and
    !it is a neighbor within the catchment area (distance<d_max), available is set to true
    if(neighborhood(i,1)>0.5) then !if the link is to node 0, that means that this link is not useful (contact network)
        if((ICU_demand(int(neighborhood(i,1))) < 0) .AND. (neighborhood(i,2)<d_max)) then
            available = .TRUE. !at least a destination is not collapsed
        endif
    endif
enddo

if(available .eqv. .TRUE.) then
    do
        !I am choosing a possible destination at random from its neighborhood
        cont2=0
        do
            cont2=cont2+1
            call random_number(x_rand)
            x_int = int(x_rand*degree) + 1
            destination = int(neighborhood(x_int,1))
            if(destination>0) exit !this is to guarantee that the link is not towards node 0 (as it could happen in contact nets)
            if(cont2>100000) then
                !cant find a way out, aborting
                write(*,*) 'cant find a link to a non-zero node to share, abort'
                stop
            endif
        enddo

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
