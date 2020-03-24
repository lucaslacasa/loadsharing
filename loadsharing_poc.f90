
program loadsharing_poc
use loadsharing_module
implicit none

!!  WE ARE ASSUMING A NEIGHBORHOOD K=5 FOR EACH REGION/HOSPITAL, THUS WE HAVE A 5-REGULAR GEOMETRIC GRAPH
!! TESSELATING THE SET OF REGIONS/HOSPITALS

integer, allocatable :: ICU_demand(:) !projected ICU_demand in each region
integer, allocatable :: updated_ICU_demand(:,:) !projected ICU demand in each region after loadsharing, for each loadsharing configuration
integer, allocatable :: changes(:,:)
integer :: total_num_regions
integer, allocatable :: loadshare(:,:)
integer, allocatable :: ICU_beds(:)
integer :: config
integer :: opt
integer :: destination
logical :: available
integer :: config_max !total number of configurations tested
integer :: x !global_stress without loadsharing
integer :: x_opt
integer :: x_future
integer :: i,j,k
double precision :: r, distance
character(3)::trustID, targetTrust
character(3), allocatable :: TrustID_array(:)
integer :: trustNo, targetNo
double precision, allocatable :: neighborhood(:,:,:)


config_max=100000
total_num_regions = 132



call random_number(r)
call random_number(r)
allocate(ICU_demand(total_num_regions),updated_ICU_demand(config_max,total_num_regions))
allocate(changes(config_max,total_num_regions))
allocate(loadshare(config_max,total_num_regions))
allocate(ICU_beds(total_num_regions), TrustID_array(total_num_regions))
allocate(neighborhood(total_num_regions,5,2))

!LOADING ICU beds data per Trust+++++++++++++++++
open(100,file='icuBedsByTrustLocation_ed.txt')
read(100,*)
do i=1,total_num_regions
    read(100,*) trustID, trustNo, ICU_beds(trustNo)
    TrustID_array(trustNo) = trustID
enddo
rewind(100)
write(*,*) 'ICU bed data loaded...'
!+++++++++++++++++++++++++++++++++++++++++++++++++

!LOADING Trust Networks (5-regular geometric graph)+++++++++++++++++
open(101,file='icuToicuNetwork_ed.txt')
read(101,*)
do k=1,660
    read(101,*) trustId, j, targetTrust, distance !name of the origin, j-th neighbor, name of the neighbor, distance
    do i=1,total_num_regions
        if(TrustID_array(i)==trustId) then
            trustNo = i !match name with integer label for the origin node
            exit
        endif
    enddo
    do i=1,total_num_regions
        if(TrustID_array(i)==targetTrust) then
            targetNo = i !match name with integer label for the destination node
            exit
        endif
    enddo
    neighborhood(TrustNo, j,1)= targetNo
    neighborhood(TrustNo, j,2)= distance
enddo
write(*,*) 'Trust Network data loaded...'

!estimate prrojected ICU demand for next week
do i=1,total_num_regions
    ICU_demand(i) = projected_ICU_demand(i) - ICU_beds(i)
 !LHS is an array
 !ICU_beds is an array (already loaded
 !projected_ICU_demand() is a function that counts three types of infected numbers
enddo
write(*,*) 'estimated ICU demands for next week loaded...'
write(*,*) ICU_demand

call global_stress(ICU_demand,x) !compute the global stress before proposed loadsharing, the result is in x
write(*,*) 'the net global stress for next week without loadsharing is:',x

if(x<0) then
    write(*,*) 'this week we dont need to loadshare !!!'

else !we start the process

    write(*,*) 'loadsharing algorithm initiated...'
    x_opt = x !this is the baseline global stress which we aim to decrease
    do config=1,config_max !loop in configurations, where each configuration is a random local loadsharing
        updated_ICU_demand(config,:) = ICU_demand
        do i=1,total_num_regions
            if(ICU_demand(i)>0) then
    call sharing_availability(i,ICU_demand,available, neighborhood(i,:,:), destination,loadshare(config,i))
                !the output of the routine sharing_availability() is
                    !* whether there is at least a neighbour of i whose ICU_demand(j)<0 (TRUE/FALSE)
                    !* the values of ICU_demand(j) for all j which are available
                if(available .eqv. .TRUE.) then
                !this is share(i)
                    !* decide how to make the loadsharing among all possibilities (random local loadsharing)
                    !* write down the specific choice we took in the array changes(:,)
                    !select at random one of the 5 possible and note it down in the vector of changes
                    changes(config,i)=destination !I log in the config # realisation where i-->j goes

                    !* update the projected ICU demands after this loadsharing choice
                    updated_ICU_demand(config,i) = updated_ICU_demand(config,i) - loadshare(config,i)
                    updated_ICU_demand(config,destination) = updated_ICU_demand(config,destination) + loadshare(config,i)
                    !if(updated_ICU_demand(config,destination)>0) then
                     !   write(*,*) 're-routine is collapsing some other node'
                        !stop
                    !endif
                endif
            endif
        enddo

        call global_stress(updated_ICU_demand(config,:),x_future) !compute the global stress after proposedloadsharing, result in x_future

        if(x_future < x_opt) then !if the global stress computed for the new config is smaller than the smallest so far, I keep it
                opt = config
                x_opt = x_future
        endif
    enddo

    !Now I apply the optimal change


    if(x_opt<x) then
        !accept the change
        ICU_demand(:) = updated_ICU_demand(opt,:)
        !report change
        write(*,*) 'optimal configuration number:',opt
        write(*,*) 'advised changes:'
        do i=1,total_num_regions
          write(*,*) i, 'shares', loadshare(opt,i),'loads to', changes(opt,i)
        enddo
        write(*,*) 'Previous global stress',x
        write(*,*) 'New global stress',x_opt
        write(*,*) 'total net improve:',x-x_opt, 'ICU units'
        write(*,*) 'new ICU demand after loadsharing:'
        write(*,*) ICU_demand
    else
        !don't accept the change
    endif
endif

end program
