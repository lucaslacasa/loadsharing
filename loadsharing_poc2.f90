
program loadsharing_poc2
use loadsharing_module
implicit none

!#############################################################################################################
! MAIN PROGRAM
! DEPENDENCIES: it uses the module LOADSHARING_MODULE (filled with some routines)
!
! Comments
! 1) WE ARE ASSUMING A NEIGHBORHOOD K=4 FOR EACH REGION/HOSPITAL/TRUST, THUS WE HAVE A 4-REGULAR GEOMETRIC GRAPH
! TESSELATING THE SET OF REGIONS/HOSPITALS
!
! v2, 31/03/2020
! v2 improvement wrt to v1 includes:
! - loads can now be shared to more than one receptor, by sequentially sharing to a possible total of num_shares
! (so the total number of receptors is smaller or equal to num_shares)
! - this improvement only makes sense in the sequential mode, so we only allow it in that mode
!
! Lucas Lacasa, Queen Mary University of London, l.lacasa.AT.qmul.ac.uk
!
!###############################################################################################################

!## DECLARATION OF VARIABLES
integer, allocatable :: ICU_demand(:) !array with projected ICU_demand in each region/hospital/trust
integer, allocatable :: updated_ICU_demand(:,:) !projected ICU demand in each region after loadsharing, for each loadsharing configuration
integer, allocatable :: changes(:,:,:) !records, for each realisation, the destination for each node. the third column is the ordinal of the
!choice: if =1 then we are looking at the first time the node was given the option to share loads
integer :: total_num_regions !total number of regions/hospitals/trusts
integer, allocatable :: loadshare(:,:,:) !records, for each realisation, the total load shared (if none, =0). last column is ordinal of share
integer, allocatable :: ICU_beds(:) !array recording the baseline capacity of each region/hospital/trust in terms of ICU beds
integer, allocatable :: acute_beds(:) !array recording the baseline capacity of each region/hospital/trust in terms of acute beds
integer, allocatable :: day_beds(:) !array recording the baseline capacity of each region/hospital/trust in terms of day beds
integer :: config
integer :: cont_unacceptable
integer :: total_unacceptable
integer :: opt !optimal configuration number
integer :: destination !integer label of the region/hospital/trust which will be the destination of a given loadshare
logical :: available !set to .true. if at least one of the neighbors of the node that needs to transfer has available capacity
logical :: parallel !set to .true. if loadsharing decisions are performed in parallel and .false. if sequential
!sequential guarantees that a solidary receptor won't get collapsed.
integer :: config_max !total number of configurations tested
integer :: x !global_stress without loadsharing
integer :: x_opt !global stress for the optimal configuration
integer :: x_future !global stress of each configuration tested
integer :: num_shares !number of times the same node is offered the possibility of sharing (Use only in 'sequential' mode!!!)
integer :: i,j,k,iii
double precision :: r
double precision :: LAT,LON !latitude and longitude
double precision :: distance !used to load the distance between any two region/hospital/trust
double precision :: d_max !maximum distance between trusts which is acceptable for a transfer
character(3)::trustID, targetTrust,regionID !character arrays that label the 3-letter code for each trust
character(3), allocatable :: TrustID_array(:) !character array where we match the integer label of a trust and its 3-letter code
character :: choice !asks the user to select between parallel and sequential and loads the keyboard response there
integer :: trustNo, targetNo !integer variables to record the integer label of a given region/hospital/trust
double precision, allocatable :: neighborhood(:,:,:) !records, for a given configuration, the integer label of all 5 neighbors
                                                     !of a region/hospital/trust, along with the distance between the origin and destination

!+++++++++++++++++++++++++
!## CONSTANTS and PRESETS
config_max=100000
total_num_regions = 141
d_max = 1
num_shares = 1
do
    write(*,*) 'Choose optimisation process: Sequential (S) or Parallel (P):'
    read(*,*) choice
    if(choice == 'P') then
        parallel = .TRUE.
        exit
    elseif(choice == 'S') then
        parallel = .FALSE.
        write(*,*) 'Insert number of choices offered to a node to share load'
        read(*,*) num_shares
        exit
    else
        write(*,*) 'wrong input...'
    endif
enddo
!+++++++++++++++++++++++++


!used to change the random seed
call random_number(r)
call random_number(r)

!allocate memory for different arrays+++++++++++++++++++++++++++++++++++++++++++++++++++
allocate(ICU_demand(total_num_regions),updated_ICU_demand(config_max,total_num_regions))
allocate(changes(config_max,total_num_regions,num_shares))
allocate(loadshare(config_max,total_num_regions,num_shares))
allocate(ICU_beds(total_num_regions), TrustID_array(total_num_regions))
allocate(acute_beds(total_num_regions),day_beds(total_num_regions))
allocate(neighborhood(total_num_regions,5,2))
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!LOADING ICU beds data per Trust+++++++++++++++++
open(100,file='icuBedsByTrustLocation_new_ed.txt')
read(100,*)
do i=1,total_num_regions
    read(100,*) regionID,trustID, trustNo, acute_beds(trustNo),day_beds(trustNo),ICU_beds(trustNo),LAT,LON
    TrustID_array(trustNo) = trustID
enddo
rewind(100)
write(*,*) 'ICU bed data loaded...'
!+++++++++++++++++++++++++++++++++++++++++++++++++

!LOADING Trust Networks (5-regular geometric graph)++++++++++++++++++++++++++++++++++++++
open(101,file='icuToicuNetwork_new_ed.txt')
read(101,*)
do k=1,705
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
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!estimate projected ICU demand for next week+++++++++++++++++++++++++++++++++++++++++++++++
open(1000,file='ICU_demand_pre')
do i=1,total_num_regions
    ICU_demand(i) = projected_ICU_demand(i,ICU_beds) - ICU_beds(i)
 !LHS is an array
 !ICU_beds is an array (already loaded)
 !projected_ICU_demand() is a function that counts three types of infected numbers (see module)
enddo
write(*,*) 'estimated ICU demands for next week loaded...'
write(*,*) ICU_demand
do i=1,total_num_regions
   write(1000,*) ICU_demand(i)
enddo
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!computation of the global stress before proposed loadsharing, the result is in x+++++++++++
call global_stress(ICU_demand,x)
write(*,*) 'the net global stress for next week without loadsharing is:',x
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!LOADSHARING CORE ALGORITHM
if(x<0) then
     write(*,*) 'this week we dont need to loadshare !!!'

else !we start the process
    write(*,*) 'loadsharing algorithm initiated...'
    x_opt = x !this is the baseline global stress which we aim to decrease
    do config=1,config_max !loop of config_max realisations, where each configuration performs random local loadsharing
        cont_unacceptable = 0 !this counts the number of regions/hospitals/trusts that were initially available but get collapsed
        total_unacceptable = 0
        updated_ICU_demand(config,:) = ICU_demand !initialise the ICU demands for a particular realisation
        do i=1,total_num_regions !loop in regions. Note that

            do iii=1,num_shares !loop in the number of times that a node is offered to share, set to 1 for parallel and entered via
                                !keyboard for sequential
            if(ICU_demand(i)>0) then
                if(parallel.eqv. .TRUE.) then
                    !if on the 'parallel option', then the decisions on where to go are based on ICU_demand (not updated sequentially),
                    !so there is a risk that one receptor which was not overwhelmed received shares from many nodes and gets overwhelmed
call sharing_availability(i,ICU_demand,available, neighborhood(i,:,:), destination,loadshare(config,i,iii),d_max)
                else
call sharing_availability(i,updated_ICU_demand(config,:),available, neighborhood(i,:,:), destination,loadshare(config,i,iii),d_max)
                    !here the decisions are based in terms of updated_ICU_demand instead of ICU_demand and thus load sharing decisions are done
                    !sequentially, starting from node 1 etc.
!warning: if num_shares>1 (multiple shares per node), then loadshare(config,i) is overwritten each of the num_shares times of variable iii.
                endif
                if(available .eqv. .TRUE.) then
                    changes(config,i,iii)=destination !I log in the config # realisation where i-->j goes
                    !* update the projected ICU demands after this loadsharing choice
                    updated_ICU_demand(config,i) = updated_ICU_demand(config,i) - loadshare(config,i,iii)
                    updated_ICU_demand(config,destination) = updated_ICU_demand(config,destination) + loadshare(config,i,iii)
                    !now I record all the 'unacceptable transfers' (overwhelming non-overwhelmed nodes because of parallel update)
                    if(updated_ICU_demand(config,destination)>0.and.ICU_demand(destination)<0) then
                        cont_unacceptable = cont_unacceptable + 1
                        total_unacceptable = total_unacceptable + updated_ICU_demand(config,destination)
                    endif
                endif
            endif
            enddo !end loop in number of choices to share a load per node

        enddo

        !I compute the global stress resulting after the loadsharing
        call global_stress(updated_ICU_demand(config,:),x_future) !compute the global stress after proposedloadsharing, result in x_future

        !I only keep track of this loadsharing configuration if it improves the overall global stress
        if(x_future < x_opt) then !if the global stress computed for the new config is smaller than the smallest so far, I keep it
                opt = config
                x_opt = x_future
        endif
    enddo

    !Once all realisations are done, I have finished my exploration
    !Now I look at the optimal change, and I implement / report it
    if(x_opt<x) then
        !accept the change
        ICU_demand(:) = updated_ICU_demand(opt,:)
        !report change
         write(*,*) 'optimal configuration number:',opt
        write(*,*) 'advised changes:'
        do i=1,total_num_regions
            do iii=1,num_shares
          write(*,*) i, 'shares', loadshare(opt,i,iii),'loads to', changes(opt,i,iii)
            enddo
        enddo
        write(*,*) 'new ICU demand after loadsharing:'
        open(1002,file='ICU_demand_post')
        write(*,*) ICU_demand
        do i=1,total_num_regions
            write(1002,*) ICU_demand(i)
        enddo
        write(*,*)

        write(*,*) '***********************************************************************************************'
        write(*,*) 'SUMMARY OF THE LOADSHARING PROCESS:'
        if(parallel .eqv. .TRUE.) then
            write(*,*) 'processing mode: parallel update // unacceptable overloads of solidary receptors are POSSIBLE'
        else
            write(*,*) 'processing mode: sequential update // unacceptable overloads of solidary receptors are FORBIDDEN'
        endif

        write(*,*) 'Previous global stress',x
        write(*,*) 'New global stress',x_opt
        write(*,*) 'total net improve:',x-x_opt, 'ICU units'
        write(*,*) 'Number of regions/hospitals/trusts that get collapsed because of solidarity:', cont_unacceptable
        write(*,*) 'Total number of unacceptable collapsed ICU units:', total_unacceptable
        write(*,*) '***********************************************************************************************'
    else
        !don't accept the change
    endif
endif

end program
