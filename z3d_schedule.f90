module z3d_schedule
!------------------------------------------------------------------------------
! Module to find all the Z3D files and separate them into Schedule(s)
! Updated 28/Sep/13 by MB.
!------------------------------------------------------------------------------

use str_util
use file_util
use log

! Public method
public :: get_schedule

! Private methods
private :: get_all_Z3Ds
private :: datenum
private :: find_schedule

! Type Schedule
type, public :: z3d_tschedule
    integer                                 :: inb_file             ! nb of files
    character(1280),dimension(30)           :: cfiles               ! ex:data/ZEUS234.Z3D
    character(10)                           :: cDate                ! Date
    character(8)                            :: cTime                ! Time
    integer                                 :: iADCrate             ! A/D freq
    integer                                 :: nb_buffer            ! Nb of buffer
    integer                                 :: buffer_size          ! Buffer size
    integer                                 :: res_bytes            ! extra left over points
end type z3d_tschedule

!-------------------------------------------------------------------------------
contains

!*******************************************************************************
!*******************************************************************************

subroutine get_all_Z3Ds_UNIX(file_names,Total_Z3D)
!------------------------------------------------------------------------------
! Get all the Z3D files in folder and subfolders on a MAC or LINUX
! Updated 28/Sep/13 by MB.
!------------------------------------------------------------------------------

  implicit none

  integer,intent(out)                                    :: Total_Z3D
  character(1280), dimension(:), allocatable,intent(out) :: file_names
  character(1280)                                        :: file_name_temp
  integer                                                :: i,reason,iStation,count
  real                                                   :: r

! Find Z3D file (avoid file smaller than 500 ko)
call system('find ./ -type f \( -iname "*.z3d" \) -size +1000 > l')

open(31,FILE='l',action="read")
  i = 0
  do
   read(31,fmt='(a)',iostat=reason) r
   if (reason/=0) EXIT
   i=i+1
  end do
  Total_Z3D=i
  allocate(file_names(Total_Z3D))
  rewind(31)
  do i = 1,Total_Z3D
   read(31,'(a)') file_name_temp
   count=LEN_TRIM(file_name_temp)
   file_names(i)=file_name_temp(1:count)
end do 

close(31)

call system('rm l')


end subroutine get_all_Z3Ds_UNIX

!*******************************************************************************
!*******************************************************************************

subroutine get_all_Z3Ds_PC(file_names,Total_Z3D)
!------------------------------------------------------------------------------
! Get all the Z3D files in folder and subfolders on a PC
! Updated 28/Sep/13 by MB.
!------------------------------------------------------------------------------

implicit none

  integer,intent(out)                                    :: Total_Z3D
  character(1280), dimension(:), allocatable,intent(out) :: file_names
  character(1280)                                        :: file_name_temp
  integer                                                :: i,reason,iStation,count
  real                                                   :: r

call system('forfiles /s /m *.z3d /c "cmd /v:on /c set sz=000000000000000@fsize&if !sz:~-15! geq 000000000512000 echo @relpath" >l')
! avoid file smaller than 500 ko

open(31,FILE='l',action="read")
  i = 0
  do
   read(31,fmt='(a)',iostat=reason) r
   if (reason/=0) EXIT
   i=i+1
  end do
  Total_Z3D=i
  allocate(file_names(Total_Z3D))
  rewind(31)
  do i = 1,Total_Z3D
   read(31,'(a)') file_name_temp
   count=LEN_TRIM(file_name_temp)
   file_names(i)=file_name_temp(4:count-1)
end do 
    file_names(1:Total_Z3D-1)=file_names(2:Total_Z3D)
    Total_Z3D=Total_Z3D-1
close(31)

call system('del l')


end subroutine get_all_Z3Ds_PC

!*******************************************************************************
!*******************************************************************************

subroutine find_schedule(file_names,Total_Z3D,headers,z3d_schOBJ,nb_schedule,date_time)
!------------------------------------------------------------------------------
! Find schedule(s)
! Updated 28/Sep/13 by MB.
!------------------------------------------------------------------------------

    implicit none

    character(1280),dimension(:),intent(in)              :: file_names
    character(10),dimension(:,:),intent(in)              :: date_time
    integer*8,dimension(:,:),intent(in)                  :: headers
    integer,intent(in)                                   :: Total_Z3D   ! Number of Files
    type(z3d_tschedule),dimension(:),intent(inout)       :: z3d_schOBJ
    integer,intent(out)                                  :: nb_schedule
    integer                                              :: i, j, k     ! Number of Channels per cac file indexes
    
    nb_schedule=1
    k=0
    j=1
    do i=1, Total_Z3D
        k=k+1
        if (headers(i, 2) /= headers(i+1, 2)) then
            z3d_schOBJ(nb_schedule)%cfiles(1:k)=file_names(headers(j:i,1))
            z3d_schOBJ(nb_schedule)%inb_file=k
            z3d_schOBJ(nb_schedule)%cDate=date_time(headers(j,1),1)
            z3d_schOBJ(nb_schedule)%cTime=date_time(headers(j,1),2)
            z3d_schOBJ(nb_schedule)%iADCrate=headers(j,5)
            k=0
            j=i+1
            nb_schedule=nb_schedule+1
        end if
    end do
            nb_schedule=nb_schedule-1

end subroutine find_schedule

!*******************************************************************************
!*******************************************************************************

subroutine get_schedule(z3d_schOBJ,nb_schedule,envi)
!------------------------------------------------------------------------------
! Find files (on a mac or a PC) and set SCHEDULE OBJ
! Updated 28/Sep/13 by MB.
!------------------------------------------------------------------------------

implicit none

integer,intent(in)                                        :: envi
character(1280),dimension(:),allocatable                  :: file_names
integer                                                   :: Total_Z3D,x,i
character(512)                                            :: header,timestamp
integer*8                                                 :: ts_int8
integer*8,dimension(:,:),allocatable                      :: headers
integer                                                   :: channel,ios
integer                                                   :: file_size
integer                                                   :: adc_rate
type (z3d_tschedule),dimension(:),intent(inout)           :: z3d_schOBJ
integer,intent(out)                                       :: nb_schedule
character(10),dimension(:,:),allocatable                  :: date_time


! Collect all z3d files (Mac or PC environment)
if (envi==0) then !IF MAC
    call get_all_Z3Ds_UNIX(file_names,Total_Z3D) 
elseif (envi==1) then !IF PC
    call get_all_Z3Ds_PC(file_names,Total_Z3D) 
end if

call log_1(Total_Z3D)

allocate(headers(Total_Z3D,5))
allocate(date_time(Total_Z3D,2))

do i=1,Total_Z3D

    open(unit=1, file =file_names(i), action = 'read', access='stream', form='unformatted')
    read (1) header
    inquire(FILE=file_names(i), SIZE=file_size)
    close(1)
    
    x = index(header, ", Sync:") 
    timestamp= header(x-19:x-10)//" "//header(x-8:x-1)                                ! Get GPS start-timestamp string
    
    date_time(i,1)=header(x-14:x-13)//'/'//header(x-11:x-10)//'/'//header(x-19:x-16)  ! get date (string)
    date_time(i,2)=header(x-8:x-1)                                                    ! get time (string)
    
    x = index(header, "Channel: ")
    call value(header(x+9:x+10),channel,ios)                                          ! Get Channel number
    
    x = index(header, "Rate: ")
    call value(header(x+6:x+9),adc_rate,ios)                                          ! Get A/D Rate (frequency)
    
    call datenum(timestamp,ts_int8) 
    headers(i,1)=i
    headers(i,2)=ts_int8
    headers(i,3)=channel
    headers(i,4)=file_size
    headers(i,5)=adc_rate
       
end do
    
! Sort array per schedule and per channels
call row_sort_by2(headers,2,3)
call log_2(Total_Z3D,date_time,headers,file_names)  !print schedules Z3Dfiles :-)
! Find schedules
call find_schedule(file_names,Total_Z3D,headers,z3d_schOBJ,nb_schedule,date_time)

deallocate(headers)
deallocate(date_time)

end subroutine get_schedule

!*******************************************************************************
!*******************************************************************************

end module z3d_schedule