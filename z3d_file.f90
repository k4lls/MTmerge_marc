module z3d_file
!------------------------------------------------------------------------------
! Module for merging and syncing Z3D files together.
! Updated 28/Sep/13 by MB.
!------------------------------------------------------------------------------

use z3d_data
use log

! Public methods
public  ::  mergeTS         ! Write final CAC file 
public  ::  syncZ3D         ! Find file matching the sync
! Private methods
private :: read_GPSstamps   ! Read GPS time stamp
private :: z3d_found_sync   ! Find Z3D schedule sync
private :: read_buffer_TS   ! Read a buffer of TS
private :: get_buffer_info  ! Set the optimized buffer size based on the file sizes
private :: checkSize        ! Check file size

!-------------------------------------------------------------------------------
contains

!*******************************************************************************
!*******************************************************************************

subroutine get_buffer_info(TS_bytes,TS_nb_buffer,TS_buffer_size,total_bytes_residual)
!-------------------------------------------------------------------------------
! Find optimized buffer size based on file sizes.
! Updated 23/Sep/13 by MB.
!-------------------------------------------------------------------------------

implicit none

integer,dimension(:),intent(in)             :: TS_bytes
integer,intent(out)                         :: TS_buffer_size
integer,intent(out)                         :: TS_nb_buffer
integer,intent(out)                         :: total_bytes_residual
integer                                     :: min_length_file

min_length_file=MINVAL(TS_bytes)

if (min_length_file < 2**15) then 
    TS_buffer_size=2**12
elseif (min_length_file >= 2**15 .and. min_length_file < 2**17) then
    TS_buffer_size=2**13
elseif (min_length_file >= 2**17 .and. min_length_file < 2**21) then
    TS_buffer_size=2**15
    elseif (min_length_file >= 2**21) then
    TS_buffer_size=2**16
end if

TS_nb_buffer=min_length_file/TS_buffer_size
total_bytes_residual=min_length_file-TS_nb_buffer*TS_buffer_size

end subroutine get_buffer_info

!*******************************************************************************
!*******************************************************************************

subroutine read_GPSstamps(full_filename,buffer_size,allocMem,length_file,GPS_block,length_GPS_block,file_size)
!-------------------------------------------------------------------------------
! Read GPS timestamps and output array of first values.
! Updated 15/Sep/13 by MB.
!-------------------------------------------------------------------------------

implicit none

character(len=1280), intent(in)                  :: full_filename
integer,intent(in)                               :: buffer_size
integer,intent(in)                               :: allocMem
integer,dimension(:,:),intent(out)               :: GPS_block
integer, intent(out)                             :: length_file,length_GPS_block
integer,dimension(:,:),allocatable               :: buffer
integer                                          :: j, bit, flag_inc, bit_offset
integer,intent(in)                               :: file_size
integer                                          :: buffer_read, flag
integer                                          :: buffer_size_used

    
    ! Correct if header is not a binary number (ex: user manually changed header) & get total number of bytes
    bit=1
    bit_offset=mod(file_size,4)
    bit=bit+bit_offset
    length_file=file_size-bit_offset    

    ! Set buffer
    buffer_size_used=buffer_size
    if (buffer_size_used>length_file/4) then
    buffer_size_used=length_file/4
    end if
    
    allocate(buffer(buffer_size_used,1))
    
    ! Open Z3D file
    open(unit=1, file =full_filename, action = 'read', access='stream', form='unformatted')

    flag=-1
    flag_inc=1
    j=1        
    read(unit=1, pos=bit) buffer
    do buffer_read=1,buffer_size_used
        if (buffer(j,1)==flag) then
            GPS_block(flag_inc,1) = buffer(j+1,1)/1024    ! get first GPS stamp
            GPS_block(flag_inc,2) = bit+(j+8)*4           ! bit pointer
            flag_inc=flag_inc+1
            j=j+9
        else
            j=j+1
        end if
        if (j >= buffer_size_used-1 .or. flag_inc>=allocMem-1) exit
    end do
    
    length_GPS_block=flag_inc-1

    close (1) 
    
    deallocate(buffer)    
   
end subroutine 

!*******************************************************************************
!*******************************************************************************

subroutine z3d_found_sync(GPS_start,timestamps,min_length_GPS,nb_channel,selected_GPS,status)
!-------------------------------------------------------------------------------
! Find sync-able timestamp value.
! Updated 16/Sep/13 by MB.
!-------------------------------------------------------------------------------

use list_util

implicit none

! Arguments
integer,intent(in)                         :: min_length_GPS
integer,intent(in)                         :: GPS_start
integer,intent(in)                         :: nb_channel
integer,dimension(:,:,:),intent(inout)     :: timestamps
integer,dimension(:,:),intent(out)         :: selected_GPS
logical,intent(out)                        :: status
! Local variables
integer                                    :: length_used
integer                                    :: timestamps_test
integer                                    :: value_test
integer                                    :: iMatch,iflag,row

selected_GPS=0
status=.false.
length_used=min_length_GPS-GPS_start
do timestamps_test=GPS_start,length_used
   value_test=timestamps(timestamps_test,1,1)
    do row=1,nb_channel
        call List_Append(value_test,length_used,timestamps(GPS_start:min_length_GPS,1,row),iMatch,iflag)
        if (iflag>0 ) then ; exit
        else
            selected_GPS(row,1)=value_test
            selected_GPS(row,2)=timestamps(iMatch+GPS_start-1,2,row)
            if (row==nb_channel) then
                status=.true. ; end if
        end if
    end do
    if (status .eqv. .true.) exit  
end do

call log_5(status,nb_channel,timestamps,GPS_start,length_used) !! Display Syncing message
   
end subroutine z3d_found_sync

!*******************************************************************************
!*******************************************************************************

subroutine read_buffer_TS(fid,buffer_size,pos_bit,flag,TS_buffer,length_TS_buffer,j)
!-------------------------------------------------------------------------------
! Read one buffer of binary Timeseries
! Updated 12/Sep/13 by MB.
!-------------------------------------------------------------------------------
implicit none
    
integer,intent(in)                               :: fid,buffer_size,pos_bit,flag
integer,intent(out)                              :: length_TS_buffer
integer,intent(inout)                            :: j
integer,dimension(:),intent(inout)               :: TS_buffer
integer,dimension(:)                             :: buffer(buffer_size)
integer                                          :: buffer_read,TS_inc

        read(unit=fid, pos=pos_bit) buffer 
        TS_inc=1
        do buffer_read=1,buffer_size
            if (buffer(j)==flag) then
                j=j+9
            else
                TS_buffer(TS_inc) = buffer(j)
                j=j+1
                TS_inc=TS_inc+1  
            end if 
            if (j >= buffer_size+1) exit         
        end do
        j=j-buffer_size        
        length_TS_buffer=TS_inc-1
 
end subroutine read_buffer_TS

!*******************************************************************************
!*******************************************************************************

subroutine checkSize(nb_channel,ilength_file)
!-------------------------------------------------------------------------------
! Check size of files
! if too big ask for a new size file
! or read config.cfg maximum size
! Updated 28/Sep/13 by MB.
!-------------------------------------------------------------------------------

implicit none

integer,intent(in)                               :: nb_channel
integer                                          :: LL
logical                                          :: file_exists
logical                                          :: error1, error2
integer                                          :: row
integer,dimension(:),intent(inout)               :: ilength_file

error1=.true.
error2=.true.

INQUIRE(FILE="config.cfg", EXIST=file_exists)
if (file_exists .eqv. .true.) then 
open(1, file="config.cfg", action = 'read', access='sequential', form='formatted') ! CAC 
read(1,'(i8)'), LL
close(1)
else
LL=MAXVAL(ilength_file(:))
end if

do row=1,nb_channel
    if (ilength_file(row)>LL) then
        ilength_file(row)=LL
    end if
    if (ilength_file(row)==-1) then
        error1=.false.
    elseif (ilength_file(row)==-4) then
        error2=.false.
    end if
end do

    if (error1 .eqv. .false.) then   
        call log_10
    end if
    if (error2 .eqv. .false.) then   
        call log_11
        read *, ilength_file(row)
    end if
    
end subroutine checkSize

!*******************************************************************************
!*******************************************************************************

subroutine syncZ3D(full_filename,nb_channel,GPS_start,GPSbu,alMem,z3d_HOBJ,sch_OBJ,sch,status)
!-------------------------------------------------------------------------------
! Find Start time and byte for all the schedule files
! Updated 16/Sep/13 by MB.
!-------------------------------------------------------------------------------

use z3d_schedule

implicit none

character(len=1280),dimension(:),intent(in)      :: full_filename
integer,intent(in)                               :: nb_channel
integer,intent(in)                               :: GPS_start ! skip 4 first seconds (known garbage data)
integer,intent(in)                               :: GPSbu
integer,intent(in)                               :: alMem
integer,intent(in)                               :: sch
! Local variable
integer,dimension(:),allocatable                 :: length_file,TS_bytes
integer                                          :: min_length_file
integer                                          :: row,min_length_GPS
logical                                          :: status
integer,dimension(:),allocatable                 :: length_GPS_block
integer, dimension(:,:,:),allocatable            :: timestamps_mem,timestamps
integer,dimension(:,:),allocatable               :: selected_GPS
integer                                          :: LL

! Types
type(z3d_tTSH),dimension(:),intent(inout)        :: z3d_HOBJ
type(z3d_tschedule),dimension(:),intent(inout)   :: sch_OBJ

allocate(length_file(nb_channel))
allocate(TS_bytes(nb_channel))
allocate(length_GPS_block(nb_channel))
allocate(timestamps_mem(alMem,2,nb_channel))
allocate(selected_GPS(nb_channel,2))

! Check size
call checkSize(nb_channel,z3d_HOBJ(1:nb_channel)%ilength_file)

do row=1,nb_channel
! get for timestamp array
LL=z3d_HOBJ(row)%ilength_file
call read_GPSstamps(z3d_HOBJ(row)%cfull_filename,GPSbu,alMem,length_file(row),timestamps_mem(:,:,row),length_GPS_block(row),LL)
end do

! Find smallest timestamp array
min_length_GPS = MINVAL(length_GPS_block(1:nb_channel))

! Get a rectangular timestamps matrice
allocate(timestamps(min_length_GPS,2,nb_channel))
timestamps=timestamps_mem(1:min_length_GPS,:,:)

! Find sync
call z3d_found_sync(GPS_start,timestamps,min_length_GPS,nb_channel,selected_GPS,status)

! length in bytes of the useful TSs
TS_bytes=(length_file(1:nb_channel)-selected_GPS(:,2))/4

! Abord if sync failed
if (status .eqv. .true.) then

! Find optimal buffer
call get_buffer_info(TS_bytes,sch_OBJ(sch)%nb_buffer,sch_OBJ(sch)%buffer_size,sch_OBJ(sch)%res_bytes)

! Assign synced start time and start byte to TS_OBJ
do row=1,nb_channel
z3d_HOBJ(row)%istart_time=selected_GPS(row,1)
z3d_HOBJ(row)%istart_byte=selected_GPS(row,2)
end do

end if

deallocate(length_GPS_block)
deallocate(timestamps_mem)
deallocate(timestamps)
deallocate(selected_GPS)
deallocate(length_file)

end subroutine

!*******************************************************************************
!*******************************************************************************

subroutine mergeTS(flag,nb_channel,z3d_HOBJ,sch_OBJ,TS_write_length,sch,cac_OBJ,maxDroppingPoints)
!-------------------------------------------------------------------------------
! Write Timeseries to CAC files
! Updated 28/Sep/13 by MB.
!-------------------------------------------------------------------------------

use z3d_schedule
use cac_file

implicit none
! Parameters
integer,intent(in)                                :: flag 
integer,intent(in)                                :: nb_channel
integer,intent(in)                                :: sch
integer,intent(in)                                :: maxDroppingPoints
integer,intent(out)                               :: TS_write_length
type(z3d_tTSH),dimension(:),intent(inout)         :: z3d_HOBJ
type(z3d_tschedule),dimension(:),intent(inout)    :: sch_OBJ
type(cac_tTSH),intent(inout)                      :: cac_OBJ
! Local variables
character(5)                                      :: x1
character(30)                                     :: CAC_file_name
character(10)                                     :: sTS_write_length
integer,dimension(:),allocatable                  :: val
integer, dimension(:,:),allocatable               :: TS_buffer,TS_write,TS_write_buffer
integer, dimension(:), allocatable                :: pos_bit,length_TS_buffer,test_extra,prev
integer                                           :: buffering
integer                                           :: min_length_tsbuffer,row
integer                                           :: maxTestExtra,minTestExtra
integer                                           :: CAC_buffer_memory
integer,dimension(:,:),allocatable                :: residual,TS_write_residual
integer,dimension(:),allocatable                  :: length_residual
integer                                           :: min_length_residual
integer                                           :: TS_buf_size
integer                                           :: TS_nb_buffer
integer                                           :: total_bytes_residual
integer                                           :: record_pos,n,fseek
integer                                           :: TS_NPNT_pos
integer(2)                                        :: TS_record
logical                                           :: status

! Define variables
status=.true.
TS_buf_size=sch_OBJ(sch)%buffer_size
TS_nb_buffer=sch_OBJ(sch)%nb_buffer
total_bytes_residual=sch_OBJ(sch)%res_bytes
CAC_buffer_memory=TS_buf_size*3    !! Maximum amount of late TS bytes:set to 3 * buffer size (must be > TS_buf_size)

! Allocate memory
allocate(val(nb_channel))
allocate(pos_bit(nb_channel))
allocate(length_TS_buffer(nb_channel))
allocate(prev(nb_channel))
allocate(test_extra(nb_channel))
allocate(TS_buffer(TS_buf_size,nb_channel))
allocate(TS_write(TS_buf_size,nb_channel))
allocate(TS_write_buffer(CAC_buffer_memory,nb_channel))

! Initialize variable
TS_write_length=0
test_extra=0
val(:)=1

! Define CAC file name
write (x1,'(I5.5)') sch
CAC_file_name='sch'//trim(x1)//'.cac'
call log_7(CAC_file_name)   !! Print File name message

! OPEN FILES ---------------------------------------------------------------------------
do row=1,nb_channel
pos_bit(row)=z3d_HOBJ(row)%istart_byte
open(row, file=z3d_HOBJ(row)%cfull_filename, action ='read', access='stream', form='unformatted') ! Z3Ds
end do
open(999, file=CAC_file_name, action = 'write', access='stream', form='unformatted') ! CAC
!---------------------------------------------------------------------------------------

! Write CAC record

call write_CAC_header(999,TS_NPNT_pos,cac_OBJ)

! OPEN TS Record
TS_record=16
call write_beginning_of_record_TS(999,1,TS_record,record_pos)

! START BUFFERING ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
do buffering=1,TS_nb_buffer

! 1- Get Z3D TS buffer -----------------
do row=1,nb_channel     
    call read_buffer_TS(row,TS_buf_size,pos_bit(row),flag,TS_buffer(:,row),length_TS_buffer(row),val(row))
    TS_write_buffer(test_extra(row)+1:test_extra(row)+length_TS_buffer(row),row)=TS_buffer(1:length_TS_buffer(row),row)
    pos_bit(row)=pos_bit(row)+TS_buf_size*4;        
end do

    ! Minimum Z3D TS buffer -------------  
    min_length_TSbuffer = MINVAL(length_TS_buffer(1:nb_channel))     

! 2- write CAC & adjust TS_length differences 
      do row=1,nb_channel  
        ! Generate CAC TS
        TS_write(1:min_length_TSbuffer,row)=TS_write_buffer(1:min_length_TSbuffer,row);        
        ! Adjust possible TS_length differences 
        prev(row)=test_extra(row)
        test_extra(row)=length_TS_buffer(row)+prev(row)-min_length_TSbuffer
            if (test_extra(row)/=0) then
                TS_write_buffer(1:test_extra(row),row)=TS_write_buffer(min_length_TSbuffer+1:length_TS_buffer(row)+prev(row),row);
            end if  
      end do
      
      ! Check for dropping points
      maxTestExtra = MAXVAL(test_extra(1:nb_channel))
      minTestExtra = MINVAL(test_extra(1:nb_channel)) 
      if (maxTestExtra-minTestExtra>=maxDroppingPoints) then
      call log_8(maxDroppingPoints,buffering,maxTestExtra,test_extra)  ! Print dropping points memory overloaded message
      status=.false.
      exit
      end if

! 3- Write CAC and update TS length -------------
       write(999),transpose(TS_write(1:min_length_TSbuffer,1:nb_channel))
       TS_write_length=TS_write_length+min_length_TSbuffer
       
end do
! END BUFFERING +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
! Abord if CAC_buffer_memory overflow
if (status .eqv. .true.) then

! WRITE RESIDUAL ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

 allocate(residual(total_bytes_residual,nb_channel))
 allocate(TS_write_residual(total_bytes_residual,nb_channel))
 allocate(length_residual(nb_channel))
 
! 1- Get last Z3D TS data -----------------
do row=1,nb_channel        
    call read_buffer_TS(row,total_bytes_residual,pos_bit(row),flag,residual(:,row),length_residual(row),val(row))
end do

! Minimum Z3D TS buffer -------------  
    min_length_residual = MINVAL(length_residual(1:nb_channel))  

! 2- write CAC & adjustTS_length differences 
    do row=1,nb_channel  
       ! Generate CAC TS
        TS_write_residual(1:min_length_residual,row)=residual(1:min_length_residual,row); 
    end do
! 3- Write CAC and get TS length -------------
       write(999),transpose(TS_write_residual(1:min_length_residual,1:nb_channel))
       TS_write_length=TS_write_length+min_length_residual

deallocate(residual)
deallocate(length_residual)
deallocate(TS_write_residual)

! END RESIDUAL +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

end if
 
! WRITE RECORD length_file to extremum of TS Record
 write(999),(TS_write_length*nb_channel*4)+2                   ! write to end of TS record
 write(999,pos=record_pos+1),(TS_write_length*nb_channel*4)+2  ! write to beginning of TS record
 
 ! Write Nb of points to meta
 write (sTS_write_length,'(I10)') TS_write_length
 write(999,pos=TS_NPNT_pos-1),ADJUSTL(sTS_write_length)
 
! CLOSE FILES --------------------------------------------------------------------------
do row=1,nb_channel 
    close(row)
end do
    close(999)
!---------------------------------------------------------------------------------------

deallocate(val)
deallocate(pos_bit)
deallocate(length_TS_buffer)
deallocate(prev)
deallocate(test_extra)
deallocate(TS_buffer)
deallocate(TS_write)
deallocate(TS_write_buffer)

end subroutine mergeTS

!*******************************************************************************
!*******************************************************************************

end module z3d_file