module log

contains

subroutine log_1(Total_Z3D)
!-------------------------------------------------------------------------------
! Display Total number of Z3D file(s) found
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

integer                         :: Total_Z3D

    print *, ''
    print *, '--------------------------------'
    print *, 'Total Number of Z3D file(s) found : ' , Total_Z3D
    print *, '--------------------------------'
    print *, ''

end subroutine log_1

!*******************************************************************************
!*******************************************************************************

subroutine log_2(Total_Z3D,date_time,headers,file_names)
!-------------------------------------------------------------------------------
! Display file(s) found
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

integer,intent(in)                                                  :: Total_Z3d
character(10),dimension(:,:),allocatable,intent(in)                 :: date_time
character(1280),dimension(:),allocatable,intent(in)                 :: file_names
integer*8,dimension(:,:),allocatable,intent(in)                     :: headers
integer                                                             :: i


    print *, 'Date  -  ','Time  -  ','Channel Nb  -  ','bytes  -  ','SR  -  ','path'
    print *, '-------------------------------------------------------------------------------------------------'
    do i=1,Total_Z3D
      print *, trim(date_time(headers(i,1),1)),' ',trim(date_time(headers(i,1),2)),' ',headers(i,3:5),trim(file_names(headers(i,1)))
      if (headers(i, 2) /= headers(i+1, 2)) then
            print *, '-------------------------------------------------------------------------------------------------'
      end if
end do

end subroutine log_2

!*******************************************************************************
!*******************************************************************************

subroutine log_3(nb_schedule)
!-------------------------------------------------------------------------------
! Display Total number of schedule(s)
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

integer                                          :: nb_schedule

    print *,''
    print *,''
    print *, '--------------------------------'
    print *, 'Nb of Schedule(s) : ' , nb_schedule
    print *, '--------------------------------'
    print *, char(7)  ! bip
    print *, 'Press Enter to continue' 
    read( *, * ) 

end subroutine log_3

!*******************************************************************************
!*******************************************************************************

subroutine log_4(sch,nb_schedule,nb_file,cfiles)
!-------------------------------------------------------------------------------
! Display the progress of the processing
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

integer,intent(in)                                 :: sch
integer,intent(in)                                 :: nb_schedule
integer,intent(in)                                 :: nb_file
character(*),dimension(:),intent(in)               :: cfiles
integer                                            :: x

    print *, '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    print *, 'PROGRESS : ' , sch , '/' , nb_schedule
    print *, '--------------------------------'
    print *, '-- Files  ----------------------'
    do x=1,nb_file
        print *, trim(cfiles(x))
    end do

end subroutine log_4

!*******************************************************************************
!*******************************************************************************

subroutine log_5(status,nb_channel,timestamps)
!-------------------------------------------------------------------------------
! Display Syncing status
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

logical,intent(in)                      :: status
integer,intent(in)                      :: nb_channel
integer,dimension(:,:,:),intent(in)     :: timestamps
integer                                 :: row

    if (status .eqv. .false.) then
        print *, '------------------------------'
        print *, 'Error : Sync fail, found no reference GPS timestamps though out channels'
        do row=1,nb_channel
        print *, timestamps(1:16,1,row)
        end do
        print *, '------------------------------'
        print *, char(7)  ! bip
        print *, 'Press Enter to continue' 
        read( *, * ) 
        else 
        print *, '------------------------------'
        print *, 'Files are correctly Synced ---'
        print *, '------------------------------'
    end if

end subroutine log_5

!*******************************************************************************
!*******************************************************************************

subroutine log_6(inb_file,buffer_size,nb_buffer,res_bytes)
!-------------------------------------------------------------------------------
! Display processing variables 1
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

integer,intent(in)                          :: inb_file
integer,intent(in)                          :: buffer_size
integer,intent(in)                          :: nb_buffer
integer,intent(in)                          :: res_bytes

    print *, '--------------------------------'
    print *, 'Nb of file(s) : ',  inb_file , 'Buffer size : ', buffer_size
    print *, 'Nb of buffer(s) : ' , nb_buffer , 'Last bytes : ', res_bytes
    print *, '--------------------------------'
    
end subroutine log_6

!*******************************************************************************
!*******************************************************************************

subroutine log_7(CAC_file_name)
!-------------------------------------------------------------------------------
! Display processed files
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

character(*),intent(in)                      :: CAC_file_name

    print *, 'File name : ' , CAC_file_name

end subroutine log_7

!*******************************************************************************
!*******************************************************************************

subroutine log_8(maxDroppingPoints,buffering,maxTestExtra,test_extra)
!-------------------------------------------------------------------------------
! Display error message if dropping points appear
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

integer,intent(in)                  :: maxDroppingPoints
integer,intent(in)                  :: buffering
integer,intent(in)                  :: maxTestExtra
integer,dimension(:),intent(in)     :: test_extra

      print *, char(7)  ! bip
      print *, '----------------------------------------------------------------------'      
      print *, 'CAC_buffer_memory is overload : Deficient channel are missing more than', maxDroppingPoints
      print *, 'at Buffer :', buffering, 'Missing points per channel :', maxTestExtra-test_extra
      print *, 'The CAC file is still readable but most likely corrupted'
      print *, '----------------------------------------------------------------------'

end subroutine log_8

!*******************************************************************************
!*******************************************************************************

subroutine log_9(TS_write_length)
!-------------------------------------------------------------------------------
! Display TS total length
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

integer,intent(in)                               :: TS_write_length

    print *,'CAC TS length : ', TS_write_length
    print *, '--------------------------------'

end subroutine log_9

!*******************************************************************************
!*******************************************************************************

subroutine log_10
!-------------------------------------------------------------------------------
! Display error file size (general error)
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

    print *, 'Error File size could not be determined'
    print *, char(7)  ! bip
    print *, 'Press Enter to continue' 
    read( *, * ) 

end subroutine log_10

!*******************************************************************************
!*******************************************************************************

subroutine log_11  
!-------------------------------------------------------------------------------
! Display error file size (too big)
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

    print *, 'Files size are too big'
    print *, 'Select a new size(bytes)'
    print *, char(7)  ! bip

end subroutine log_11 

!*******************************************************************************
!*******************************************************************************

subroutine log_12(istart_time,istart_byte)
!-------------------------------------------------------------------------------
! Display processing variables 2
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

integer,dimension(:),intent(in)                          :: istart_time
integer,dimension(:),intent(in)                          :: istart_byte

    print *, 'Timestamps : ' , istart_time
    print *, '     Bytes : ' , istart_byte
    print *, '--------------------------------'

end subroutine log_12


end module log