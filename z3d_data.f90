module z3d_data
!------------------------------------------------------------------------------
! Holds Z3D file Objects (file properties, header, metadata, calibration)
! Updated 11/Sep/13 by MB.
!------------------------------------------------------------------------------

! Public methods
public   :: generate_z3d_objs
! Private methods
private  :: populate_z3d      ! Collect possible header information (header, meta,cal)
private  :: get_meta          ! Find metadata
private  :: get_cal           ! Find Calibration

! File info/ file header type -------------------------------------------------
type,public :: z3d_tTSH
    character(1280)                         :: cfull_filename   ! ex:data/ZEUS234.Z3D
    character(256)                          :: cfilename        ! ex:ZEUS234.Z3D
    character(1024)                         :: cpath            ! ex:data/
    character(8)                            :: cSerial          ! Serial
    character(20)                           :: cch_factor       ! Channel Factor
    integer                                 :: ilength_file     ! size of the file (bytes)
    integer                                 :: istart_time      ! TS start time (gps second)
    integer                                 :: istart_byte      ! TS first byte
    integer                                 :: igain            ! Gain
end type z3d_tTSH
!-------------------------------------------------------------------------------

! Metadata type ----------------------------------------------------------------
type z3d_tTSM
    character(1280)                         :: cRxStn           ! RX.STN
    character(256)                          :: cChCMP           ! CH.CMP
    integer                                 :: iChNumber        ! CH.NUMBER
    integer                                 :: iChLength        ! CH.LENGTH
end type z3d_tTSM
!-------------------------------------------------------------------------------

! Calibration type ----------------------------------------------------------------
type z3d_tTSC
    ! to be define
end type z3d_tTSC
!-------------------------------------------------------------------------------

contains 

!*******************************************************************************
!*******************************************************************************

subroutine populate_z3d (cfull_filename,z3d_HOBJ,z3d_MOBJ,z3d_COBJ,row,env)
!-------------------------------------------------------------------------------
! Constructor for Z3D property / header / metadata / Calibration
! Updated 11/Sep/13 by MB.
!-------------------------------------------------------------------------------
    use file_util
    use str_util
    
    implicit none
    ! Arguments
    type(z3d_tTSH),dimension(:),intent(inout)     :: z3d_HOBJ
    type(z3d_tTSM),dimension(:),intent(inout)     :: z3d_MOBJ
    type(z3d_tTSC),dimension(:),intent(inout)     :: z3d_COBJ
    character(*),intent(in)                       :: cfull_filename
    integer,intent(in)                            :: row
    integer,intent(in)                            :: env
    ! Local variables
    character(256)                                :: cfilename
    character(1024)                               :: cpath
    integer                                       :: ilength_file,islash_index,iext_index,x
    character(16384)                              :: header
    integer                                       :: gain,ios
!-------------------------------------------------------------------------------
      
! Get file size -------------------------------
    inquire(FILE=cfull_filename, SIZE=ilength_file)
    if (ilength_file<=0) then; print *, 'Error, file size too small(-1) or too big(-4)' 
    end if
z3d_HOBJ(row)%ilength_file=ilength_file
    
! Get file name & path ------------------------
if (env==0) then
    islash_index=Index_FName_mac(cfull_filename)
elseif (env==1) then
    islash_index=Index_FName_PC(cfull_filename)
end if
    iext_index=Index_FExt(cfull_filename,'.Z3D')
    cpath=cfull_filename(1:islash_index-1)
    cfilename=cfull_filename(islash_index:iext_index+1)
z3d_HOBJ(row)%cfull_filename=cfull_filename
z3d_HOBJ(row)%cfilename=cfilename
z3d_HOBJ(row)%cpath=cpath

!+ Header information  +++++++++++++++++++++++++++
! Open file
    open(unit=1, file =cfull_filename, action = 'read', access='stream', form='unformatted')
    read (1) header
    close(1)
    
! Get Serial number  ---------------------------
    x = index(header, "Serial: ")    
z3d_HOBJ(row)%cSerial=header(x+10:x+17)


! GAIN -----------------------------------------
    x = index(header, "Gain: ")
    call value(header(x+6:x+7),gain,ios)  

    SELECT CASE (gain)
        CASE (1);gain=0;CASE (2);gain=1;CASE (4);gain=2;CASE (8);gain=3;CASE (16);gain=4;CASE (32);gain=5;CASE (64);gain=6;     
    END SELECT 
z3d_HOBJ(row)%igain=gain

! Factor ---------------------------------------

z3d_HOBJ(row)%cch_factor='9.5367431640625e-10'
                         

call get_meta(z3d_MOBJ,row,header)

!+ Header information  +++++++++++++++++++++++++++

end subroutine populate_z3d

!*******************************************************************************
!*******************************************************************************

subroutine generate_z3d_objs(sch_OBJ,z3d_HOBJ,z3d_MOBJ,z3d_COBJ,sch,env)
!-------------------------------------------------------------------------------
! Generates of the Z3D Objects
! Updated 25/Sep/13 by MB.
!-------------------------------------------------------------------------------

use z3d_schedule

integer,intent(in)                               :: env
type(z3d_tschedule),dimension(:),intent(inout)   :: sch_OBJ
type(z3d_tTSH),dimension(:),intent(inout)        :: z3d_HOBJ
type(z3d_tTSM),dimension(:),intent(inout)        :: z3d_MOBJ
type(z3d_tTSC),dimension(:),intent(inout)        :: z3d_COBJ
integer,intent(in)                               :: sch
integer                                          :: file


! generate TS_OBJ 
do file=1,sch_OBJ(sch)%inb_file 
call populate_z3d(sch_OBJ(sch)%cfiles(file),z3d_HOBJ,z3d_MOBJ,z3d_COBJ,file,env)
end do

end subroutine generate_z3d_objs

!*******************************************************************************
!*******************************************************************************

subroutine get_meta(z3d_MOBJ,row,header)
!-------------------------------------------------------------------------------
! Collect Metadata
! Updated 25/Sep/13 by MB.
!-------------------------------------------------------------------------------

    use str_util
     
    implicit none

    type(z3d_tTSM),dimension(:),intent(inout)     :: z3d_MOBJ
    integer,intent(in)                            :: row
    character(*),intent(in)                       :: header
    integer                                       :: metaExist
    
    metaExist=index(header, 'GPS Brd339/Brd357 Metadata Record')
    
    if (metaExist==0) then
    
z3d_MOBJ%cRxStn='1'             ! RX.STN
z3d_MOBJ%cChCMP='Ex'            ! CH.CMP
z3d_MOBJ%iChNumber=1            ! CH.NUMBER
z3d_MOBJ%iChLength=100          ! CH.LENGTH
    
    else
    
    ! if metadata found
    
    end if
    
end subroutine get_meta

!*******************************************************************************
!*******************************************************************************

subroutine get_cal
!-------------------------------------------------------------------------------
! Collect Calibration
! Updated 25/Sep/13 by MB.
!-------------------------------------------------------------------------------

! Place to implement the collection of calibrations.

end subroutine get_cal

!*******************************************************************************
!*******************************************************************************

end module z3d_data