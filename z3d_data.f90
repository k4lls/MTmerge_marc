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
private  :: found_str         ! Found string in metadata
private  :: gen_hw_key        ! Generate Hkey for calibration to match setup

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
    ! Calibration
    character(8)                            :: cal_serial       ! Calibration Card SN
    real,dimension(256,3)                   :: cal_info         ! freq,mag,phase
    integer                                 :: nb_cal_info      ! freq,mag,phase

end type z3d_tTSM
!-------------------------------------------------------------------------------

contains 

!*******************************************************************************
!*******************************************************************************

subroutine generate_z3d_objs(sch_OBJ,z3d_HOBJ,z3d_MOBJ,sch,env)
!-------------------------------------------------------------------------------
! Generates of the Z3D Objects
! Updated 25/Sep/13 by MB.
!-------------------------------------------------------------------------------

use z3d_schedule

implicit none

integer,intent(in)                               :: env
type(z3d_tschedule),dimension(:),intent(inout)   :: sch_OBJ
type(z3d_tTSH),dimension(:),intent(inout)        :: z3d_HOBJ
type(z3d_tTSM),dimension(:),intent(inout)        :: z3d_MOBJ
integer,intent(in)                               :: sch
integer                                          :: file

! generate TS_OBJ 
do file=1,sch_OBJ(sch)%inb_file 
call populate_z3d(sch_OBJ(sch)%cfiles(file),sch_OBJ(sch)%iADCrate,z3d_HOBJ,z3d_MOBJ,file,env)
end do

end subroutine generate_z3d_objs

!*******************************************************************************
!*******************************************************************************

subroutine populate_z3d (cfull_filename,iADCrate,z3d_HOBJ,z3d_MOBJ,row,env)
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
    character(*),intent(in)                       :: cfull_filename
    integer,intent(in)                            :: row
    integer,intent(in)                            :: env
    integer,intent(in)                            :: iADCrate
    ! Local variables
    character(256)                                :: cfilename
    character(1024)                               :: cpath
    integer                                       :: ilength_file,islash_index,iext_index,x
    character(16384)                              :: header,meta
    integer                                       :: gain,ios
    integer                                       :: attenuator
    integer                                       :: freq_set
    integer(8)                                    :: hkey
    character(8)                                  :: hkey_str
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
                         
! GET/SET METADATA
call get_meta(z3d_MOBJ,row,header,meta)

! GET/SET CALIBRATION
attenuator=0
freq_set=0
call gen_hw_key(iADCrate,gain,attenuator,freq_set,hkey,hkey_str)

call get_cal(z3d_MOBJ,row,meta,hkey,hkey_str)

!+ end of Header information  +++++++++++++++++++++++++++

end subroutine populate_z3d

!*******************************************************************************
!*******************************************************************************

subroutine get_meta(z3d_MOBJ,row,header,meta)
!-------------------------------------------------------------------------------
! Collect Metadata
! Updated 25/Sep/13 by MB.
!-------------------------------------------------------------------------------

    use str_util
     
    implicit none


    type(z3d_tTSM),dimension(:),intent(inout)     :: z3d_MOBJ
    integer,intent(in)                            :: row
    character(*),intent(in)                       :: header
    character(16384)                              :: meta
    integer                                       :: metaExist
    integer                                       :: nb_meta_record
    integer                                       :: length_header
    integer                                       :: i, extent
    integer,dimension(12)                         :: meta_pos
    integer                                       :: x1, x
    integer                                       :: conv_value, ios, status
    
    length_header=len(header)
    meta_pos=0
    
    metaExist=index(header, 'GPS Brd339/Brd357 Metadata Record')
    
if (metaExist==0) then

! if NO metadata found
    
 z3d_MOBJ(row)%cRxStn='999'             ! RX.STN
 z3d_MOBJ(row)%cChCMP='Ex'            ! CH.CMP
 z3d_MOBJ(row)%iChNumber=888            ! CH.NUMBER
 z3d_MOBJ(row)%iChLength=999          ! CH.LENGTH
    
    else
    
! if metadata found
    
    ! merge metadata record into one  ---------------------
    
    nb_meta_record=countsubstring(header,'GPS Brd339/Brd357 Metadata Record')
    meta_pos(1)=metaExist
    meta=''
    
    if (nb_meta_record>=2) then
        do i=2,nb_meta_record
            meta_pos(i)=index(header(meta_pos(i-1)+33:length_header), 'GPS Brd339/Brd357 Metadata Record')
            meta_pos(i)=meta_pos(i)+meta_pos(i-1)+32
        end do 
    end if
    
    meta_pos(nb_meta_record+1)=meta_pos(nb_meta_record)+512-5
    do i=1,nb_meta_record
        meta=trim(meta)//trim(header(meta_pos(i)+35:meta_pos(i+1)-5))
    end do
    
    
    ! end of merge metadata record into one  --------------

    ! Set META OBJ
 call found_str('RX.STN',meta,x,x1,status)
 if (status==1) then
 z3d_MOBJ(row)%cRxStn=meta(x:x1)
 else; z3d_MOBJ(row)%cRxStn='999'; end if
 
 call found_str('CH.CMP',meta,x,x1,status)
 if (status==1) then
 z3d_MOBJ(row)%cChCMP=meta(x:x1)
 else; z3d_MOBJ(row)%cChCMP='Ex'; end if
 
 call found_str('CH.NUMBER',meta,x,x1,status)
 if (status==1) then
 call value(meta(x:x1),conv_value,ios) 
 z3d_MOBJ(row)%iChNumber=conv_value
 else; z3d_MOBJ(row)%iChNumber=888; end if

 call found_str('CH.VARASP',meta,x,x1,status)
 if (status==1) then
 call value(meta(x:x1),conv_value,ios) 
 z3d_MOBJ(row)%iChLength=conv_value
 else; z3d_MOBJ(row)%iChLength=999; end if
    
end if
    
end subroutine get_meta

!*******************************************************************************
!*******************************************************************************

subroutine found_str(tag,meta,x,x1,status)
!-------------------------------------------------------------------------------
! Found string into an other one.
! Updated 12/Oct/13 by MB.
!-------------------------------------------------------------------------------

use file_util

implicit none

character(*),intent(in)                   :: tag
character(*),intent(in)                   :: meta
integer,intent(out)                       :: x, x1
integer                                   :: length_meta
integer                                   :: length_tag
integer                                   :: status

    status=1

    length_meta=len_trim(meta)
    length_tag=len_trim(tag)
    x = index(meta,trim(tag))+length_tag+1
    if (x==length_tag+1) then ; status=0; end if
    x1 = Index_FExt_beg(meta(x:length_meta) , '|')+x-1


end subroutine found_str

!*******************************************************************************
!*******************************************************************************

subroutine get_cal(z3d_MOBJ,row,meta,hkey,hkey_str)
!-------------------------------------------------------------------------------
! Collect Calibration
! Updated 25/Sep/13 by MB.
!-------------------------------------------------------------------------------

use str_util

implicit none

    type(z3d_tTSM),dimension(:),intent(inout)     :: z3d_MOBJ
    character(*),intent(in)                       :: meta
    integer(8),intent(in)                         :: hkey
    integer,intent(in)                            :: row
    character(1024)                               :: cal
    character(8)                                  :: calserial
    integer                                       :: length_cal
    character(*),intent(in)                       :: hkey_str
    integer                                       :: CalExist
    integer                                       :: x, x1
    real,dimension(256,3)                         :: cal_info
    real                                          :: pi
    character(16)                                 :: hkey_raw
    integer                                       :: nargs, nargs2, nargs3
    character(256),dimension(256)                 :: args
    character(256),dimension(3)                   :: args2
    character(256),dimension(2)                   :: args3
    integer                                       :: i,j,k, ios, status
    
    pi = ACOS(0.0)
    
    CalExist=index(meta, 'CAL.SYS')
    
    if (CalExist>0) then

    ! if CALIBRATION found
     
     call found_str('CAL.SYS',meta,x,x1,status)
     cal=meta(x+10:x1-2)
     length_cal=len_trim(cal)
     z3d_MOBJ(row)%cal_serial=meta(x:x+10)
    
    ! GET ASSOCIATED CALIBRATION
    
    call parse(cal,',',args,nargs)
    
    k=0
    do i=1,nargs
     
      call parse(trim(args(i)),':',args2,nargs2)
      hkey_raw=args2(2)
      CalExist=index(hkey_raw(9:16), hkey_str)    
        if (CalExist==1) then   ! Found the calibration data that is matching the corresponding Hkey
         k=k+1
    
         call parse(args2(3),' ',args3,nargs3)
         call value(args2(1),cal_info(k,1),ios)      ! freq
         call value(args3(1),cal_info(k,2),ios)      ! mag
         call value(args3(2),cal_info(k,3),ios)      ! raw_phase(degree)
         
         cal_info(k,3)=cal_info(k,3)*(pi/180)*1000   ! phase (in milliradian)
    
        end if
    end do
    
        z3d_MOBJ(row)%nb_cal_info=k
        z3d_MOBJ(row)%cal_info=cal_info
    
    ! END GET ASSOCIATED CALIBRATION

        end if


end subroutine get_cal

!*******************************************************************************
!*******************************************************************************

subroutine gen_hw_key(iADCrate,gain,attenuator,freq_set,hkey,hkey_str)
!-------------------------------------------------------------------------------
! Generate hash key for calibration matching
! Updated 25/Sep/13 by MB.
!-------------------------------------------------------------------------------

implicit none

integer,intent(in)                  :: iADCrate
integer,intent(in)                  :: gain
integer,intent(in)                  :: attenuator
integer,intent(in)                  :: freq_set
character(8),intent(out)            :: hkey_str
integer(8),intent(out)              :: hkey
integer                             :: SR

SELECT CASE (iADCrate);CASE (256);SR=0;CASE (512);SR=1;CASE (1024);SR=2;CASE (2048);SR=3;CASE (4096);SR=4;END SELECT

hkey=0
hkey=ior(hkey,SR)
hkey=ior(hkey,ishft(attenuator,9))
hkey=ior(hkey,ishft(freq_set,18))
hkey=ior(hkey,ishft(gain,22))

write (hkey_str,'(z8.8)') , hkey

! Place to implement the collection of calibrations.

end subroutine gen_hw_key

!*******************************************************************************
!*******************************************************************************


end module z3d_data