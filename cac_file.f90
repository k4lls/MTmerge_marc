module cac_file
!-------------------------------------------------------------------------------
! GDP32-24 binary CAC file module
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

! Public methods
public :: populate_CAC_OBJ             ! POPULATE CAC OBJ (called from mtmerge)
public :: write_CAC_header             ! Write CAC header (called from z3d_file-mergeTS)
public :: write_beginning_of_record_TS  ! Write the beginning of the TS record (called from z3d_file-mergeTS)
! Private methods
private :: merge_channel
private :: write_record_nav            ! Write navigation record
private :: write_record_meta           ! Write metadata and / or calibration
private :: metadata                    ! Set the content of the metadata record
private :: calibration                 ! Set the content of the calibration record
private :: META_line                   ! write a line to CAC file and count the number of bytes

! Type Schedule
type, public :: cac_tTSH
        character(10)                           :: cDate                ! Date
        character(8)                            :: cTime                ! Time
        character(4)                            :: cADCrate             ! A/D freq
        character(3)                            :: cNbChannel           ! Nb Channel
        character(256)                          :: cLowPass             ! Low pass
        character(256)                          :: cSerial              ! Serials
        character(256)                          :: cGain                ! Gains
        character(256)                          :: cch_factor           ! Channels factor
        character(256)                          :: cRxStn               ! RX.STN
        character(256)                          :: cChCMP               ! CH.CMP
        character(256)                          :: iChNumber            ! CH.NUMBER
        character(256)                          :: iChLength            ! CH.LENGHTH
        character(8192),dimension(16)           :: CAL_LINES            ! CALIBRATION LINES
        integer                                 :: cal_status           ! is there calibration status 
end type cac_tTSH

! Generic interface
  INTERFACE write_record
    MODULE PROCEDURE write_record_nav, write_record_meta
  END INTERFACE

!-------------------------------------------------------------------------------
  contains

!*******************************************************************************
!*******************************************************************************

subroutine populate_CAC_OBJ(cac_Hobj,sch_OBJ,z3d_HOBJ,z3d_MOBJ)
!-------------------------------------------------------------------------------
! Populate CAC OBJECT
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

    use z3d_schedule
    use z3d_data
    use str_util

    implicit none
    ! Arguments
    type(cac_tTSH),intent(inout)                    :: cac_Hobj
    type(z3d_tschedule),intent(inout)               :: sch_OBJ
    type(z3d_tTSH),dimension(:),intent(inout)       :: z3d_HOBJ
    type(z3d_tTSM),dimension(:),intent(inout)       :: z3d_MOBJ
    character(256)                                  :: str
    character(8192)                                 :: str_line
    integer                                         :: i, j, I2
    integer                                         :: nb_channel,adc_rate,lp
    integer                                         :: str_line_LL
    CHARACTER(256),dimension(:),allocatable         :: entite
    CHARACTER(8192)                                 :: sBuffer

    ! Local variables
!-------------------------------------------------------------------------------
    
    nb_channel=sch_OBJ%inb_file
    
    allocate(entite(nb_channel))
    
    cac_Hobj%cDate=sch_OBJ%cDate                         ! Date
    cac_Hobj%cTime=sch_OBJ%cTime                         ! Time
    
    call writenum(sch_OBJ%inb_file,str,"i3")
    cac_Hobj%cNbChannel=str                              ! Nb Channel
    
    call writenum(sch_OBJ%iADCrate,str,"i4")
    cac_Hobj%cADCrate=str                                ! ADC Rate
    
    adc_rate=sch_OBJ%iADCrate  
    SELECT CASE (adc_rate)
        CASE (4096);lp=1536;CASE (2048);lp=768;CASE (1024);lp=384;CASE (512);lp=192;CASE (256);lp=96;     
    END SELECT 
    call writenum(lp,str,"i4")
    
    do i=1,nb_channel ; entite(i)=str ; end do
    call merge_channel(nb_channel,entite,sBuffer,I2)
    cac_Hobj%cLowPass=sBuffer(1:I2)
    
    do i=1,nb_channel ; entite(i)=z3d_HOBJ(i)%cSerial ; end do
    call merge_channel(nb_channel,entite,sBuffer,I2)
    cac_Hobj%cSerial=sBuffer(1:I2)
    
    do i=1,nb_channel 
    call writenum(z3d_HOBJ(i)%igain,str,"i1")
    entite(i)='0'//trim(str)//'-0' ; end do
    call merge_channel(nb_channel,entite,sBuffer,I2)
    cac_Hobj%cGain=sBuffer(1:I2)
      
    do i=1,nb_channel ; entite(i)=z3d_HOBJ(i)%cch_factor ; end do
    call merge_channel(nb_channel,entite,sBuffer,I2)
    cac_Hobj%cch_factor=sBuffer(1:I2)

    cac_Hobj%cRxStn=z3d_MOBJ(1)%cRxStn
    
    do i=1,nb_channel ; entite(i)=z3d_MOBJ(i)%cChCMP ; end do
    call merge_channel(nb_channel,entite,sBuffer,I2)
    cac_Hobj%cChCMP=sBuffer(1:I2)
     
    do i=1,nb_channel 
    call writenum(z3d_MOBJ(i)%iChNumber,str,"i4")
    entite(i)=str ; end do
    call merge_channel(nb_channel,entite,sBuffer,I2)
    cac_Hobj%iChNumber=sBuffer(1:I2)
    
    do i=1,nb_channel 
    call writenum(z3d_MOBJ(i)%iChLength,str,"i8")
    entite(i)=str ; end do
    call merge_channel(nb_channel,entite,sBuffer,I2)
    cac_Hobj%iChLength=sBuffer(1:I2)
    
    50 format (F12.6,a1,F12.6,a1,F12.6,a1)
    
    cac_Hobj%cal_status=0
    do i=1,nb_channel
    
    str_line=''
    do j=1,z3d_MOBJ(i)%nb_cal_info
    write(str,fmt=50) z3d_MOBJ(i)%cal_info(j,1),':', z3d_MOBJ(i)%cal_info(j,2),':', z3d_MOBJ(i)%cal_info(j,3),','
    str_line=TRIM(str_line)//TRIM(str)
    end do
    
    str_line_LL=len_trim(str_line)-1
    
    cac_Hobj%CAL_LINES(i)='CAL.SYS,'//z3d_MOBJ(i)%cal_serial//','//str_line(1:str_line_LL)
    cac_Hobj%cal_status=len_trim(cac_Hobj%CAL_LINES(i))-17+cac_Hobj%cal_status
    
    end do
    
    deallocate(entite)
    
end subroutine populate_CAC_OBJ

!*******************************************************************************
!*******************************************************************************

subroutine merge_channel(nb_channel,entite,sBuffer,I2)
!-------------------------------------------------------------------------------
! Cut and Paste strings together (Ex: Ex,Ey,Hx,Hy,Hz)
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

    implicit none
    
    integer,intent(in)                          :: nb_channel
    character(256),dimension(:),intent(in)      :: entite
    CHARACTER(8192),intent(out)                 :: sBuffer
    integer,intent(out)                         :: I2
    integer                                     :: i, I1, NRecLen
    CHARACTER(256)                              :: sRecord

    I2=1
    do i=1,nb_channel
    sRecord=TRIM(entite(i))//','
    I1 = I2
    NRecLen = LEN_TRIM(sRecord)
    I2 = I1 + NRecLen
    sBuffer(I1:I2) = sRecord(1:NRecLen)
    end do
    I2=I2-2
    
end subroutine merge_channel

!*******************************************************************************
!*******************************************************************************

subroutine write_CAC_header(iunit,TS_NPNT_pos,cac_Hobj)
!-------------------------------------------------------------------------------
! Write Navigation, Metadata, Calibration records
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

integer,intent(in)                       :: iunit
integer                                  :: ilength_record
integer(2)                               :: itype
integer(1),dimension(41)                 :: icontent_nav
CHARACTER(8192)                          :: sBuffer
integer                                  :: NBufLen
integer,intent(out)                      :: TS_NPNT_pos
type(cac_tTSH),intent(inout)             :: cac_Hobj


! Nav record  -------------------------
ilength_record=43
itype=4
icontent_nav=0

call write_record(iunit,ilength_record,itype,icontent_nav)
! -------------------------------------

! Meta record -------------------------
call metadata(sBuffer,NBufLen,TS_NPNT_pos,cac_Hobj)

ilength_record=NBufLen
itype=514

TS_NPNT_pos=ftell(iunit)+10+TS_NPNT_pos+1

call write_record(iunit,ilength_record,itype,sBuffer)
! -------------------------------------


! Cal record -------------------------
call calibration(sBuffer,NBufLen,cac_Hobj)

ilength_record=NBufLen
itype=528

if (cac_Hobj%cal_status>0) then
call write_record(iunit,ilength_record,itype,sBuffer)
end if 
! -------------------------------------

end subroutine write_CAC_header

!*******************************************************************************
!*******************************************************************************

subroutine write_record_nav(iunit,ilength_record,itype,icontent)
!-------------------------------------------------------------------------------
! Write Navigation record
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

integer                              :: iflag=-1
integer,intent(in)                   :: ilength_record
integer(2),intent(in)                :: itype
integer,intent(in)                   :: iunit
integer(1),dimension(:),intent(in)   :: icontent

    write(iunit), ilength_record, iflag, itype, icontent, ilength_record

end subroutine write_record_nav

subroutine write_record_meta(iunit,ilength_record,itype,icontent)
!-------------------------------------------------------------------------------
! Write Meta record
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

integer                              :: iflag=-1
integer,intent(in)                   :: ilength_record
integer(2),intent(in)                :: itype
integer,intent(in)                   :: iunit
CHARACTER(8192),intent(out)          :: icontent

write(iunit), ilength_record+2, iflag, itype, icontent(1:ilength_record), ilength_record+2

end subroutine write_record_meta

!*******************************************************************************
!*******************************************************************************

subroutine write_beginning_of_record_TS(iunit,ilength_record,itype,record_pos)
!-------------------------------------------------------------------------------
! Write start of TS records
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none

integer                             :: iflag=-1
integer,intent(in)                  :: ilength_record
integer(2),intent(in)               :: itype
integer,intent(in)                  :: iunit
integer,intent(out)                 :: record_pos

    record_pos = ftell( iunit )
    write(iunit), ilength_record, iflag, itype

end subroutine write_beginning_of_record_TS

!*******************************************************************************
!*******************************************************************************

subroutine metadata(sBuffer,NBufLen,TS_NPNT_pos,cac_Hobj)
!-------------------------------------------------------------------------------
! Generate metadata content
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

    implicit none

! Local variables
  INTEGER                                         :: I1, I2
  CHARACTER(256)                                  :: sRecord
  CHARACTER(8192),intent(out)                     :: sBuffer
  integer,intent(out)                             :: NBufLen,TS_NPNT_pos
  type(cac_tTSH),intent(inout)                    :: cac_Hobj

      I2=0

      sRecord = 'HEADER.TYPE,Survey'
      call META_line(sRecord,I1,I2,sBuffer)
        
      sRecord = 'SURVEY.ACQMETHOD,timeseries'
      call META_line(sRecord,I1,I2,sBuffer)

      sRecord = 'SURVEY.TYPE,AMT'
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'GDP.TYPE,OTHER'
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'DATA.VERSION,1.0'
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'GDP.PROGVER,1.0'
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'GDP.DATE,'//cac_Hobj%cDate
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'GDP.TIME,'//cac_Hobj%cTime
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'TS.ADFREQ,'//cac_Hobj%cADCrate
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'TS.NPNT,         .'
      call META_line(sRecord,I1,I2,sBuffer)
      TS_NPNT_pos=I2-10
      
      sRecord = 'CH.NUMON,'//cac_Hobj%cNbChannel
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'RX.STN,'//cac_Hobj%cRxStn
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'CH.CMP,'//cac_Hobj%cChCMP
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'CH.NUMBER,'//cac_Hobj%iChNumber
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'CH.LENGTH,'//cac_Hobj%iChLength
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'CH.GAIN,'//cac_Hobj%cGain
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'UNIT.LENGTH,m'
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'CH.LOWPASS,'//cac_Hobj%cLowPass
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'CH.FACTOR,'//cac_Hobj%cch_factor
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'CH.ADCARDSN,'//cac_Hobj%cSerial
      call META_line(sRecord,I1,I2,sBuffer)
           
!     Update buffer length
      NBufLen= I2
      
end subroutine metadata
    
!*******************************************************************************
!*******************************************************************************

subroutine calibration(sBuffer,NBufLen,cac_Hobj)
!-------------------------------------------------------------------------------
! Generate metadata content
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

    use str_util

    implicit none

! Local variables
  INTEGER                                         :: I1, I2
  CHARACTER(256)                                  :: sRecord
  CHARACTER(8192),intent(out)                     :: sBuffer
  integer,intent(out)                             :: NBufLen
  type(cac_tTSH),intent(inout)                    :: cac_Hobj
  integer                                         :: i,nb_channel, ios
  
  call value(cac_Hobj%cNbChannel,nb_channel,ios)

      I2=0

      sRecord = 'HEADER.TYPE,Calibrate'
      call META_line(sRecord,I1,I2,sBuffer)
      
      sRecord = 'CAL.VER,0.21'
      call META_line(sRecord,I1,I2,sBuffer)
        
    do i=1,nb_channel
      sRecord = cac_Hobj%CAL_LINES(i)
      if (len_trim(sRecord)>17) then
      call META_line(sRecord,I1,I2,sBuffer)
      end if
    end do
           
!     Update buffer length
      NBufLen= I2
      
end subroutine calibration
    
!*******************************************************************************
!*******************************************************************************

subroutine META_line(sRecord,I1,I2,sBuffer)
!-------------------------------------------------------------------------------
! Get sRecord length and put sRecord into sBuffer
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

implicit none
      
CHARACTER(256),intent(in)     :: sRecord
INTEGER,intent(inout)         ::  I1, I2
CHARACTER(8192),intent(inout) :: sBuffer
integer                       :: NRecLen
CHARACTER(1), PARAMETER       :: sLF=CHAR(10) ! LF flags end of text subrecords
      
      I1 = I2 + 1
      NRecLen = LEN_TRIM(sRecord)
      I2 = I1 + NRecLen
      sBuffer(I1:I2) = sRecord(1:NRecLen)//sLF      
      
end subroutine META_line

end module cac_file