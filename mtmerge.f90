program mtmerge
!---------------------------------------------------------------------------------------
! Main program. 
! Convert Z3D files into MT CAC files.
! Updated on 29/Sept/2013
!---------------------------------------------------------------------------------------

use param_data
use z3d_file, only : mergeTS, syncZ3D   ! ( 5 / 3 )
use z3d_data, only : generate_z3d_objs  ! ( 2 )
use z3d_schedule, only : get_schedule   ! ( 1 )
use cac_file, only : populate_CAC_OBJ   ! ( 4 )
use log

implicit none

! Program timing variables  ----------------------------------------------------------
real                                            :: t1, t2
! Variable  ---------------------------------------------------------------------------
integer                                         :: nb_schedule
integer                                         :: sch
integer                                         :: TS_write_length     !! CAC TS length
logical                                         :: status
!---------------------------------------------------------------------------------------

! Zonge International message
call log_13

!---------------------------------------------------------------------------------------
! 1 GET SCHEDULE FILE ------------------------------------------------------------------
!---------------------------------------------------------------------------------------

call get_schedule(z3d_schOBJ,nb_schedule,envi)

if (nb_schedule>0) then 

call log_3(nb_schedule)  !! Display Nb schedule message

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call cpu_time(t1)   ! For timeing purposes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

do sch=1,nb_schedule

  call log_4(sch,nb_schedule,z3d_schOBJ(sch)%inb_file,z3d_schOBJ(sch)%cfiles)  !! Display working schedule message

!---------------------------------------------------------------------------------------
! 2 CREATE Z3D OBJECT and POPULATE WITH HEADER, META, CAL information ------------------
!---------------------------------------------------------------------------------------

  call generate_z3d_objs(z3d_schOBJ,z3d_HOBJ,z3d_MOBJ,sch,envi)

!---------------------------------------------------------------------------------------
! 3 FOUND FILE SYNC LOCATIONS ----------------------------------------------------------
!---------------------------------------------------------------------------------------

  call syncZ3D(z3d_schOBJ(sch)%cfiles,z3d_schOBJ(sch)%inb_file,GPS_start,GPS_buf_size,allocMem,z3d_HOBJ,z3d_schOBJ,sch,status)

!---------------------------------------------------------------------------------------
! 4 GENERATE CAC FILE ------------------------------------------------------------------
!---------------------------------------------------------------------------------------

  if (status .eqv. .true.) then ! If SYNC OK then CONTINUE else GO TO next schedule.
    
    ! Populate CAC object
      call populate_CAC_OBJ(cac_Hobj,z3d_schOBJ(sch),z3d_HOBJ,z3d_MOBJ)
    
    ! Display reading info message
      call log_6(z3d_schOBJ(sch)%inb_file,z3d_schOBJ(sch)%buffer_size,z3d_schOBJ(sch)%nb_buffer,z3d_schOBJ(sch)%res_bytes)
      call log_12(z3d_HOBJ(1:z3d_schOBJ(sch)%inb_file)%istart_time,z3d_HOBJ(1:z3d_schOBJ(sch)%inb_file)%istart_byte)

    !-----------------------------------------------------------------------------------
    ! 5 WRITE CAC FILE (HEADER + TS) ---------------------------------------------------
    !-----------------------------------------------------------------------------------

      call mergeTS(flag,z3d_schOBJ(sch)%inb_file,z3d_HOBJ,z3d_schOBJ,TS_write_length,sch,cac_Hobj,maxDroppingPoints)
      call log_9(TS_write_length) ! Display CAC Nb of points message

  end if

end do ! End of Schedule loop

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call cpu_time(t2)
print *, "processed in:", t2-t1, "seconds."
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end if

end program mtmerge