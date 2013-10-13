module param_data
!-------------------------------------------------------------------------------
! Parameters
! Updated 27/Sept/13 by MB
!-------------------------------------------------------------------------------

use z3d_data
use z3d_schedule
use cac_file

! Parameters
integer,public,parameter                        :: envi=0                      !! Unix(Linux,mac)=0 or M-dos Windows system=1
integer,public,parameter                        :: GPS_start=4                !! syncZ3D : Skip 4 first seconds (known garbage data)
integer,public,parameter                        :: GPS_buf_size=8192          !! syncZ3D : Buffer used to sync channels
integer,public,parameter                        :: allocMem=512               !! syncZ3D : Maximum number of Timestamps memory available          
integer,public,parameter                        :: flag=-1                    !! mergeTS : Always -1 (ff ff ff ff) 
integer,public,parameter                        :: maxDroppingPoints=4096     !! Maximum allowed dropping points
integer,public,parameter                        :: max_channel=16             !! Maximum allocation for Channel OBJs
integer,parameter                               :: max_schedule=256           !! Maximum allocation for Schedule OBJ
type(z3d_tTSH),dimension(max_channel)           :: z3d_HOBJ                   !! HEADER object initialization
type(z3d_tTSM),dimension(max_channel)           :: z3d_MOBJ                   !! META object initialization
type (z3d_tschedule),dimension(max_schedule)    :: z3d_schOBJ                 !! SCHEDULE OBJECT initialization
type (cac_tTSH)                                 :: cac_Hobj                   !! CAC OBJECT initialization

end module param_data