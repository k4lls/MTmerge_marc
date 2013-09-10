module ts_data

private :: z3d_info,z3d_header
public :: z3d_ts

!! file name / path / total bytes
type z3d_info
    character(256)                          :: filename
end type z3d_info

!!! header
type z3d_header
 
end type z3d_header

!!! Timeseries (binary)
type z3d_ts
    integer,allocatable,dimension(:,:)      :: ts_in_memory
    integer                                 :: length_ts_in_memory
end type z3d_ts

contains 

function make_z3d_ts_object (ts_in_memory,length_ts_in_memory) result (z3d_ts_object)

integer,allocatable,dimension(:,:),intent(in)     :: ts_in_memory
integer,intent(in)                                :: length_ts_in_memory
type(z3d_ts)                                      :: z3d_ts_object

z3d_ts_object= z3d_ts(ts_in_memory,length_ts_in_memory)

end function make_z3d_ts_object

!subroutine add_filename(x,)

!type(ts)          ::  x

!x%filename=

!end subroutine add_filename


subroutine print_Ts(baby)

type(z3d_ts),intent(inout) :: baby

baby%length_ts_in_memory=34

print *, baby%ts_in_memory
print *, 'jolie fleur', baby%length_ts_in_memory

end subroutine print_Ts


end module ts_data