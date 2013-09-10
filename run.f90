program run

use z3d_file
implicit none



character(len=256)  :: filename
real                :: t1, t2
integer             :: file_size,bit_offset,length_file,buffer_size,length_TS
integer             :: num_of_buffer_to_read, num_of_timestamps
integer, dimension(:,:), allocatable :: TS_integer, GPS_block


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call cpu_time(t1)   ! For timeing purposes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    

filename='data/ZEN161.Z3D'
buffer_size=8192
num_of_buffer_to_read=4
num_of_timestamps=50

allocate (GPS_block(num_of_timestamps,2))

! Get file length
call read_GPSstamps(filename,buffer_size,length_file,GPS_block,num_of_buffer_to_read,num_of_timestamps)


print *, ' Start'
print *, GPS_block(1,1)
print *, GPS_block(1,2)

print *, '--------------'
print *, length_file

! Define TS_integer
allocate (TS_integer(length_file/4,1))

call read_TS(filename,GPS_block(1,2),buffer_size,length_file,TS_integer,length_TS)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call cpu_time(t2)
print *, "processed in:", t2-t1, "seconds."
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program run