program run

use z3d_file
use ts_data

implicit none

character(len=256)  :: filename
real                :: t1, t2
integer             :: fid,buffer_size,nb_buffer,buffering
integer             :: flag,nb_channel,row,j,min_length_TSbuffer
integer, dimension(:,:), allocatable :: TS_buffer, pos_bit,length_TS_buffer,zub
type (z3d_ts)                   :: ts1





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call cpu_time(t1)   ! For timeing purposes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    

filename='data/ZEN161.Z3D'

buffer_size=16   
nb_buffer=1

flag=-1
nb_channel=1
j=1

allocate(pos_bit(1,nb_channel))
allocate(length_TS_buffer(1,nb_channel))
allocate(TS_buffer(buffer_size,1))

fid=1
pos_bit(1,1)=2881

length_TS_buffer(1,2)=30

open(fid, file=filename, action = 'read', access='stream', form='unformatted')

    do buffering=1,nb_buffer
    
        print *, 'BUF :', buffering

        do row=1,nb_channel

            call read_buffer_TS(fid,buffer_size,pos_bit(1,row),flag,TS_buffer,length_TS_buffer(1,row),j)
            pos_bit(1,row)=pos_bit(1,row)+buffer_size*4;   
            
            print *,'row:',row   ,nb_channel  
            print *, TS_buffer(1:length_TS_buffer(1,row),1)
            print *, 'LL',length_TS_buffer(1,row)
            
    end do

        
        min_length_TSbuffer = MINVAL(length_TS_buffer(1,1:nb_channel))
        print *, '--------------------'
        print *, 'MIN', min_length_TSbuffer

        !ts1%ts_in_memory=66
        
        !call print_Ts(ts1)
        
        allocate( zub(10,1) )
        zub=12
        ts1=make_z3d_ts_object(zub,2)
        call print_Ts(ts1)
        
        !print *, ts1
    
    end do
    
    
close(fid)

deallocate(TS_buffer)
deallocate(pos_bit)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call cpu_time(t2)
print *, "processed in:", t2-t1, "seconds."
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program run