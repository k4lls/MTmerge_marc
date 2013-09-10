module z3d_file
implicit none
contains

subroutine read_GPSstamps(filename,buffer_size,length_file,GPS_block,num_of_buffer_to_read,num_of_timestamps)

implicit none

    character(len=256), intent(in)                   :: filename
    integer,intent(in)                               :: buffer_size,num_of_buffer_to_read,num_of_timestamps
    integer                                          :: buffer(buffer_size)
    integer                                          :: i, j, bit=1, flag_inc=1, bit_offset, file_size
    integer                                          :: buffer_read, flag
    integer, intent(out)                             :: length_file
    integer, dimension(:,:), allocatable,intent(out) :: GPS_block

    allocate (GPS_block(num_of_timestamps,2))
    
    ! Get Z3D file size (bytes)
    inquire(FILE=filename, SIZE=file_size)
    
    ! If the size of the file cannot be determined display ERROR message.
    if (file_size==-1) then
    print *, 'Error' 
    end if
    
    ! Correct if header is not a binary number (ex: user manually changed header) & get total number of bytes
    bit_offset=mod(file_size,4)
    bit=bit+bit_offset
    length_file=file_size-bit_offset
    
    ! Open Z3D file
    open(unit=1, file =filename, action = 'read', access='stream', form='unformatted')

    flag=-1
    print *, '------------------'
    
    do i=1,num_of_buffer_to_read
        
        ! get buffer
        read(unit=1, pos=bit) buffer
        
        j=1
        do buffer_read=1,buffer_size
            if (buffer(j)==flag) then
            GPS_block(flag_inc,1) = buffer(j+1)/1024   ! get first GPS stamp
            !GPS_block(flag_inc,2) = TS_inc            ! GPS stamp position in TS
            GPS_block(flag_inc,2) = bit+(j+8)*4        ! bit pointer
            flag_inc=flag_inc+1
            j=j+9
            else
            j=j+1
            end if
            if (j == buffer_size+1) exit
        end do
        bit=bit+buffer_size*4
    end do

    close (1)
    
    !    open(unit=3, file='GPSbloc2.dat')!, action = 'write' )!,form='unformatted')
    !101 format (i8)
     !   write (unit=3,fmt=101) GPS_block(:,1)
      !  close(3)
    
    
        print *, '------------------'

    print *, GPS_block(3,1)
    print *, GPS_block(3,2)
   ! print *, GPS_block(3,3)
        print *, '------------------'
        

    
end subroutine read_GPSstamps


subroutine read_TS(filename,bit,buffer_size,length_file,TS_integer,length_TS)
implicit none
    character(len=256), intent(in)                   :: filename
    integer,intent(in)                               :: buffer_size,length_file
    integer                                          :: buffer(buffer_size)
    integer                                          :: i, j, nb_buffer,TS_prev,length_remaining
    integer                                          :: TS_inc,buffer_read,flag,remaining_read
    integer, intent(inout)                           :: bit
    integer, intent(out)                             :: length_TS
    integer, intent(out)                             :: TS_integer(length_file/4,1)
    integer, dimension(:), allocatable               :: remaining
    
   

    nb_buffer=length_file/buffer_size/4
    length_remaining=length_file/4-nb_buffer*buffer_size
    
    allocate(remaining(length_remaining))
    
    remaining=0
    
    print *, 'remaining', remaining
    
    ! Open Z3D file
    open(unit=1, file=filename, action = 'read', access='stream', form='unformatted')
    open(unit=2, file='data.dat')!, action = 'write' )!,form='unformatted')
    100 format (i8)
    
    flag=-1
    TS_inc=1
    TS_prev=1
    
    ! Reformat Timeseries
    do i=1,nb_buffer-1
        
        ! read buffer
        read(unit=1, pos=bit) buffer
        
        ! get timeseries
        j=1
        do buffer_read=1,buffer_size
            if (buffer(j)==flag) then
            j=j+9
            else
            TS_integer(TS_inc,1) = buffer(j)    
            j=j+1
            TS_inc=TS_inc+1
            end if
            if (j == buffer_size+1) exit
        end do
        bit=bit+buffer_size*4    
    end do
         
        ! read end of timeserie (remaining)
        read(unit=1, pos=bit) remaining
        j=1
        do remaining_read=1,length_remaining
            if (remaining(j)==flag) then
            j=j+9
            else
            TS_integer(TS_inc,1) = remaining(j)  
            j=j+1
            TS_inc=TS_inc+1
            end if
            if (j == length_remaining+1) exit
        end do
         
       ! write (unit=2,fmt=100) TS_integer(TS_prev:TS_inc-1,1)
        TS_prev=TS_inc
         
        length_TS=TS_inc-1
         
    ! Close Z3D file
    close (1)


    close(2)
    !write *, TS_integer(1:length_TS,1)
    print *, '------------------'
    print *, 'length TS', length_TS
    print *, '------------------'
    

end subroutine read_TS



subroutine read_buffer_TS(fid,buffer_size,pos_bit,flag,TS_buffer,length_TS_buffer,j)
    implicit none
    
    integer,intent(in)                               :: fid,buffer_size,pos_bit,flag
    integer,intent(out)                              :: length_TS_buffer
    integer,intent(inout)                            :: j
    integer,dimension(:,:),allocatable,intent(inout) :: TS_buffer
    integer                                          :: buffer(buffer_size,1)
    integer                                          :: buffer_read,TS_inc

        TS_inc=1
        read(unit=fid, pos=pos_bit) buffer
        do buffer_read=1,buffer_size
            if (buffer(j,1)==flag) then
                j=j+9
            else
                TS_buffer(TS_inc,1) = buffer(j,1)
                j=j+1
                TS_inc=TS_inc+1  
            end if 
            if (j >= buffer_size+1) exit         
        end do
        j=j-buffer_size        
        length_TS_buffer=TS_inc-1
        
end subroutine read_buffer_TS

end module z3d_file