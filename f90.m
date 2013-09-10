
clear all
clc

tic
    
    %filename='/Users/marc/Desktop/new/data/ZEN159.Z3D';
     filename='/Users/marc/Desktop/new/data/BOX07_CH4/ZEN111.Z3D';
    %filename2='/Users/marc/Desktop/new/data/ZEN1562.Z3D';
    filename2='/Users/marc/Desktop/new/data/BOX07_CH5/ZEN065.Z3D';
    buffer_size=8192;
    
    
  %  buffer=zeros(1:buffer_size,1);
    
    % Get Z3D file size (bytes)
    info_file=dir(filename);
    file_size=info_file.bytes;
    
    % If the size of the file cannot be determined display ERROR message.
    if (file_size==-1)
    disp('Error') ;
    end
    
    % Correct if header is not a binary number (ex: user manually changed header) & get total number of bytes
    pos_bit=4225;
    bit_offset=mod(file_size,4);
    pos_bit=pos_bit+bit_offset;
    length_file=file_size;

    nb_buffer=floor(length_file/buffer_size/4);
    length_remaining=length_file/4-nb_buffer*buffer_size;
    
    %TS_integer=zeros(length_file/4,1);
    remaining=zeros(length_remaining,1);
    
    
    
    flag=-1;
    TS_inc=1;
    TS_prev=1;
   
    fid(1) = fopen(filename); 
    fid(2) = fopen(filename);
    fid(3) = fopen(filename);
    
    length_TSbuffer=zeros(1,3);
    
   pos_bit=[1,1,1];
   test_extra=[0,0,0];
   nb_channel=3;
   TS_write_length=0;
   %TS2{:,1}(1:100)=2
   
    % Reformat Timeseries
    
    
    %% WRITE the biggest commum TS length and keep in memory what have not been
    % added for the next round.
    
    for i=1:nb_buffer
             
        % Get Z3D TS buffer
        for row=1:nb_channel  
         disp(i)
         [TS_integer,length_TSbuffer(1,row)] = buf( fid(row),buffer_size,pos_bit(1,row),TS_inc,flag);       
         pos_bit(1,row)=pos_bit(1,row)+buffer_size*4;
         TS2{1,row}(test_extra(row)+1:test_extra(row)+length_TSbuffer(1,row),1)=TS_integer; 
        end
         
        min_length_TSbuffer=min(length_TSbuffer);
        
        % Write to CAC
        for row=1:nb_channel
        TS_write{:,i}(:,row)=TS2{1,row}(1:min_length_TSbuffer);
        TS_write_length=TS_write_length+min_length_TSbuffer;
        end
        
        % ADJUST POSSIBLE TS_length differences 
        for row=1:nb_channel
        prev(row)=test_extra(row);
        test_extra(row)=length_TSbuffer(row)+prev(row)-min_length_TSbuffer;
            if test_extra(row)~=0
                TS2{1,row}(1:test_extra(row),1)=TS2{1,row}(min_length_TSbuffer+1:length_TSbuffer(row)+prev(row));
                TS2{1,row}(test_extra(row)+1:length_TSbuffer(row))=[];
            else     
                TS2{:,row}=[];
            end
        end


    end
    
    %%  % Close Z3D file
    fclose ('all');
    
    toc
    %%
    % read end of timeserie (remaining)
        fseek(fid, pos_bit-1, 'bof');
        remaining=fread(fid, length_remaining,'int32');
        
        j=1;
        for remaining_read=1:length_remaining
            if (remaining(j)==flag)
            j=j+9;
            else
            TS_integer(TS_inc,1) = remaining(j); 
            j=j+1;
            TS_inc=TS_inc+1;
            end
            if (j == length_remaining+1)
                break;
            end
        end
         
       % write (unit=2,fmt=100) TS_integer(TS_prev:TS_inc-1,1)
        TS_prev=TS_inc;
         
        length_TS=TS_inc-1;
         
        TS_integer=TS_integer(1:length_TS,1);
  


    disp(length_TS)

    



