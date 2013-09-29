
MODULE File_Util
!-------------------------------------------------------------------------------
! Holds filename and filename search methods.
! Updated 25/Sept/09 by MB
!-------------------------------------------------------------------------------
! Public s/r
  PUBLIC Index_FName_PC     ! returns index + 1 of last occurrence of character "\" in sString
  PUBLIC Index_FExt      ! returns index - 1 of last occurrence of character "." in sString
  PUBLIC Index_FName_mac ! returns index + 1 of last occurrence of character "/" in sString
  PUBLIC row_sort_by2
  PUBLIC row_sort
! Private s/r
  PRIVATE Partition2d
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  CONTAINS

!*******************************************************************************
!*******************************************************************************

  INTEGER FUNCTION Index_FName_PC( sString )
!-------------------------------------------------------------------------------
! Returns index + 1 of last occurence of character "\" in sString.
! If "\" is not in sString, returns 1.
! Used to get position of file name in a sString which may include
! directory tree structure.
! Modified 25/Sept/09 by SCM
!-------------------------------------------------------------------------------
! Arguments:
  CHARACTER(LEN=*), INTENT(IN) :: sString ! string to search from right to left
!-------------------------------------------------------------------------------
!     Search for leftmost occurence of "\"
      Index_FName = INDEX(sString,'\',BACK=.TRUE.) + 1
    RETURN
  END FUNCTION Index_FName_PC

!*******************************************************************************
!*******************************************************************************

  INTEGER FUNCTION Index_FExt( sString , ext)
!-------------------------------------------------------------------------------
! Returns index of last occurence of character "." in sString - 1.
! If "." is not in sString, returns nonblank length of string.
! Used to get position of file name extension in a sString which may
! include directory tree structure.
! Modified 21/Dec/98 by SCM.
!-------------------------------------------------------------------------------
! Arguments:
  CHARACTER(LEN=*), INTENT(IN) :: sString ! string to search from right to left
  CHARACTER(LEN=*), INTENT(IN) :: ext ! string to search from right to left
!-------------------------------------------------------------------------------
!     Search for leftmost occurence of "."
      Index_FExt = SCAN(sString,ext,BACK=.TRUE.) - 1
!     If "." not present, return nonblank string length.
      IF( Index_FExt<=0 ) Index_FExt = LEN_TRIM(sString)
    RETURN
  END FUNCTION Index_FExt

!*******************************************************************************
!*******************************************************************************

  INTEGER FUNCTION Index_FName_mac( sString )
!-------------------------------------------------------------------------------
! Returns index + 1 of last occurence of character "/" in sString.
! If "/" is not in sString, returns 1.
! Used to get position of file name in a sString which may include
! directory tree structure.
! Modified 11/Sep/2013 by MB
!-------------------------------------------------------------------------------
! Arguments:
  CHARACTER(LEN=*), INTENT(IN) :: sString ! string to search from right to left
!-------------------------------------------------------------------------------
!     Search for leftmost occurence of "/"
      Index_FName_mac = INDEX(sString,'/',BACK=.TRUE.) + 1
    RETURN
  END FUNCTION Index_FName_mac
  
!*******************************************************************************
!*******************************************************************************

  recursive subroutine row_sort(A, c)
!-------------------------------------------------------------------------------------
! Recursive quicksort routine:
! Sorts rows of a 2D array of type integer by any column into ascending numerical order
!-------------------------------------------------------------------------------------
    integer*8, intent(in out), dimension(:,:) :: A  ! 2D array to be sorted
    integer, intent(in)                       :: c  ! Sort catagory (by column)
    integer                                   :: iq

    if (c > size(A,2)) then
        print *, "Error: Array column selection out of range!"
        return
    end if

    if(size(A,1) > 1) then
        call Partition2d(A, c, iq)
        call row_sort(A(:iq-1,:), c)
        call row_sort(A(iq:,:), c)
    endif
end subroutine row_sort

!*******************************************************************************
!*******************************************************************************

subroutine Partition2d(A, c, marker)
!-------------------------------------------------------------------------------------
! This function carries the meat of the sorting
!-------------------------------------------------------------------------------------
    integer*8, dimension(:,:), intent(in out)   :: A
    integer*8, dimension(:,:), allocatable      :: temp
    integer, intent(out)                        :: marker
    integer, intent(in)                         :: c        ! Sort catagory
    integer                                     :: i, j, k
    integer*8                                   :: x        ! pivot point

    allocate(temp(1,size(A, 2)))

    x = A(1,c)
    i = 0
    j = size(A,1) + 1

    do
        j = j-1
        do
            if (A(j,c) <= x) exit
            j = j-1
        end do
        i = i+1
        do
            if (A(i,c) >= x) exit
            i = i+1
        end do
        if (i < j) then
            ! exchange A(i,:) and A(j,:)
            do k=1, size(A,2)
                temp(1,k) = A(i,k)
                A(i,k) = A(j,k)
                A(j,k) = temp(1,k)
            end do
        else if (i == j) then
            marker = i+1
            return
        else
            marker = i
            return
        endif
    end do
end subroutine Partition2d

!*******************************************************************************
!*******************************************************************************

subroutine row_sort_by2(A, c1, c2)
!-------------------------------------------------------------------------------------
! This function sorts rows of a 2d array of type integer by two columns (categories)
! c1 is the first column to be sorted and then each section of rows equal are sorted
! by c2 column.
! Created on 19/12/2012 by Kevin
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
    integer*8, intent(in out), dimension(:,:) :: A      ! 2D array to be sorted
    integer, intent(in)                       :: c1, c2 ! Sort catagories (by columns)
    integer                                   :: x, k, i

    if (c1 > size(A,2) .or. c2 >size(A,2)) then
        print *, "Error: Array column selections out of range!"
        return
    end if

    call row_sort(A, c1)

    x = 1
    k = 1
    do i=1, size(A,1)
        if (A(i+1,c1) /= A(i,c1)) then
            call row_sort(A(x:k,:), c2)
            x = k+1
        end if
            k = k+1
    end do
end subroutine row_sort_by2

END MODULE File_Util

