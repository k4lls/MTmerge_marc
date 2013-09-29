
MODULE List_Util
!-------------------------------------------------------------------------------
! Methods for building and searching ordered & unordered I4, R4 & SN lists.
! Updated 27/July/09 by SCM.
!-------------------------------------------------------------------------------
! Public methods
  PUBLIC List_Append  ! search unordered list for match, append if no match found, return index of match
  PUBLIC List_Insert  ! search ordered list for match, insert if no match found
  PUBLIC List_Search  ! search ordered list for match to value
  PUBLIC List_Nearest ! search ordered list for nearest match to value
  PUBLIC List_Bracket ! find list elements that bracket value
! Private methods
  PRIVATE List_Append_I4  ! search unordered list for match, append if no match found, return list index
  PRIVATE List_Insert_I4  ! search ordered list for match, insert if no match found
  PRIVATE List_Search_I4  ! search ordered list for match to iValue
  PRIVATE List_Nearest_I4 ! search ordered list for nearest match to iValue
  PRIVATE List_Bracket_I4 ! find list elements that bracket iValue
  PRIVATE List_Append_R4  ! search unordered list for match, append if no match found, return list index
  PRIVATE List_Insert_R4  ! search ordered list for match, insert if no match found
  PRIVATE List_Search_R4  ! search ordered list for match to fValue
  PRIVATE List_Nearest_R4 ! search ordered list for nearest match to fValue
  PRIVATE List_Bracket_R4 ! find list elements that bracket fValue
  PRIVATE List_Append_SN  ! search unordered list for match, append if no match found, return list index
  PRIVATE List_Insert_SN  ! search ordered list for match, insert if no match found
  PRIVATE List_Search_SN  ! search ordered list for match to sValue
  PRIVATE List_Nearest_SN  ! search ordered list for nearest match to sValue
  PRIVATE List_Bracket_SN ! find list elements that bracket sValue
  PRIVATE iStr_Fuzzy_Match ! Damerau-Levenshtein case-insensitive distance between str1 & str2

! Generic interface blocks
  INTERFACE List_Append
    MODULE PROCEDURE List_Append_I4, List_Append_R4, List_Append_SN
  END INTERFACE

  INTERFACE List_Insert
    MODULE PROCEDURE List_Insert_I4, List_Insert_R4, List_Insert_SN
  END INTERFACE

  INTERFACE List_Search
    MODULE PROCEDURE List_Search_I4, List_Search_R4, List_Search_SN
  END INTERFACE

  INTERFACE List_Nearest
    MODULE PROCEDURE List_Nearest_I4, List_Nearest_R4, List_Nearest_SN
  END INTERFACE

  INTERFACE List_Bracket
    MODULE PROCEDURE List_Bracket_I4, List_Bracket_R4, List_Bracket_SN
  END INTERFACE
!-------------------------------------------------------------------------------
  CONTAINS

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Append_I4( iValue, NList, iList, iMatch, iFlag )
!-------------------------------------------------------------------------------
! Search unordered iList(1:NList) for match to iValue.
! If match found, return iMatch s.t. iList(iMatch)==iValue and return iFlag=0
! If no match, extend list, append iValue & return iFlag=1
! If no match & no room on list, replace last point with iValue & return iFlag=2
! Modified 9/May/08 by SCM.
!-------------------------------------------------------------------------------
! Arguments
  INTEGER, INTENT(IN)    :: iValue   ! new value
  INTEGER, INTENT(INOUT) :: NList    ! list length
  INTEGER, INTENT(INOUT) :: iList(:) ! unordered list
  INTEGER, INTENT(OUT)   :: iMatch   ! index for matching iList(:) value
  INTEGER, INTENT(OUT)   :: iFlag    ! 0=found match, 1=extended list, 2=no room to extend list
! Local variables
  INTEGER J, MList
!-------------------------------------------------------------------------------
      iFlag = 0
!     Is this an empy list?
      IF( NList<1 ) THEN
        NList = 1
        iList(1) = iValue
        iMatch = 1
        iFlag = 1
        RETURN
      END IF

!     Search unordered iList(1:NList) for match to iValue
      DO J = 1,NList
        IF( iList(J)==iValue ) THEN
!         Have a match
          iMatch = J
          iFlag = 0
          RETURN
        END IF
      END DO

!     No match, can list be extended?
      MList = SIZE(iList)
      IF( NList<MList ) THEN
        NList = NList + 1
        iFlag = 1
      ELSE
        iFlag = 2 ! last point dropped
      END IF

!     Append iValue
      iMatch = NList
      iList(NList) = iValue
    RETURN
  END SUBROUTINE List_Append_I4

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Insert_I4( iValue, NList, iList, iFlag )
!-------------------------------------------------------------------------------
! Search ordered iList(1:NList) for match to iValue.
! If match found, return iFlag=0
! If no match, extend list, insert iValue & return iFlag=1
! If no match & no room on list, drop last point, insert iValue & return iFlag=2
! Modified 27/July/09 by SCM.
!-------------------------------------------------------------------------------
! Arguments
  INTEGER, INTENT(IN)    :: iValue   ! new value
  INTEGER, INTENT(INOUT) :: NList    ! list length
  INTEGER, INTENT(INOUT) :: iList(:) ! low-to-high ordered list
  INTEGER, INTENT(OUT)   :: iFlag    ! 0=found match, 1=extended list, 2=no room to extend list
! INTEGERariables
  INTEGER iDV1, iDV2, J, J2, J3, MList
  INTEGER, SAVE :: JLO=0
!-------------------------------------------------------------------------------
      iFlag = 0
!     Is this an empy list?
      IF( NList<1 ) THEN
        JLO = 1
        NList = 1
        iList(1) = iValue
        iFlag = 1
        RETURN
      END IF

!     Search ordered iList(1:NList) for nearest iValue
      CALL List_Bracket_I4( iValue, NList, iList, JLO, iDV1, iDV2 )
!     Is there a match to the nearest element?
      IF( iDV1==0 .OR. iDV2==0 ) THEN
!       Have a match
        iFlag = 0
        RETURN
      END IF

!     No match, can list be extended?
      MList = SIZE(iList)
      IF( NList<MList ) THEN
        NList = NList + 1
        iFlag = 1
      ELSE
        iFlag = 2 ! last point dropped
      END IF

!     Insert new value at J2 = JLO + 1
      J2 = MIN(JLO+1,NList)
      J3 = J2 + 1
      DO J = NList,J3,-1
        iList(J) = iList(J-1)
      END DO ! next J
      iList(J2) = iValue
    RETURN
  END SUBROUTINE List_Insert_I4

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Search_I4( iValue, NList, iList, iMatch )
!-------------------------------------------------------------------------------
! Search ordered iList(1:NList) for match to iValue.
! If match found, return index to match in iMatch, no precise match =>  iMatch=0
! Modified 27/July/09 by SCM.
!-------------------------------------------------------------------------------
! Arguments
  INTEGER, INTENT(IN)  :: iValue       ! target tuple value
  INTEGER, INTENT(IN)  :: NList        ! list length
  INTEGER, INTENT(IN)  :: iList(NList) ! low-to-high ordered tuple list
  INTEGER, INTENT(OUT) :: iMatch       ! list index of match, if no match, set to 0
! Local variables
  INTEGER iDV1, iDV2
  INTEGER, SAVE :: JLO=0
!-------------------------------------------------------------------------------
!     Initialize
      iMatch = 0
!     Is this an empy list?
      IF( NList<1 ) THEN
        JLO = 1
        RETURN
      END IF

!     Search ordered iList(1:NList) for iValue
      CALL List_Bracket_I4( iValue, NList, iList, JLO, iDV1, iDV2 )
!     Is there a match to JLO interval endpoint?
      IF( iDV1==0 ) THEN
        iMatch = MAX(1,JLO)
      ELSE IF( iDV2==0 ) THEN
        iMatch = MIN(JLO+1,NList)
      ELSE
        iMatch = 0 ! no precise match
      END IF
    RETURN
  END SUBROUTINE List_Search_I4

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Nearest_I4( iValue, NList, iList, iMatch )
!-------------------------------------------------------------------------------
! Search ordered iList(1:NList) for nearest match to iValue.
! Return index of nearest list element to match, if error, return iMatch=0
! Modified 27/July/09 by SCM.
!-------------------------------------------------------------------------------
! Arguments
  INTEGER, INTENT(IN)  :: iValue       ! target tuple value
  INTEGER, INTENT(IN)  :: NList        ! list length
  INTEGER, INTENT(IN)  :: iList(NList) ! low-to-high ordered tuple list
  INTEGER, INTENT(OUT) :: iMatch       ! list index of match, if no match, set to 0
! Local variables
  INTEGER iDV1, iDV2
  INTEGER, SAVE :: JLO=0
!-------------------------------------------------------------------------------
!     Initialize
      iMatch = 0
!     Is this an empy list?
      IF( NList<1 ) THEN
        JLO = 1
        RETURN
      END IF

!     Search ordered iList(1:NList) for iValue
      CALL List_Bracket_I4( iValue, NList, iList, JLO, iDV1, iDV2 )
!     Which endpoint is nearest?
      IF( ABS(iDV1)<=ABS(iDV2) ) THEN
        iMatch = MAX(1,JLO)
      ELSE
        iMatch = MIN(JLO+1,NList)
      END IF
    RETURN
  END SUBROUTINE List_Nearest_I4

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Bracket_I4( iValue, NList, iList, JLO, iDV1, iDV2 )
!-------------------------------------------------------------------------------
! Given a low-to-high ordered array iList(1:NList) and target value iValue,
! returns JLO such that iList(JLO) <= iValue < iList(JLO+1).
! iList(1:NList) must be ordered low to high.
! JLO=0 or JLO=NList is returned to indicate that iValue is out of range.
! Input JLO is taken as the initial guess for output JLO.
! Input JLO=0 for binary search with no preliminary hunting.
! Adapted from Press, et al, 1992, Numerical Recipes 2nd Ed, p112, Hunt_Bracket
! Modified 27/July/09 by SCM
!-------------------------------------------------------------------------------
! Arguments:
  INTEGER, INTENT(IN)    :: iValue       ! target value to match or bracket
  INTEGER, INTENT(IN)    :: NList        ! # elements in ordered array
  INTEGER, INTENT(IN)    :: iList(NList) ! low-to-high ordered tuple list
  INTEGER, INTENT(INOUT) :: JLO          ! lower index of interval bracketing iValue
  INTEGER, INTENT(OUT)   :: iDV1         ! iValue - iList(JLO)
  INTEGER, INTENT(OUT)   :: iDV2         ! iList(JHi) - iValue
! Local variables:
  INTEGER JINC, JHI, JM
!-------------------------------------------------------------------------------
      IF( NList<1 ) THEN
        JLO = 0
        iDV1 = -1
        iDV2 =  0
        RETURN
      ELSE IF( NList==1 ) THEN
        IF( iValue<iList(1) ) THEN
          JLO = 0
          iDV2 = iValue - iList(1)  ! iDV2<0
          iDV1 = MIN(2*iDV2,-1)
        ELSE
          JLO = 1
          iDV1 = iValue - iList(1)  ! iDV1>=0
          iDV2 = MAX(1,2*iDV1)
        END IF
        RETURN
      END IF

!     Is input guess useful?
      IF ( JLO<=0 .OR. JLO>NList ) THEN ! input guess not useful, go directly to bisection
        JLO = 0
        JHI = NList + 1
        GO TO 120
      END IF

!.....Hunt from JLO for bracketing interval
      JINC = 1 ! initialize hunting increment
      IF ( iValue>=iList(JLO) ) THEN ! iValue to right of iList(JLO) => hunt up
 100    CONTINUE
        JHI = JLO + JINC
        IF ( JHI>NList ) THEN ! off end of table => done hunting
          JHI = NList + 1
        ELSE IF( iValue>=iList(JHI) ) THEN ! still hunting to the right
          JLO = JHI          ! move lower boundary up
          JINC = JINC + JINC ! double the increment
          GO TO 100          ! try again
        END IF
      ELSE ! iValue to right of iList(JLO), hunt down
        JHI = JLO
 110    CONTINUE
        JLO = JHI - JINC
        IF ( JLO<1 ) THEN ! off end of table => done hunting
          JLO = 0
        ELSE IF ( iValue<iList(JLO) ) THEN ! still hunting to the left
          JHI = JLO          ! move upper boundary down
          JINC = JINC + JINC ! double the increment
          GO TO 110          ! try again
        END IF
      END IF

!.....Bisection search
 120  CONTINUE
      DO WHILE( JHI-JLO>1 )
        JM = (JHI+JLO)/2 ! middle of search interval
        IF ( iValue>iList(JM) ) THEN
          JLO = JM ! iValue is to right of JM
        ELSE
          JHI = JM ! iValue is to left of JM
        END IF
      END DO

!.....Get distance between iValue, iList(JLO) & iList(JHI)
      IF( JLO<1 ) THEN ! iValue < iList(1)
        iDV2 = iValue - iList(1)
        iDV1 = MIN(2*iDV2,-1)
      ELSE IF( JLO>=NList ) THEN ! iValue > iValue(NList)
        iDV1 = iValue - iList(NList)
        iDV2 = MAX(1,2*iDV1)
      ELSE ! iList(JLO)<= iValue <= iList(JLO+1)
        iDV1 = iValue - iList(JLO)
        iDV2 = iList(JLO+1) - iValue
      END IF
    RETURN
  END SUBROUTINE List_Bracket_I4

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Append_R4( fValue, NList, fList, iMatch, iFlag )
!-------------------------------------------------------------------------------
! Search unordered fList(1:NList) for match to fValue.
! If match found, return iMatch s.t. fList(iMatch)==fValue and return iFlag=0
! If no match, extend list, append fValue & return iFlag=1
! If no match & no room on list, replace last point with fValue & return iFlag=2
! Modified 9/May/08 by SCM.
!-------------------------------------------------------------------------------
! Arguments
  REAL,    INTENT(IN)    :: fValue   ! new value
  INTEGER, INTENT(INOUT) :: NList    ! list length
  REAL,    INTENT(INOUT) :: fList(:) ! unordered list
  INTEGER, INTENT(OUT)   :: iMatch   ! index for matching fList(:) value
  INTEGER, INTENT(OUT)   :: iFlag    ! 0=found match, 1=extended list, 2=no room to extend list
! Local variables
  INTEGER J, MList
!-------------------------------------------------------------------------------
      iFlag = 0
!     Is this an empy list?
      IF( NList<1 ) THEN
        NList = 1
        fList(1) = fValue
        iMatch = 1
        iFlag = 1
        RETURN
      END IF

!     Search unordered fList(1:NList) for match to fValue
      DO J = 1,NList
        IF( ABS(fList(J)-fValue)<100.0*SPACING(fValue) ) THEN
!         Nearly exact match
          iMatch = J
          iFlag = 0
          RETURN
        END IF
      END DO

!     No match, can list be extended?
      MList = SIZE(fList)
      IF( NList<MList ) THEN
        NList = NList + 1
        iFlag = 1
      ELSE
        iFlag = 2 ! last point dropped
      END IF

!     Append fValue
      iMatch = NList
      fList(NList) = fValue
    RETURN
  END SUBROUTINE List_Append_R4

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Insert_R4( fValue, NList, fList, iFlag )
!-------------------------------------------------------------------------------
! Search ordered fList(1:NList) for match to fValue.
! If match found, return iFlag=0
! If no match, extend list, insert fValue & return iFlag=1
! If no match & no room on list, drop last point, insert fValue & return iFlag=2
! Modified 9/May/08 by SCM.
!-------------------------------------------------------------------------------
! Arguments
  REAL,    INTENT(IN)    :: fValue   ! new value
  INTEGER, INTENT(INOUT) :: NList    ! list length
  REAL,    INTENT(INOUT) :: fList(:) ! low-to-high ordered list
  INTEGER, INTENT(OUT)   :: iFlag    ! 0=found match, 1=extended list, 2=no room to extend list
! Local variables
  INTEGER J, J2, J3, MList
  INTEGER, SAVE :: JLO=0
  REAL    fDV1, fDV2
!-------------------------------------------------------------------------------
      iFlag = 0
!     Is this an empy list?
      IF( NList<1 ) THEN
        JLO = 1
        NList = 1
        fList(1) = fValue
        iFlag = 1
        RETURN
      END IF

!     Search ordered fList(1:NList) for nearest fValue
      CALL List_Bracket_R4( fValue, NList, fList, JLO, fDV1, fDV2 )
!     Is there a match to the nearest element?
      IF( ABS(fDV1)<100.0*SPACING(fValue) .OR. ABS(fDV2)<100.0*SPACING(fValue) ) THEN
!       Nearly exact match
        iFlag = 0
        RETURN
      END IF

!     No match, can list be extended?
      MList = SIZE(fList)
      IF( NList<MList ) THEN
        NList = NList + 1
        iFlag = 1
      ELSE
        iFlag = 2 ! last point dropped
      END IF

!     Insert new value at J2 = JLO + 1
      J2 = MIN(JLO+1,NList)
      J3 = J2 + 1
      DO J = NList,J3,-1
        fList(J) = fList(J-1)
      END DO ! next J
      fList(J2) = fValue
    RETURN
  END SUBROUTINE List_Insert_R4

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Search_R4( fValue, NList, fList, iMatch )
!-------------------------------------------------------------------------------
! Search ordered fList(1:NList) for match to fValue.
! If match found, return index to match in iMatch, no precise match =>  iMatch=0
! Modified 27/July/09 by SCM.
!-------------------------------------------------------------------------------
! Arguments
  REAL,    INTENT(IN)  :: fValue       ! target tuple value
  INTEGER, INTENT(IN)  :: NList        ! list length
  REAL,    INTENT(IN)  :: fList(NList) ! low-to-high ordered tuple list
  INTEGER, INTENT(OUT) :: iMatch       ! list index of match, if no match, set to 0
! Local variables
  INTEGER, SAVE :: JLO=0
  REAL    fDV1, fDV2
!-------------------------------------------------------------------------------
!     Initialize
      iMatch = 0
!     Is this an empy list?
      IF( NList<1 ) THEN
        JLO = 1
        RETURN
      END IF

!     Search ordered fList(1:NList) for fValue
      CALL List_Bracket_R4( fValue, NList, fList, JLO, fDV1, fDV2 )
!     Is there a match to JLO interval endpoint?
      IF( ABS(fDV1)<100.0*SPACING(fValue) ) THEN
        iMatch = MAX(1,JLO)
      ELSE IF( ABS(fDV2)<100.0*SPACING(fValue) ) THEN
        iMatch = MIN(JLO+1,NList)
      ELSE
        iMatch = 0 ! no precise match
      END IF
    RETURN
  END SUBROUTINE List_Search_R4

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Nearest_R4( fValue, NList, fList, iMatch )
!-------------------------------------------------------------------------------
! Search ordered fList(1:NList) for nearest match to fValue.
! Return index of nearest list element to match, if error, return iMatch=0
! Modified 27/July/09 by SCM.
!-------------------------------------------------------------------------------
! Arguments
  REAL,    INTENT(IN)  :: fValue       ! target tuple value
  INTEGER, INTENT(IN)  :: NList        ! list length
  REAL,    INTENT(IN)  :: fList(NList) ! low-to-high ordered tuple list
  INTEGER, INTENT(OUT) :: iMatch       ! list index of match, always between 1 & NList
! Local variables
  INTEGER, SAVE :: JLO=0
  REAL    fDV1, fDV2
!-------------------------------------------------------------------------------
!     Initialize
      iMatch = 0
!     Is this an empy list?
      IF( NList<1 ) THEN
        JLO = 1
        RETURN
      END IF

!     Search ordered fList(1:NList) for fValue
      CALL List_Bracket_R4( fValue, NList, fList, JLO, fDV1, fDV2 )
!     Is there a match to JLO interval endpoint?
      IF( ABS(fDV1)<=ABS(fDV2) ) THEN
        iMatch = MAX(1,JLO)
      ELSE
        iMatch = MIN(JLO+1,NList)
      END IF
    RETURN
  END SUBROUTINE List_Nearest_R4

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Bracket_R4( fValue, NList, fList, JLO, fDV1, fDV2 )
!-------------------------------------------------------------------------------
! Given a low-to-high ordered array fList(1:NList) and target value fValue,
! returns JLO such that fList(JLO) <= fValue < fList(JLO+1).
! fList(1:NList) must be ordered low to high.
! JLO=0 or JLO=NList is returned to indicate that fValue is out of range.
! Input JLO is taken as the initial guess for output JLO.
! Input JLO=0 for binary search with no preliminary hunting.
! Adapted from Press, et al, 1992, Numerical Recipes 2nd Ed, p112, Hunt_Bracket
! Modified 27/July/09 by SCM
!-------------------------------------------------------------------------------
! Arguments:
  REAL,    INTENT(IN)    :: fValue       ! target value to match or bracket
  INTEGER, INTENT(IN)    :: NList        ! # elements in ordered array
  REAL,    INTENT(IN)    :: fList(NList) ! low-to-high ordered tuple list
  INTEGER, INTENT(INOUT) :: JLO          ! lower index of interval bracketing fValue
  REAL,    INTENT(OUT)   :: fDV1         ! fValue - fList(JLO)
  REAL,    INTENT(OUT)   :: fDV2         ! fList(JHi) - fValue
! Local variables:
  INTEGER JINC, JHI, JM
!-------------------------------------------------------------------------------
      IF( NList<1 ) THEN
        JLO = 0
        fDV1 = -1.0
        fDV2 =  0.0
        RETURN
      ELSE IF( NList==1 ) THEN
        IF( fValue<fList(1) ) THEN
          JLO = 0
          fDV2 = fValue - fList(1) ! fDV2<0
          fDV1 = MIN(2.0*fDV2,-1.0)
        ELSE
          JLO = 1
          fDV1 = fValue - fList(1) ! fDV1>=0
          fDV2 = MAX(1.0,2.0*fDV1)
        END IF
        RETURN
      END IF

!     Is input guess useful?
      IF ( JLO<=0 .OR. JLO>NList ) THEN ! input guess not useful, go directly to bisection
        JLO = 0
        JHI = NList + 1
        GO TO 120
      END IF

!.....Hunt from JLO for bracketing interval
      JINC = 1 ! initialize hunting increment
      IF ( fValue>=fList(JLO) ) THEN ! fValue to right of fList(JLO) => hunt up
 100    CONTINUE
        JHI = JLO + JINC
        IF ( JHI>NList ) THEN ! off end of table => done hunting
          JHI = NList + 1
        ELSE IF( fValue>=fList(JHI) ) THEN ! still hunting to the right
          JLO = JHI          ! move lower boundary up
          JINC = JINC + JINC ! double the increment
          GO TO 100          ! try again
        END IF
      ELSE ! fValue to right of fList(JLO), hunt down
        JHI = JLO
 110    CONTINUE
        JLO = JHI - JINC
        IF ( JLO<1 ) THEN ! off end of table => done hunting
          JLO = 0
        ELSE IF ( fValue<fList(JLO) ) THEN ! still hunting to the left
          JHI = JLO          ! move upper boundary down
          JINC = JINC + JINC ! double the increment
          GO TO 110          ! try again
        END IF
      END IF

!.....Bisection search
 120  CONTINUE
      DO WHILE( JHI-JLO>1 )
        JM = (JHI+JLO)/2 ! middle of search interval
        IF ( fValue>fList(JM) ) THEN
          JLO = JM ! fValue is to right of JM
        ELSE
          JHI = JM ! fValue is to left of JM
        END IF
      END DO

!.....Get distance between fValue, fList(JLO) & fList(JHI)
      IF( JLO<1 ) THEN ! fValue < fList(1)
        fDV2 = fValue - fList(1)
        fDV1 = MIN(2.0*fDV2,-1.0)
      ELSE IF( JLO>=NList ) THEN ! fValue > fValue(NList)
        fDV1 = fValue - fList(NList)
        fDV2 = MAX(1.0,2.0*fDV1)
      ELSE ! fList(JLO)<= fValue <= fList(JLO+1)
        fDV1 = fValue - fList(JLO)
        fDV2 = fList(JLO+1) - fValue
      END IF
    RETURN
  END SUBROUTINE List_Bracket_R4

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Append_SN( sValue, NList, sList, iMatch, iFlag )
!-------------------------------------------------------------------------------
! Search unordered sList(1:NList) for match to sValue.
! If match found, return iMatch s.t. sList(iMatch)==sValue and return iFlag=0
! If no match, extend list, append sValue & return iFlag=1
! If no match & no room on list, replace last point with sValue & return iFlag=2
! Modified 9/May/08 by SCM.
!-------------------------------------------------------------------------------
! Arguments
  CHARACTER(LEN=*), INTENT(IN)    :: sValue   ! new value
  INTEGER,          INTENT(INOUT) :: NList    ! list length
  CHARACTER(LEN=*), INTENT(INOUT) :: sList(:) ! unordered list
  INTEGER,          INTENT(OUT)   :: iMatch   ! index for matching sList(:) value
  INTEGER,          INTENT(OUT)   :: iFlag    ! 0=found match, 1=extended list, 2=no room to extend list
! Local variables
  INTEGER J, MList
!-------------------------------------------------------------------------------
      iMatch = 0
      iFlag = 0
!     Is this an empy list?
      IF( NList<1 ) THEN
        NList = 1
        sList(1) = sValue
        iMatch = 1
        iFlag = 1
        RETURN
      END IF

!     Search unordered sList(1:NList) for match to sValue
      DO J = 1,NList
        IF( sList(J)==sValue ) THEN
!         Have a match
          iMatch = J
          iFlag = 0
          RETURN
        END IF
      END DO

!     No match, can list be extended?
      MList = SIZE(sList)
      IF( NList<MList ) THEN
        NList = NList + 1
        iFlag = 1
      ELSE
        iFlag = 2 ! last point dropped
      END IF

!     Append sValue
      iMatch = NList
      sList(NList) = sValue
    RETURN
  END SUBROUTINE List_Append_SN

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Insert_SN( sValue, NList, sList, iFlag )
!-------------------------------------------------------------------------------
! Search ordered sList(1:NList) for match to sValue.
! If match found, return iFlag=0
! If no match, extend list, insert sValue & return iFlag=1
! If no match & no room on list, drop last point, insert sValue & return iFlag=2
! Modified 9/May/08 by SCM.
!-------------------------------------------------------------------------------
! Arguments
  CHARACTER(LEN=*), INTENT(IN)    :: sValue   ! new value
  INTEGER,          INTENT(INOUT) :: NList    ! list length
  CHARACTER(LEN=*), INTENT(INOUT) :: sList(:) ! low-to-high ordered list
  INTEGER,          INTENT(OUT)   :: iFlag    ! 0=found match, 1=extended list, 2=no room to extend list
! INTEGERariables
  INTEGER iDV1, iDV2, J, J2, J3, MList
  INTEGER, SAVE :: JLO=0
!-------------------------------------------------------------------------------
      iFlag = 0
!     Is this an empy list?
      IF( NList<1 ) THEN
        JLO = 1
        NList = 1
        sList(1) = sValue
        iFlag = 1
        RETURN
      END IF

!     Search ordered sList(1:NList) for nearest sValue
      CALL List_Bracket_SN( sValue, NList, sList, JLO, iDV1, iDV2 )
!     Is there a match to the nearest element?
      IF( iDV1==0 .OR. iDV2==0 ) THEN
!       Have a match
        iFlag = 0
        RETURN
      END IF

!     No match, can list be extended?
      MList = SIZE(sList)
      IF( NList<MList ) THEN
        NList = NList + 1
        iFlag = 1
      ELSE
        iFlag = 2 ! last point dropped
      END IF

!     Insert new value at J2 = JLO + 1
      J2 = MIN(JLO+1,NList)
      J3 = J2 + 1
      DO J = NList,J3,-1
        sList(J) = sList(J-1)
      END DO ! next J
      sList(J2) = sValue
    RETURN
  END SUBROUTINE List_Insert_SN

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Search_SN( sValue, NList, sList, iMatch )
!-------------------------------------------------------------------------------
! Search ordered sList(1:NList) for match to sValue.
! If match found, return index to match in iMatch, no precise match =>  iMatch=0
! Modified 9/May/08 by SCM.
!-------------------------------------------------------------------------------
! Arguments
  CHARACTER(LEN=*), INTENT(IN)  :: sValue       ! target tuple value
  INTEGER,          INTENT(IN)  :: NList        ! list length
  CHARACTER(LEN=*), INTENT(IN)  :: sList(NList) ! low-to-high ordered tuple list
  INTEGER,          INTENT(OUT) :: iMatch       ! list index of match, if no match, set to 0
! Local variables
  INTEGER iDV1, iDV2
  INTEGER, SAVE :: JLO=0
!-------------------------------------------------------------------------------
!     Initialize
      iMatch = 0
!     Is this an empy list?
      IF( NList<1 ) THEN
        JLO = 1
        RETURN
      END IF

!     Search ordered sList(1:NList) for sValue
      CALL List_Bracket_SN( sValue, NList, sList, JLO, iDV1, iDV2 )
!     Is there a match to JLO interval endpoint?
      IF( iDV1==0 ) THEN
        iMatch = MAX(1,JLO)
      ELSE IF( iDV2==0 ) THEN
        iMatch = MIN(JLO+1,NList)
      ELSE
        iMatch = 0 ! no precise match
      END IF
    RETURN
  END SUBROUTINE List_Search_SN

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Nearest_SN( sValue, NList, sList, iMatch )
!-------------------------------------------------------------------------------
! Search ordered sList(1:NList) for nearest match to sValue.
! If match found, return index to match in iMatch, no precise match =>  iMatch=0
! Modified 27/July/09 by SCM.
!-------------------------------------------------------------------------------
! Arguments
  CHARACTER(LEN=*), INTENT(IN)  :: sValue       ! target tuple value
  INTEGER,          INTENT(IN)  :: NList        ! list length
  CHARACTER(LEN=*), INTENT(IN)  :: sList(NList) ! low-to-high ordered tuple list
  INTEGER,          INTENT(OUT) :: iMatch       ! list index of match, if no match, set to 0
! Local variables
  INTEGER I, iDV, iDVmin
!-------------------------------------------------------------------------------
!     Initialize
      iMatch = 0
!     Is this an empy list?
      IF( NList<1 ) THEN
        RETURN
      END IF

!     Search sList(1:NList) for nearest sValue
!     Does not use list order because it's decoupled from Damerau-Levenshtein distance
      iDVmin = iStr_Fuzzy_Match( sValue, sList(1) )
      iMatch = 1
      DO I = 2,NList
        iDV = iStr_Fuzzy_Match( sValue, sList(I) )
        IF( iDVmin>iDV ) THEN
          iDVmin = iDV
          iMatch = I
        END IF
      END DO ! next I

    RETURN
  END SUBROUTINE List_Nearest_SN

!*******************************************************************************
!*******************************************************************************

  SUBROUTINE List_Bracket_SN( sValue, NList, sList, JLO, iDV1, iDV2 )
!-------------------------------------------------------------------------------
! Given a low-to-high ordered array sList(1:NList) and target value sValue,
! returns JLO such that sList(JLO) <= sValue < sList(JLO+1).
! sList(1:NList) must be ordered low to high.
! JLO=0 or JLO=NList is returned to indicate that sValue is out of range.
! Input JLO is taken as the initial guess for output JLO.
! Input JLO=0 for binary search with no preliminary hunting.
! Adapted from Press, et al, 1992, Numerical Recipes 2nd Ed, p112, Hunt_Bracket
! Modified 27/July/09 by SCM
!-------------------------------------------------------------------------------
! Arguments:
  CHARACTER(LEN=*), INTENT(IN)    :: sValue       ! target value to match or bracket
  INTEGER,          INTENT(IN)    :: NList        ! # elements in ordered array
  CHARACTER(LEN=*), INTENT(IN)    :: sList(NList) ! low-to-high ordered tuple list
  INTEGER,          INTENT(INOUT) :: JLO          ! lower index of interval bracketing sValue
  INTEGER,          INTENT(OUT)   :: iDV1         ! |sValue - sList(JLO)|
  INTEGER,          INTENT(OUT)   :: iDV2         ! |sList(JHi) - sValue|
! Local variables:
  INTEGER JINC, JHI, JM
!-------------------------------------------------------------------------------
      IF( NList<1 ) THEN
        JLO = 0
        iDV1 = -1
        iDV2 =  0
        RETURN
      ELSE IF( NList==1 ) THEN
        IF( LLT(sValue,sList(1)) ) THEN
          JLO = 0
          iDV2 = -iStr_Fuzzy_Match( sValue, sList(1) ) ! iDV2<0
          iDV1 = 2*iDV2
        ELSE
          JLO = 1
          IF( sValue==sList(1) ) THEN
            iDV1 = 0
          ELSE
            iDV1 = iStr_Fuzzy_Match( sValue, sList(NList) ) ! iDV1>=0
          END IF
          iDV2 = MAX(1,2*iDV1)
        END IF
        RETURN
      END IF

!     Is input guess useful?
      IF ( JLO<=0 .OR. JLO>NList ) THEN ! input guess not useful, go directly to bisection
        JLO = 0
        JHI = NList + 1
        GO TO 120
      END IF

!.....Hunt from JLO for bracketing interval
      JINC = 1 ! initialize hunting increment
      IF ( LGE(sValue,sList(JLO)) ) THEN ! sValue to right of sList(JLO) => hunt up
 100    CONTINUE
        JHI = JLO + JINC
        IF ( JHI>NList ) THEN ! off end of table => done hunting
          JHI = NList + 1
        ELSE IF( LGE(sValue,sList(JHI)) ) THEN ! still hunting to the right
          JLO = JHI          ! move lower boundary up
          JINC = JINC + JINC ! double the increment
          GO TO 100          ! try again
        END IF
      ELSE ! sValue to right of sList(JLO), hunt down
        JHI = JLO
 110    CONTINUE
        JLO = JHI - JINC
        IF ( JLO<1 ) THEN ! off end of table => done hunting
          JLO = 0
        ELSE IF ( LLT(sValue,sList(JLO)) ) THEN ! still hunting to the left
          JHI = JLO          ! move upper boundary down
          JINC = JINC + JINC ! double the increment
          GO TO 110          ! try again
        END IF
      END IF

!.....Bisection search
 120  CONTINUE
      DO WHILE( JHI-JLO>1 )
        JM = (JHI+JLO)/2 ! middle of search interval
        IF ( LGT(sValue,sList(JM)) ) THEN
          JLO = JM ! sValue is to right of JM
        ELSE
          JHI = JM ! sValue is to left of JM
        END IF
      END DO

!.....Get distance between sValue, sList(JLO) & sList(JHI)
      IF( JLO<1 ) THEN ! sValue < sList(1)
        IF( sValue==sList(1) ) THEN
          iDV2 = 0
        ELSE
          iDV2 = -iStr_Fuzzy_Match( sValue, sList(1) )
        END IF
        iDV1 = MIN(2*iDV2,-1)
      ELSE IF( JLO>=NList ) THEN ! sValue > sValue(NList)
        IF( sValue==sList(NList) ) THEN
          iDV1 = 0
        ELSE
          iDV1 = iStr_Fuzzy_Match( sValue, sList(NList) )
        END IF
        iDV2 = MAX(1,2*iDV1)
      ELSE ! sList(JLO)<= sValue <= sList(JLO+1)
        IF( sValue==sList(JLO) ) THEN
          iDV1 = 0
        ELSE
          iDV1 = iStr_Fuzzy_Match( sValue, sList(JLO) )
        END IF
        IF( sValue==sList(JLO+1) ) THEN
          iDV2 = 0
        ELSE
          iDV2 = iStr_Fuzzy_Match( sValue, sList(JLO+1) )
        END IF
      END IF
    RETURN
  END SUBROUTINE List_Bracket_SN

!*******************************************************************************
!*******************************************************************************

  INTEGER FUNCTION iStr_Fuzzy_Match( str1, str2 )
!-------------------------------------------------------------------------------
! Returns Damerau-Levenshtein case-insensitive distance between str1 & str2.
! Damerau-Levenshtein distance = # edits required to make strings identical,
! returns 0 if strings are identical.
! Adapted from Wikipedia search on appoximate string match, 12/March/06
! Modified 15/March/06 by SCM.
!-------------------------------------------------------------------------------
! Arguments:
  CHARACTER(LEN=*), INTENT(IN) :: str1, str2 ! strings to be compared.
! Local variables:
  INTEGER i, icost, j, n1, n2
  INTEGER istr1(len_trim(str1)), istr2(len_trim(str2))
  INTEGER idist(0:len_trim(str1),0:len_trim(str2))
  CHARACTER(LEN=26), PARAMETER :: sLowerCase = 'abcdefghijklmnopqrstuvwxyz'
  CHARACTER(LEN=26), PARAMETER :: sUpperCase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
!-------------------------------------------------------------------------------
!   Initialize => save ascii code for upper-case letters
    n1 = len_trim(str1)
    n2 = len_trim(str2)
    DO j = 1,n1
!     Is this a lower-case letter?
      i = index(sLowerCase,str1(j:j))
      if( i>0 ) then ! lower-case
        istr1(j) = ichar(sUpperCase(i:i))
      else ! upper-case or not a letter
        istr1(j) = ichar(str1(j:j))
      end if
    END DO
    idist(0,0) = 0
    DO j = 1,n2
!     Is this a lower-case letter?
      i = index(sLowerCase,str2(j:j))
      if( i>0 ) then ! lower-case
        istr2(j) = ichar(sUpperCase(i:i))
      else ! upper-case or not a letter
        istr2(j) = ichar(str2(j:j))
      end if
      idist(0,j) = j
    END DO

!   Compare strings
    DO i = 1,n1
      idist(i,0) = i
      DO j = 1,n2
        IF( istr1(i)==istr2(j) ) THEN
          icost = 0
        ELSE
          icost = 1
        END IF  !          deletion,       insertion,      substitution
        idist(i,j) = MIN( idist(i-1,j)+1, idist(i,j-1)+1, idist(i-1,j-1)+icost)
        IF( i>1 .and. j>1 .and. istr1(i)==istr2(j-1) .and. istr1(i-1)==istr2(j) ) THEN
          idist(I,J) = MIN( idist(i,j), idist(i-2,j-2)+icost ) ! transposition
        END IF
      END DO ! next j
    END DO ! next i
    iStr_Fuzzy_Match = idist(n1,n2)
    RETURN
  END FUNCTION iStr_Fuzzy_Match

END MODULE List_Util
