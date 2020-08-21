C
C  SUBROUTINE X2BCSTX.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BCSTX.FOV                                  $
C  $Date::   17 Apr 1996 16:08:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C**************
C X2BCSTX - find which BCST using the STN PORT idx
C
C V03 20-JUL-94 WS MULTINETWORK CHANGES - Integrate UK changes into 
C		   X2X Baseline
C V02 25-MAY-94 GPR Check index only if port enabled
C V01 23-DEC-91 MF  ORIGINAL
C
C Calling Sequence:
C
C     CALL X2BCSTX(STN_BCST_PORT_IDX,STN,BCST_IDX)
C
C Input parameters:
C
C     STN_BCST_PORT_IDX  Int*4       Port Index
C     STN             Int*4          Station number
C Output:
C
C     BCST_IDX       Int*4          BCST index
C                                    1,2  - valid
C                                    0 - invalid
C***************
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE X2BCSTX(STN_BCST_PORT_IDX,STN,BCST_IDX)
      IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
      INTEGER*4  STN_BCST_PORT_IDX  ! Port Index
      INTEGER*4  STN                ! Station number
      INTEGER*4  BCST_IDX           ! BCST index (o- invalid)
C
      INTEGER*4  CLASS			!V03
      INTEGER*4  TYPE_PORT1, TYPE_PORT2 ! work
      LOGICAL    ENABLE1,ENABLE2, ENABLE    ! TRUE if PORTn enabled   ! V02,V03
C

C     ***** Start V02 changes *****

      CLASS = X2XS_STNCLS(STN)		!V03

      ENABLE  = ICHAR(X2XS_BCST_ENABLE(STN)) .EQ. 0		!V03
      ENABLE1 = X2XC_BCST_ENABLE1(CLASS) .EQ. 0  .AND. ENABLE	!V03
      ENABLE2 = X2XC_BCST_ENABLE2(CLASS) .EQ. 0  .AND. ENABLE	!V03

      IF (ENABLE1) THEN						!V03
         TYPE_PORT1 =  X2XPN_TYPE(X2XC_BCST_NET_PORT1(CLASS))	!V03
      ELSE
         TYPE_PORT1 = X2XPT_UNDEF
      ENDIF

      IF (ENABLE2) THEN
         TYPE_PORT2 =  X2XPN_TYPE(X2XC_BCST_NET_PORT2(CLASS))	!V03
      ELSE 
         TYPE_PORT2 = X2XPT_UNDEF
      ENDIF

C     ***** End V02 changes *****

C
C Find BCST corresponding to the STN BCST port index
C
      IF (STN_BCST_PORT_IDX .eq. 0 ) THEN
        BCST_IDX = 0
      ELSEIF (STN_BCST_PORT_IDX .eq. 1 ) THEN
        IF     (TYPE_PORT1 .EQ.  X2XPT_BCST1) THEN
          BCST_IDX  = 1                                ! First BCST server
        ELSEIF (TYPE_PORT1 .EQ.  X2XPT_BCST2) THEN
          BCST_IDX  = 2                                ! Second BCST server
        ELSE
          BCST_IDX  = 0                                ! Invalid net port
        ENDIF
      ELSEIF (STN_BCST_PORT_IDX .eq. 2 ) THEN
        IF     (TYPE_PORT2 .EQ.  X2XPT_BCST1) THEN
          BCST_IDX  = 1                                ! First BCST server
        ELSEIF (TYPE_PORT2 .EQ.  X2XPT_BCST2) THEN
          BCST_IDX  = 2                                ! First BCST server
        ELSE
          BCST_IDX  = 0                                ! Invalid net port
        ENDIF
      ENDIF
C
      RETURN
      END
