C
C SUBROUTINE X2PNCMP.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   DKA100:[X2XBASELINES.EURO_BASE]X2PNCMP.FOV                $
C  $Date::   24 Mar 1995 17:28:20                                         $
C  $Revision::   1.0                                                      $
C  $Author::   WJK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C**************
C X2FPNCMP
C
C Compares a DTE address to the STN network port PORT1 and to PORT2,
C and verifies that ENABLE1 and  ENABLE2 are set.
C Returns 0 if no match or disabled, 1 - if matches first that is enabled,
C 2 - if the second port and it is enabled.
C
C V04 25-FEB-97 DXG ADDR NEEDS TO BE SHIFTED WITH IT'S LENGTH TO BE CORRECT
C V03 20-JUL-94 WS MULTINETWORK CHANGES - Integrate UK changes 
C                  into X2X Baseline
C V02 24-MAY-94 GPR Only check DTE when enabled
C V01 21-DEC-91 MF  Original
C
C Calling Sequence:
C
C     CALL X2FPNCMP(LEN,ADDR,STN,STS)
C
C Input parameters:
C
C     LEN      Int*4           Length of Addr
C     ADDR     Int*4(2)        DTE Address , 16 BCD nibbles max
C     STN      Int*4           Station num
C
C Output status:
C
C     STS      Int*4           Output status code:
C                              0 - not equal
C                              1 - equal to PORT1 address, ENABLE1 set
C                              2 - equal to PORT2 address, ENABLE2 set
C*************
      SUBROUTINE X2PNCMP(LEN,ADDR,STN,STS)
      IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:X2XCOM.DEF'
C
      INTEGER*4 LEN                             ! len of DTE addr [nibbles]
      INTEGER*4 ADDR(2)                         ! DTE address
      INTEGER*4 TMP_ADDR(2)                     ! Net port address      ! V02
      INTEGER*4 TMP_LEN                         ! Net port addr len     ! V02
      INTEGER*4 STN                             ! station number
      INTEGER*4 STS                             ! return status
C
      LOGICAL EQUAL1, EQUAL2                    ! TRUE if addr = PORTn addr
      LOGICAL ENABLE1,ENABLE2                   ! TRUE if PORTn enabled
      INTEGER*4 CLASS                           ! V03
C
      CLASS = X2XS_STNCLS(STN)                  ! V03

CV03      ENABLE1 = ichar(X2XS_BCST_ENABLE1(STN)) .EQ. 0
CV03      ENABLE2 = ichar(X2XS_BCST_ENABLE2(STN)) .EQ. 0

      ENABLE1 = X2XC_BCST_ENABLE1(CLASS) .EQ. 0 ! V03
      ENABLE2 = X2XC_BCST_ENABLE2(CLASS) .EQ. 0 ! V03
C
C
C     Compare DTE addr with STN BCST port1
C

C     ***** Start V02 changes 

      IF (ENABLE1) THEN
CV03         TMP_ADDR(1) = X2XPN_ADRESS(1,X2XS_BCST_NET_PORT1(STN))
CV03         TMP_ADDR(2) = X2XPN_ADRESS(2,X2XS_BCST_NET_PORT1(STN))
CV03         TMP_LEN = X2XPN_ADDLEN(X2XS_BCST_NET_PORT1(STN))
         TMP_ADDR(1) = X2XPN_ADRESS(1,X2XC_BCST_NET_PORT1(CLASS))       !V03
         TMP_ADDR(2) = X2XPN_ADRESS(2,X2XC_BCST_NET_PORT1(CLASS))       !V03
         TMP_LEN = X2XPN_ADDLEN(X2XC_BCST_NET_PORT1(CLASS))             !V03
         CALL X2QSHFT(TMP_ADDR,64-(TMP_LEN*4))
         CALL X2QSHFT(ADDR,64-(LEN*4))                                  !V04
         CALL X2EQDTE( LEN,
     *                 ADDR,
     *                 TMP_LEN,
     *                 TMP_ADDR,
     *                 STS)

         EQUAL1 = STS .EQ. 1
      ELSE
         EQUAL1 = .FALSE.
      ENDIF
C
      IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *        TYPE *, 'EQUAL1', LEN, ADDR, TMP_LEN, TMP_ADDR, EQUAL1
C
C     Compare DTE addr with STN BCST port2
C
      IF (ENABLE2) THEN
CV03         TMP_ADDR(1) = X2XPN_ADRESS(1,X2XS_BCST_NET_PORT2(STN))
CV03         TMP_ADDR(2) = X2XPN_ADRESS(2,X2XS_BCST_NET_PORT2(STN))
CV03         TMP_LEN = X2XPN_ADDLEN(X2XS_BCST_NET_PORT1(STN))
         TMP_ADDR(1) = X2XPN_ADRESS(1,X2XC_BCST_NET_PORT2(CLASS))       !V03
         TMP_ADDR(2) = X2XPN_ADRESS(2,X2XC_BCST_NET_PORT2(CLASS))       !V03
         TMP_LEN = X2XPN_ADDLEN(X2XC_BCST_NET_PORT2(CLASS))             !V03
         CALL X2QSHFT(TMP_ADDR,64-(TMP_LEN*4))
         CALL X2EQDTE( LEN,
     *                 ADDR,
     *                 TMP_LEN,
     *                 TMP_ADDR,
     *                 STS)
         EQUAL2 = STS .EQ. 1
      ELSE
         EQUAL2 = .FALSE.
      ENDIF

C
      IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *        TYPE *, 'EQUAL2', LEN, ADDR, TMP_LEN, TMP_ADDR, EQUAL2
C
C     ***** End V02 changes 

C
      IF     ( EQUAL1 .AND. ENABLE1 ) THEN
        STS = 1
      ELSEIF ( EQUAL2 .AND. ENABLE2 ) THEN
        STS = 2
      ELSE
        STS = 0
      ENDIF
C
      IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *        TYPE *, 'RET X2PNCMP, STS:', STS
C
      RETURN
      END
