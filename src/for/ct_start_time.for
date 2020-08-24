C
C SUBROUTINE CT_START_TIME
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CT_START_TIME.FOV                            $
C  $Date::   17 Apr 1996 12:46:12                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92
C  DEC Baseline
C
C ** Source - ct_start_trap.for **
C
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CT_START_TIME
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CTLCOM.DEF'
	INCLUDE 'INCLIB:CTLEVN.DEF'
C
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 INIT/-1/
	INTEGER*4 STATUS
C
	INTEGER*4   DELAY(2)		!TIMER DELAY
	CHARACTER   CURTIM*30		!ASC TIME
	RECORD /CT_IOSSTRUCT/ LOCAL_IOSB
C
	EXTERNAL CTIMTRAP
C
	IF(INIT.EQ.-1) THEN
C
C CALCULATE TIME OFFSET FOR TIMER TRAP.
C
          WRITE(CURTIM,9000) CTLINT/1000,MOD(CTLINT,1000)
9000      FORMAT('0000 00:00:',I2.2,'.',I3.3)
          STATUS=SYS$BINTIM(CURTIM,DELAY)
          IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	  INIT=0
	ENDIF
C
C ENABLE TIMER TRAP
C
        STATUS=SYS$SETIMR(,DELAY,CTIMTRAP,
     *                    0,0)
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
	RETURN
	END
