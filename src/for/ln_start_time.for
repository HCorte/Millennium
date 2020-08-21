C
C SUBROUTINE LN_START_TIME
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LN_START_TIME.FOV                            $
C  $Date::   17 Apr 1996 13:51:48                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - ln_start_trap.for;1 **
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LN_START_TIME
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:LANEVN.DEF'
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
	RECORD /LN_IOSSTRUCT/ LOCAL_IOSB
C
	EXTERNAL LTIMTRAP
C
	IF(INIT.EQ.-1) THEN
C
C CALCULATE TIME OFFSET FOR TIMER TRAP.
C
          WRITE(CURTIM,9000) LANSTEP/1000,MOD(LANSTEP,1000)
9000      FORMAT('0000 00:00:',I2.2,'.',I3.3)
          STATUS=SYS$BINTIM(CURTIM,DELAY)
          IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	  INIT=0
	ENDIF
C
C ENABLE TIMER TRAP
C
        STATUS=SYS$SETIMR(,DELAY,LTIMTRAP,PTIMTICK,0)    ! 1 = PTIMTICK
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
	RETURN
	END
