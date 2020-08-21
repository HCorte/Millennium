C
C SUBROUTINE SYSMODE
C
C SYSMODE.FOR
C 
C V09 13-JUN-2000 UXN IDATE replaced with GDATE
C V08 11-Nov-1999 RXK Changed for ALPHA (UXN).
C V07 21-MAY-1999 UXN STARTIM CHANGED TO START_TIME
C V06 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V05 07-APR-1992 GCAN FIXED DATE CHECK DISCREPANCY.
C V04 30-MAY-1991 RRB  Worry about test mode again.
C V03 14-MAR-1991 TKO  no longer worries about test mode.
C V02 01-AUG-1990 XXX  RELEASED FOR VAX
C V01 07-JUN-1989 SWB INITIAL RELEASE
C
C THIS SUBROUTINE WILL GET THE START PARAMATERS PASSED TO RESET
C WHEN RESET IS STARTED. IT WILL THEN ASK THE OPERATOR IF THE
C SYSTEM MODE IS CORRECT (LIVE OR TEST)
C
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SYSMODE(TSTFLG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	LOGICAL   TSTFLG            !.TRUE. IF TEST MODE
C
	INTEGER*4 TIM, ANS
	INTEGER*2 GOLDAT(LDATE_LEN)
	INTEGER*4 START_TIME(3),OSDATE(8)
C
C SET TEST FLAG IF TEST MODE
C
	IF(TSTFLG) THEN
	   CALL BELLS(5)
	   TYPE*,IAM(),'***'
     	   TYPE*,IAM(),'*** System will be brought up in TEST mode....'
	   TYPE*,IAM(),'***'
	   CALL BELLS(5)
	   CALL BELLS(5)
	   CALL WIMG(5,'Is this correct? (Y/N) ')
	   CALL YESNO(ANS)
	   IF(ANS.NE.1) CALL GSTOP(GEXIT_SUCCESS)
	   P(TSTMOD)=1
	   GOTO 9000
	ENDIF
C
C CHECK OS DATE WITH GOLS SYSTEM DATE
C
	GOLDAT(VCDC)=DAYCDC
	CALL LCDATE(GOLDAT)
        CALL GDATE(OSDATE(2),OSDATE(3),OSDATE(1))
        IF(OSDATE(1).LT.77) THEN
          OSDATE(1) = OSDATE(1) + 2000
        ELSE
          OSDATE(1) = OSDATE(1) + 1900
        ENDIF
	IF(OSDATE(1).NE.GOLDAT(VYEAR2).OR.
     *	   OSDATE(2).NE.GOLDAT(VMON).OR.
     *	   OSDATE(3).NE.GOLDAT(VDAY)) THEN
	   CALL BELLS(5)
	   TYPE*,IAM(),'***'
	   WRITE(5,9200) IAM(),OSDATE(3),OSDATE(2),OSDATE(1)
	   WRITE(5,9201) IAM(),GOLDAT(VDAY),GOLDAT(VMON),GOLDAT(VYEAR2)
           TYPE*,IAM(),'*** System will be brought up in LIVE mode'
	   TYPE*,IAM(),'***'
	   CALL BELLS(5)
	   CALL BELLS(5)
	   CALL WIMG(5,'Is this correct? (Y/N) ')
	   CALL YESNO(ANS)
	   IF(ANS.NE.1) CALL GSTOP(GEXIT_SUCCESS)
	   GOTO 9000
	ENDIF
C
C CHECK TIME
C
	CALL XTIM(START_TIME)
	IF(P(RUNBEG).EQ.0.OR.P(RUNEND).EQ.0) GOTO 9000
	TIM=START_TIME(1)*100+START_TIME(2)
	IF(TIM.LT.P(RUNBEG).OR.TIM.GT.P(RUNEND)) THEN
	   CALL BELLS(5)
	   TYPE*,IAM(),'***'
	   TYPE*,IAM(),'*** System is being brought up outside of normal'
	   TYPE*,IAM(),'*** operational start times.'
	   WRITE(5,9202) IAM(),P(RUNBEG)/100,MOD(P(RUNBEG),100),
     *	                 P(RUNEND)/100,MOD(P(RUNEND),100)
           TYPE*,IAM(),'*** System will be brought up in LIVE mode'
	   TYPE*,IAM(),'***'
	   CALL BELLS(5)
	   CALL BELLS(5)
	   CALL WIMG(5,'Is this correct (Y/N)')
	   CALL YESNO(ANS)
	   IF(ANS.NE.1) CALL GSTOP(GEXIT_SUCCESS)
	   GOTO 9000
	ENDIF
C
C SYSTEM HAS BEEN BROUGHT IN WRONG MODE
C RESET WILL CALL GSTOP(GEXIT_SUCCESS)
C
9000	CONTINUE
	RETURN
C
C FORMAT STATEMENT
C
9200	FORMAT(1X,A,1X,'*** SYSTEM Date is ',I2.2,'.',I2.2,'.',I4.4)
9201	FORMAT(1X,A,1X,'*** GTECH  Date is ',I2.2,'.',I2.2,'.',I4.4)
9202	FORMAT(1X,A,1X,'*** Between ',I2.2,':',I2.2,' and ',I2.2,':',I2.2)
	END
