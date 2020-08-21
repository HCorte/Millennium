C  GXSRC:FPTPRO.FOR
C  
C  $Log:   GXAFXT:[GOLS]FPTPRO.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:12:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   03 Jan 1994 20:28:18   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:44:14   SYSTEM
C  Initial revision.
C
C V01 15-MAY-93 DSL INITAL RELEASE FINANCIAL PASS-THROUGH PROCESSING TASK
C     							  (FPT0001)
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM FPTPRO
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INTEGER*4 COUNT,ST
	DATA COUNT/0/
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
C
10	CONTINUE
	IF(DAYSTS.EQ.DSCLOS) CALL GSTOP(GEXIT_SUCCESS)
	IF(DAYSTS.EQ.DSSUSP) THEN
	  CALL HOLD(0,ST)
	  GOTO 10
	ENDIF
	CALL XWAIT(10,2,ST)              !XWAIT EVERY 10 SECONDS
	IF(P(SYSTYP).NE.LIVSYS) GOTO 10
	IF(P(SUPFPT).EQ.1) GOTO 10
	IF(P(FPTTIM).EQ.0) GOTO 10       !PXN ADDED TO DISABLE
	IF(P(PRMSTR).EQ.0) GOTO 10       !Not connected to IPS
	IF(P(SUPINS).EQ.1) GOTO 10       !IPS transactions are suppressed
C
C	BECAUSE XWAIT IS CONIGURED FOR A 10 SECOND LOOP, 6 WAITS EVERY MINUTE
C	FPTTIM IS AN ARRAY IN SECONDS, DIVID BY 10 TO DETERMINE NUMBER OF
C	WAIT LOOPS BEFORE RELEASE TO FORMAT MESSAGE
C	
C
	IF(MOD(COUNT,P(FPTTIM)/10).EQ.0) THEN
	  CALL FPTBLD(ST)
	  COUNT=MOD(COUNT,P(FPTTIM)/10)
	  COUNT=COUNT+1
	ELSE
	  COUNT=COUNT+1
	ENDIF
C
	GOTO 10
C
	END
