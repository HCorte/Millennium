C
C SUBROUTINE DECLTO
C $Log:   GXAFXT:[GOLS]DECLTO.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:50:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:02:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - declto.for **
C
C DECLTO.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C 07-MAY-90   LOU R. INITIAL RELEASE FOR DENMARK.
C
C DECODE BET INTO OUR FORMAT FOR LOTTO
C
C INPUT:
C       ALIAN   - FOREIGN REPRESENTATION OF BET
C       RECCNT  - NUMBER OF BETS DECODED IN BATCH
C       SYSMARK - MAXIMUM MARK FOR THIS SYSTEM
C
C OUTPUT:
C       POINTER - POINTER IN SYSTEM TABLE
C       BETPROC - TOTAL BETS PROCESSED THIS SYSTEM TYPE
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DECLTO(ALIAN,RECCNT,SYSMARK,POINTER,BETPROC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:LSYSCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 ALIAN(20,10),BETDEF(LMXMARK),TEMP
C
	INTEGER*4 XX, COUNT, OFF, CERR, MARKOFF, SUBMRK, TABOFF
	INTEGER*4 BETPROC, POINTER, SYSMARK, RECCNT
C
	CHARACTER CTEMP(4)
	EQUIVALENCE(TEMP,CTEMP(1))
C
	DO 1000 TABOFF=1,30,3
	SUBMRK=0
	CALL FASTSET(0,BETDEF,LMXMARK)
	DO 100 MARKOFF=1,RECCNT
	TEMP=0
	CALL MOVBYTN(ALIAN(1,MARKOFF),TABOFF,TEMP,3,2)
	IF(CTEMP(3).EQ.' ') CTEMP(3)='0'
	CALL ASCBIN(TEMP,3,2,BETDEF(MARKOFF),CERR)
	IF(CERR.NE.0) GOTO 100
	SUBMRK=SUBMRK+1
100	CONTINUE
	IF(SUBMRK.LE.0) GOTO 1000
C
	LSYS_TAB(POINTER+1)=0
	DO 200 OFF=1,SUBMRK
	IF(BETDEF(OFF).GT.LMXMARK.OR.BETDEF(OFF).LE.0.OR.
     *	   BETDEF(OFF).GT.SYSMARK) THEN
	  TYPE *,' INVALID BET AT OFFSET ',OFF,' BET ',BETDEF(OFF)
	  GOTO 200
	ELSE
	  CALL BSET(LSYS_TAB(POINTER+1),BETDEF(OFF)-1)
	ENDIF
200	CONTINUE
C
	CALL BITCNT(LSYS_TAB(POINTER+1),L_SYSBYTES,COUNT)
	IF(COUNT.NE.SUBMRK) THEN
	   TYPE*,' INVALID COUNT OF BITS IN DEFINITION ',COUNT
	   TYPE*,' SHOULD BE ',SUBMRK
	ENDIF
C
	LSYS_TAB(POINTER)=SUBMRK
	POINTER=POINTER+2
	BETPROC=BETPROC+1
	WRITE(5,900) (BETDEF(XX),XX=1,SUBMRK)
	WRITE(7,901) BETPROC,(BETDEF(XX),XX=1,SUBMRK)
1000	CONTINUE
	RETURN
C
C THE FOLLOWING ARE FORMATS USED
C
900	FORMAT(1X,20(1X,I2))
901	FORMAT(1X,' BET ',I4,2X,20(1X,I2))
	END
