C
C $Log:   GXAFXT:[GOLS]OPNPOL_CNTRL.FOV  
C  
C     Rev 1.0   17 Apr 1996 14:19:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   03 Dec 1993 15:31:02   HXK
C  REMOVED TYPE STATEMENT
C  
C     Rev 1.0   15 Nov 1993 19:35:46   GXA
C  Initial revision.
C
C
C SUBROUTINE TO CALCULATE CONTROL REVISION FOR OPINION POLLS.
C THE CONTROL NUMBER CONSISTS OF: START, END DATE, POLL ID AND
C                                 TICKET TEXT REVISION, CHECKSUMED.
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
	SUBROUTINE OPNPOL_CNTRL(POLLNUM,REVNUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4	POLLNUM			!Opinion Poll number.
	INTEGER*4	REVNUM			!Control Rev#.
C
	BYTE		BYTTAB(50)		!Byte Checksum table.
	BYTE		I1TEMP(4)
C
	INTEGER*4       I4TEMP
	INTEGER*4	IND			!Index into BYTTAB. 
C
	EQUIVALENCE	(I4TEMP,I1TEMP)
C
C
	REVNUM = 0
	IND = 1
C
	IF(POLLNUM.LT.1.OR.POLLNUM.GT.PRM_NUMOPN) RETURN
C
	IF(SCC_OPNDATE(PRM_STRDAT,POLLNUM).LE.DAYCDC.AND.
     *     SCC_OPNDATE(PRM_ENDDAT,POLLNUM).GE.DAYCDC)     THEN
	   CALL MOVBYT(SCC_OPNDATE(PRM_STRDAT,POLLNUM),1,BYTTAB,IND,4)
	   IND = IND + 4
	   CALL MOVBYT(SCC_OPNDATE(PRM_ENDDAT,POLLNUM),1,BYTTAB,IND,4)
	   IND = IND + 4
	   CALL MOVBYT(SCC_OPNID(POLLNUM),1,BYTTAB,IND,4)
	   IND = IND + 4
	   IND = IND - 1
	   CALL MOVBYT(TKTMRV(MAXGAM+POLLNUM),1,BYTTAB,IND,4)
	   CALL CHECKSUM(BYTTAB,0,IND,REVNUM)
	   I4TEMP = REVNUM
	   I1TEMP(2) = SCC_OPNID(POLLNUM)
C***	   TYPE*,IAM(),'Opnion Poll ',POLLNUM,' ID ',SCC_OPNID(POLLNUM),
C*** *	               ' activ ','Checksum: ',REVNUM
	ENDIF
C
	RETURN
	END
