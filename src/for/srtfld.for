C
C SUBROUTINE SRTFLD
C $Log:   GXAFXT:[GOLS]SRTFLD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:17:40   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   03 Jun 1993 18:33:36   SXH
C  Put HASF before AGTREC, as latter requires PRMAGT, and AGTINF 
C  
C     Rev 1.0   21 Jan 1993 17:43:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - srtfld.for **
C
C SRTFLD.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-MAR-90 TDM INITIAL RELEASE FOR DENMARK.
C
C SUBROUTINE TO SORT ONE FIELD INTO ANOTHER
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
	SUBROUTINE SRTFLD(FIELD1,FIELD2,SORT,CNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:HASF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'

C
	INTEGER*4 FIELD1,           !First field index in ASF to sort on
     *	          FIELD2,           !Sub field index in ASF to sort on
     *	          FLEN1,            !Field 1 length
     *	          FLEN2,            !Field 2 length
     *	          IALPHA(8,NUMAGT)  !Contains data to sort on
C
	INTEGER*4 SORT(NUMAGT), CERR, AGTNBR, ST, AGT, CNT
C
	CALL OPENASF(3)
C
	CNT=0
	FLEN1=FLDEND(FIELD1)-FLDBEG(FIELD1)+1
	FLEN2=FLDEND(FIELD2)-FLDBEG(FIELD2)+1
	IF (FLEN1.GT.16) FLEN1=16
	IF (FLEN2.GT.16) FLEN2=16
	TYPE*,IAM(),'Loading agent sort table'
C
C READ BIG BLOCK FROM DISK
C
	DO 100 AGT=1,NUMAGT
C
	   CALL READASF(AGT,ASFREC,ST)
C
C CHECK IF ACTIVE AGENT
C
	   AGTNBR=0
	   CALL ASCBIN(ASFINF,SAGNO,LAGNO,AGTNBR,CERR)
	   IF(AGTNBR.EQ.0) GOTO 100
C
C EXTRACT SORT DATA
C
	   CNT=CNT+1
	   CALL MOVBYT(ASFINF,FLDBEG(FIELD1),IALPHA(1,CNT),1,FLEN1)
	   CALL MOVBYT(ASFINF,FLDBEG(FIELD2),IALPHA(1,CNT),
     *	               FLEN1+1,FLEN2)
C
	   SORT(CNT)=AGT
100	CONTINUE
C
C SORT ARRAY
C
	TYPE*,IAM(),'Sort in progress...'
	IF(CNT.NE.0)
     * 	  CALL I1SHELL(SORT,CNT,IALPHA,8)
	TYPE*,IAM(),'Sort complete...... '
	RETURN
C
	END
