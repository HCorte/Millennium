C
C SUBROUTINE TMLODASF
C $Log:   GXAFXT:[GOLS]TMLODASF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:35:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:52:00   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - tmlodasf.for **
C
C TMLODASF.FOR
C
C V01 28-MAR-91 JPJ INITAL RELEASE FOR MARYLAND
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
	SUBROUTINE TMLODASF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:TMSAGT.DEF'
C
C
C
	INTEGER*4 I, ST, J, ANUM, CERR, DROP, STN
	CHARACTER CDRP(4)
	EQUIVALENCE(CDRP,DROP)
C
C LOAD AGENT INFO NEEDED FOR TMSCAN INTO COMMON
C
	TYPE *,IAM(),' Loading asf info into common'
	DO 200 I=1,NUMAGT
	   CALL READASF(I,ASFREC,ST)
	   IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),1,ST,0)
C
C LOAD UP GUTS SWAP TABLE FOR (GUTSW.FOR)
C
	   DO 210 J=1,9
	      GTSTAB(I,J)=ASFGUT(J)	
210	   CONTINUE
C
C LOAD UP AGENT NUMBERS FOR (GUTSTAT.FOR)
C
	   CALL ASCBIN(ASFINF,SAGNO,LAGNO,ANUM,CERR)
           GTSAGTN(I)=ANUM
C
C LOAD LINE AND DROP FOR (TAPRSUB.FOR)
C
           CALL ASCBIN(ASFINF,SXSTN,LXSTN,STN,ST)
           IF (ST.NE.0 .OR. STN.LT.1 .OR.
     *         STN.GT.X2X_STATIONS) GOTO 200
           DROP = 0
           CDRP(4) = ASFBYT(SDROP)
           DROP = DROP - 63
           IF (DROP.LT.1 .OR. DROP.GT.80) GOTO 200
           LINDRP(STN,DROP) = I
	   CHRDRP(I) = ASFBYT(SDROP)
C
           DO 111 J=1,MAXGAM
              LINSAL(STN)=LINSAL(STN)+ASFDAY(GSAMT,J,1)-
     *                                ASFDAY(GCAMT,J,1)
	      SALAMT(I) = SALAMT(I) + ASFDAY(GSAMT,J,1)-
     *                                ASFDAY(GCAMT,J,1)
111        CONTINUE
200	CONTINUE
	TYPE *,IAM(),' Done loading asf info into common'
	RETURN
	END
