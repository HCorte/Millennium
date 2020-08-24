C SUBROUTINE OVRSNP
C  
C  V07 31-MAY-2000 PXO Subroutine name fron OUSSNP -> OVRSNP
C  V06 17-Apr-1996 HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  V05 19-Jun-1993 HXK
C  removed PRMAGT.DEF, included AGTCOM.DEF
C  V04 13-Jun-1993 HXK
C   added AGTINF.DEF, PRMAGT.DEF
C  V03 21-Jan-1993 DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C V02 09-APR-91 MTK INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C COPYRITF.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C COPYRIGHT 1991 GTECH CORPORATION.  ALL RIGHTS RESERVED.
C
C CONFIDENTIAL PROPRIETARY INFORMATION
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
C Island, and contains confidential and trade secret information.  It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH.  Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published or disclosed, in whold or in part, directly
C or indirectly, except as expressly authorized by an officer of
C GTECH pursuant to written agreement.
C COPYRITF.DEF-------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE OVRSNP(OPT,NUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'

	INTEGER*4 ASAL(2,72)
	INTEGER*4 SLIM,NUM,I,J,L,IND,TOTSAL,OPT
C
C CLEAR PAGE IMAGE
C
	SMODE=.TRUE.
	SLIM=NUM
	IF(SLIM.LT.0) SLIM=0
	SLIM=SLIM*100/DYN_BETUNIT
	DO 10 I=1,24
	DO 10 J=1,20
	NEW(J,I)='    '
10	CONTINUE
C
C
	DO 20 I=1,72
	DO 20 J=1,2
	ASAL(J,I)=0
20	CONTINUE
C
C GET AGENT DATA
C
	IND=0
	DO 30 I=1,NUMAGT
	IF(AGTTAB(AGTNUM,I).EQ.0) GOTO 30
	TOTSAL=0
	DO 25 J=1,MAXGAM
	TOTSAL=TOTSAL+AGTGAM(GSAMT,J,I)-AGTGAM(GCAMT,J,I)
25	CONTINUE
	IF(OPT.EQ.1.AND.TOTSAL.LE.SLIM) GOTO 30
	IF(OPT.EQ.2.AND.TOTSAL.GE.SLIM) GOTO 30
	IND=IND+1
	IF(IND.GT.72) GOTO 30
	ASAL(1,IND)=AGTTAB(AGTNUM,I)
	ASAL(2,IND)=TOTSAL
30	CONTINUE
C
C
C
C ENCODE SNAPSHOT
C
	IF(OPT.EQ.1) WRITE(CLIN1,901) IND,CSMONY(SLIM,9,BETUNIT)
	IF(OPT.EQ.2) WRITE(CLIN1,9011)IND,CSMONY(SLIM,9,BETUNIT)
	WRITE(CLIN3,903)
	L=3
	DO 40 I=1,72,4
	L=L+1
	CALL ENCSLM(CNEW(1,L),ASAL,I)
40	CONTINUE
	RETURN
C
C
901	FORMAT(1X,I5,' agents have sales over ',A9)
9011	FORMAT(1X,I5,' agents have sales under ',A9)
903	FORMAT(4(' Agent num     Sales'))
	END
