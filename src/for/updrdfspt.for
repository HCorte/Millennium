C SUBROUTINE UPDRDFSPT
C  
C V01 22-MAR-2000 OXK Initial release (copied from UPDRDF.FOR)
C
C SUBROUTINE TO UPDATE ROLL POOL FILE W/ SPT-ROLL VALUES
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND
	SUBROUTINE UPDRDFSPT(GIND,DRAW,SPTPOLDIV)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECRDF.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

	INTEGER*4 GIND, DRAW
	INTEGER*4 SPTPOLDIV(SPGDIV)

 	INTEGER*4 FDB(7)
	INTEGER*4 GNUM, K, ST
C
C
	IF (GIND.LT.1 .OR. GIND.GT.NUMSPT) RETURN
	GNUM = GTNTAB(TSPT,GIND)
	IF (GNUM.LT.1 .OR. GNUM.GT.MAXGAM) RETURN

        CALL WAITLOCK(WRFBIT,ASFLOCK)
        WRITE(6,900) IAM(),(SFNAMES(K,RDF),K=1,5)
        CALL OPENW(1,SFNAMES(1,RDF),4,0,0,ST)
        CALL IOINIT(FDB,1,RDFSEC*256)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,RDF),1,ST,0)
        CALL READW(FDB,1,RDFREC,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,RDF),2,ST,1)
C
C
	CALL FASTMOV (SPTPOLDIV, RDF_SPTPOLDIV(1,GIND), SPGDIV)
	RDFADW(GNUM) = DRAW
C
C
        CALL WRITEW(FDB,1,RDFREC,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,RDF),3,ST,1)
	CALL CLOSEFIL(FDB)
        CALL FREELOCK(WRFBIT,ASFLOCK)
	RETURN
C
900	FORMAT(1X,A,' Updating roll pool file ',5A4)
	END
