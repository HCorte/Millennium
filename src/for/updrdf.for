C SUBROUTINE UPDRDF
C  
C V04 22-MAR-2000 OXK Locking of the file implemented
C V03 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V02 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C V01 12-NOV-91 MTK INITIAL RELEASE FOR NETHERLANDS
C
C SUBROUTINE TO UPDATE ROLL POOL FILE
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE UPDRDF(GAM,AMOUNT,DRAW)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECRDF.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

 	INTEGER*4 FDB(7)
	INTEGER*4 GAM, ST, DRAW, K, AMOUNT
C
C
	IF(AMOUNT.EQ.0) RETURN

        CALL WAITLOCK(WRFBIT,ASFLOCK)
        WRITE(6,900) IAM(),(SFNAMES(K,RDF),K=1,5)
        CALL OPENW(1,SFNAMES(1,RDF),4,0,0,ST)
        CALL IOINIT(FDB,1,RDFSEC*256)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,RDF),1,ST,0)
        CALL READW(FDB,1,RDFREC,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,RDF),2,ST,1)
C
C
	IF(DRAW.EQ.RDFADW(GAM)) 
     *    RDFPOL(GAM)=RDFPOL(GAM)-RDFADD(GAM)
	RDFADW(GAM)=DRAW
	RDFADD(GAM)=AMOUNT
        RDFPOL(GAM)=RDFPOL(GAM)+AMOUNT
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
