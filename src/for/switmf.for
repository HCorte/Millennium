C
C $Log:   GXAFXT:[GOLS]SWITMF.FOV  
C  
C     Rev 1.0   17 Apr 1996 15:23:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   26 Oct 1993 16:49:14   GXA
C  Changed Logical unit to 10 from 1 not to conflict with the game.
C  
C     Rev 1.0   22 Oct 1993 15:26:12   GXA
C  Initial revision.
C
C SWITMF.FTN
C
C SUBROUTINE TO UPDATE SCF.FIL WITH CHANGED TMF NAMES.
C
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode
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
        SUBROUTINE SWITMF
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
C
	INTEGER*4 SCFNAM(5)		!System Control File Name
	INTEGER*4 FDB(7)		!File Descriptor Block for SCF.
	INTEGER*4 ST		        !Subroutine Return Status.
C
	DATA	  SCFNAM/'SCF.','FIL ',3*0/
C
	CALL OPENW(10,SCFNAM,4,0,0,ST)
	IF(ST.NE.0) THEN
	   CALL FILERR(SCFNAM,1,ST,0)
	   RETURN
	ENDIF
	CALL IOINIT(FDB,10,SCFSEC*256)
        CALL READW(FDB,1,SCFREC,ST)
        IF(ST.NE.0) THEN
           CALL FILERR(SCFNAM,2,ST,1)
           CALL CLOSEFIL(FDB)
           RETURN
        ENDIF
C
	CALL FASTMOV(SFNAMES(1,PTMF),SCFSFN(1,PTMF),5)
	CALL FASTMOV(SFNAMES(1,BTMF),SCFSFN(1,BTMF),5)
	SCFPAR(DISKSW)=P(DISKSW)
C
	CALL WRITEW(FDB,1,SCFREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFNAM,3,ST,1)
C
	CALL CLOSEFIL(FDB)
	RETURN
	END
