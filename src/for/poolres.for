C
C SUBROUTINE POOLRES
C $Log:   GXAFXT:[GOLS]POOLRES.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:25:40   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:18:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - poolres.for **
C
C POOLRES.FOR
C
C     09-APR-91 MP ELIMINATED CHECK OF THE VOLUME NAME (USED TO BBE 'XRAM')
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 15-OCT-89 WS RELEASED FOR FINLAND
C
C SUBROUTINE TO RESTORE LOTTO POOLS FOR RUNSYS
C
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
	SUBROUTINE POOLRES
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOPOL.DEF'
C
	INTEGER*4 FDB(7),PFDB(7),  CURPAG, ST
C
	TYPE *,IAM(), 'Restoring lotto pools'
C
        IF(P(LTOPOL_RAM) .EQ. LTOPOL_FIL_VALUE) THEN
D         TYPE *,IAM(), 'POOLS ARE FILE_BASED'
        ELSE
D         TYPE *,IAM(), 'POOLS ARE RAM_BASED'
        ENDIF
C
	CALL OPENQW(2,SFNAMES(1,LPR),4,0,0,ST)
	IF (ST.NE.0) THEN
	  CALL FILERR(SFNAMES(1,LPR),1,ST,0)
	  CALL GPAUSE
	  GOTO 30
	ENDIF
	CALL IOQINIT(FDB,2,LTOSEC*256)
C
	CALL OPENQW(3,SFNAMES(1,LP1),4,0,0,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(SFNAMES(1,LP1),1,ST,0)
	  CALL GPAUSE
	  GOTO 30
	ENDIF
	CALL IOQINIT(PFDB,3,LTOSEC*256)
C
C
	DO 20 CURPAG=LTNUMPAG+1,1,-1
	IF (CURPAG.EQ.LTNUMPAG+1) THEN
	  CALL READQW(PFDB,CURPAG,LTOVR,ST)
	ELSE
	  CALL READQW(PFDB,CURPAG,LTPAGE,ST)
	ENDIF
	IF(ST.NE.0) THEN
	  CALL FILERR(SFNAMES(1,LP1),2,ST,CURPAG)
	  CALL GPAUSE
	  GOTO 30
	ENDIF
C
	IF (CURPAG.EQ.LTNUMPAG+1) GOTO 20
C
        IF(P(LTOPOL_RAM)  .NE. LTOPOL_FIL_VALUE) THEN
         CALL FASTMOV(LTPAGE,LTOPOL_SPACE((CURPAG-1)*PAGESIZE+1),
     1	      PAGESIZE)
        ENDIF
C
	CALL WRITEQW(FDB,CURPAG,LTPAGE,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(SFNAMES(1,LPR),3,ST,CURPAG)
	  CALL GPAUSE
	  GOTO 30
	ENDIF
20	CONTINUE
C
	LTCURPAG=1
	TYPE *,IAM(), 'Lotto pool restore complete.'
C
30	CONTINUE
	CALL CLOSEQFIL(FDB)
	CALL CLOSEQFIL(PFDB)
C
	CALL POOLSET                  !SET GAME DATA IN THE MEMORY
C
	RETURN
C
	END
