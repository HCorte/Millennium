C SUBROUTINE POOLSAV
C 
C V07 01-MAR-2000 UXN Draw number also displayed.
C V06 22-FEB-2000 OXK Game names used in displaying the stats
C V05 17-Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C		      Instant Pass Thru Phase 1
C V04 13-Jul-1993 SXH Released for Finland
C V03 21-Jan-1993 DAB Initial Release
C  		      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  		      DEC Baseline
C V02 01-AUG-1990 XXX RELEASED FOR VAX
C V01 01-FEB-1989 XXX INITIAL RELEASE FOR SWEDEN
C
C SUBROUTINE TO SAVE LOTTO POOLS DURING STOPSYS
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE POOLSAV
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'


        ! variables
	INTEGER*4  FDB(7)            !
	INTEGER*4  PFDB(7)           !
	INTEGER*4  GAM               !
	INTEGER*4  RECN              !
	INTEGER*4  ST                !
	INTEGER*4  TOTAL             !

        INTEGER*4  GAM2(LTNUMGAMES)
	INTEGER*4  GNUM,GIND,GTYP
	INTEGER*4  I

	DATA TOTAL/0/
C
	TYPE *,IAM(),'Updating pool files '
	IF (LCHKSTOP.NE.LSUSPDON.AND.LCHKSTOP.NE.LSTOP) THEN
	    LCHKSTOP=LSUSPPOL         !SUSPEND POOL UPDATE
10	    CONTINUE
	    IF (LCHKSTOP.NE.LSUSPDON) THEN
	        CALL XWAIT(100,1,ST)
	        GOTO 10
	    ENDIF
	ENDIF
C
C
	CALL OPENQW(2,SFNAMES(1,LPR),4,0,0,ST)
	IF (ST.NE.0) THEN
	    CALL FILERR(SFNAMES(1,LPR),1,ST,0)
	    GOTO 30
	ENDIF
	CALL IOQINIT(FDB,2,LTOSEC*256)
C
	CALL OPENQW(3,SFNAMES(1,LP1),4,0,0,ST)
	IF(ST.NE.0) THEN
	    CALL FILERR(SFNAMES(1,LP1),1,ST,0)
	    GOTO 30
	ENDIF
	CALL IOQINIT(PFDB,3,LTOSEC*256)
C
C
	DO RECN=1,LTNUMPAG
	    CALL READQW(FDB,RECN,LTPAGE,ST)
	    IF (ST.NE.0) THEN
	        CALL FILERR(SFNAMES(1,LPR),2,ST,LTPAGE)
	        GOTO 30
	    ENDIF
	    CALL WRITEQW(PFDB,RECN,LTPAGE,ST)
	    IF(ST.NE.0) THEN
	        CALL FILERR(SFNAMES(1,LP1),3,ST,LTPAGE)
	        GOTO 30
	    ENDIF
        END DO
C
	RECN=LTNUMPAG+1
	CALL WRITEQW(PFDB,RECN,LTOVR,ST)
	IF(ST.NE.0) THEN
	    CALL FILERR(SFNAMES(1,LP1),3,ST,LTOVR)
	    GOTO 30
	ENDIF
C
C
	TYPE *,IAM(),'Testing pools  for consistency'
	DO 24, GNUM=1,MAXGAM
           GTYP=GNTTAB(GAMTYP,GNUM)
           GIND=GNTTAB(GAMIDX,GNUM)
           IF(GTYP.LE.0 .OR. GTYP.GT.LTPOOL_MAXTYP) GOTO 24
           IF(GIND.LE.0) GOTO 24
           GAM=LTPOOL_GAMENR(GTYP,GIND)
           IF (GAM.LE.0.OR.GAM.GT.LTNUMGAMES) GOTO 24
           GAM2(GAM)=GNUM
24	CONTINUE

	DO 25, GAM=1,LTNUMGAMES
	    IF (LTPOOLDRAW(GAM).LE.0) GOTO 25
	    CALL POOLTOT(FDB,TOTAL,1,GAM,LTPAGE,PAGESIZE)
	    WRITE(6,900)IAM(),TOTAL,(GLNAMES(I,GAM2(GAM)),I=1,4),LTPOOLDRAW(GAM)
25	CONTINUE
C
	LTSKSTAT=-5   !POOLSAV HAS FINISHED SAVING POOLS
	TYPE *,IAM(),'Pool file update complete'
C
30	CONTINUE
C
	LCHKSTOP=LSTOP           !STOP POOLPRO AND OVERPRO
	CALL CLOSEQFIL(FDB)
	CALL CLOSEQFIL(PFDB)

	RETURN

900	FORMAT (1X,A,I12,' boards bet for ',4A4,' draw ',I4)
	END
