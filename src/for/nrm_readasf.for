C
C SUBROUTINE READASF
C $Log:   GXAFXT:[GOLS]READASF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:38:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:26:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_asfsubs.for **
C
C
C
C     ========================================================
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE READASF(AGT,RECBUF,ST)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:ASFSUBS.DEF'
C
	INTEGER*4 RECBUF(64*ASFSEC), NDX, BLK, ST, AGT
C
	ST=0
	BLK=(AGT-1)/RECSPERBKT + 1
	NDX=MOD(AGT-1,RECSPERBKT) + 1
C
	IF(BLK.NE.BKTNUM)THEN
	  IF(BKTCHG.NE.0)THEN
	    CALL WRITEW(FDB,BKTNUM,BUCKET,ST)
	    IF(ST.NE.0)THEN
	      CALL FILERR(SFNAMES(1,ASF),3,ST,BKTNUM)
	      BKTNUM=-1
	      BKTCHG=0
	      RETURN
	    ENDIF
	  ENDIF
C
	  CALL READW(FDB,BLK,BUCKET,ST)
	  IF(ST.NE.0)THEN
	    CALL FILERR(SFNAMES(1,ASF),2,ST,BLK)
	    BKTNUM=-1
	    BKTCHG=0
	    RETURN
	  ENDIF
	  BKTNUM=BLK
	  BKTCHG=0                !NO CHANGE MADE YET
	ENDIF
C
	CALL FASTMOV(BUCKET(1,NDX),RECBUF,64*ASFSEC)
C
	RETURN
	END
