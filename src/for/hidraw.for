C
C SUBROUTINE HIDRAW
C $Log:   GXAFXT:[GOLS]HIDRAW.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:32:16   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   22 Feb 1994 18:16:36   HXK
C  INITIALISE HDRAW TABLE TO -1 DYNAMICALLY 
C  (I.E. ON EVERY CALL OF THIS SUBROUTINE)
C  
C     Rev 1.0   21 Jan 1993 16:35:14   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - datupd.for **
C
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE HIDRAW
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	COMMON SCFREC
	INTEGER*4 FDB(7),HDRAW(MAXGAM)
	INTEGER*4 I, REC, ST
C
C
        DO I=1,MAXGAM
           HDRAW(I)=-1
        ENDDO

	TYPE*,'Setting high draw numbers '
	CALL OPENW(1,SCFSFN(1,DAF),4,0,0,ST)
	CALL IOINIT(FDB,1,DAFSEC*256)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,DAF),1,ST,0)
	  RETURN
	ENDIF
	REC=0
C
C
10	CONTINUE
	REC=REC+1
	CALL READW(FDB,REC,DAFREC,ST)
	IF(ST.EQ.144.OR.DAFSTS.EQ.0) THEN
	  CALL CLOSEFIL(FDB)
	  RETURN
	ENDIF
C
C
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,DAF),2,ST,REC)
	  CALL CLOSEFIL(FDB)
	  RETURN
	ENDIF
C
C
	DO 20 I=1,MAXGAM
	IF(DAFDRW(I).GT.0) HDRAW(I)=DAFDRW(I)
	DAFHDR(I)=HDRAW(I)
20	CONTINUE
C
C
	CALL WRITEW(FDB,REC,DAFREC,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,DAF),2,ST,REC)
	  CALL CLOSEFIL(FDB)
	  RETURN
	ENDIF
	GOTO 10
	END
