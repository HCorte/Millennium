C
C SUBROUTINE POST
C $Log:   GXAFXT:[GOLS]POST.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:27:06   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:20:30   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - tslent.for **
C
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE POST(ROW,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INTEGER*2 DATE(LDATE_LEN)
	INTEGER*4 ROW, ST, K, CDC, FLAG, EXT

C
C
	ST=-1
	WRITE(5,900) IAM(),ROW,(DTSNMS(K,1,ROW),K=1,3),
     *	                 (DTSNMS(K,2,ROW),K=1,3)
	CALL WIMG(5,'Is this correct [Y/N] ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) RETURN
C
C
	DATE(5)=DTSDTE
	CALL LCDATE(DATE)
10	CONTINUE
	WRITE(5,901) IAM(),(DATE(K),K=7,13)
	TYPE*,IAM(),' Enter new date for this row '
	CALL INPDAT(CDC,EXT)
	IF(EXT.LT.0) RETURN
	CALL WIMG(5,'Is this correct [Y/N] ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) GOTO 10
C
C
	IF(CDC.LT.DAYCDC.OR.CDC.GT.DTSDTE) THEN
	  TYPE*,IAM(),' Invalid date entered'
	  GOTO 10
	ENDIF
C
C
	TROWDAT(ROW,1)=CDC
	ST=0
	RETURN
C
C
900	FORMAT(1X,A,' Postponing row ',I4,1X,3A4,' vs ',3A4)
901	FORMAT(1X,A,' Last valid date for this match is ',7A2)
	END
