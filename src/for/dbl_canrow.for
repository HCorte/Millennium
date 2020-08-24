C
C SUBROUTINE DBL_CANROW
C $Log:   GXAFXT:[GOLS]DBL_CANROW.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:48:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   23 Nov 1995 14:48:14   PXB
C  Initial revision.
C  
C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE DBL_CANROW(ROW,N)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'

C---- Local variables.

	INTEGER*4 ROW, K, FLAG
	INTEGER*4 N		! N=1 IF DBLENT IS CALLING ROUTINE
				! N=2 IF DBLVER IS CALLING ROUTINE
			        ! NOTE THAT THIS IS ALSO USED AS AN INDEX
				! INTO THE DBROWSTS ARRAY



C----------------------- Start of code ------------------------------

C---- Chack to see if event is cancelled.

	IF (N .EQ. 1) THEN
	  IF (DBROWSTS(ROW,N) .EQ. GAMCAN .OR. 
     *	      DBROWSTS(ROW,N) .EQ. GAMREF) THEN
	    WRITE(5,901) IAM(),ROW,(DDBNMS(K,ROW),K=1,3)
	    RETURN
	  END IF
	ELSE IF (N .EQ. 2) THEN
	  IF (DBROWSTS(ROW,N) .EQ. GAMCAN) THEN
	    WRITE(5,901) IAM(),ROW,(DDBNMS(K,ROW),K=1,3)
	    RETURN
	  END IF
	END IF

C---- Check if row active.

	IF(DBROWSTS(ROW,N).EQ.GAMNUL) THEN
	  TYPE*,IAM(),' Row ',ROW,' not active for this event'
	  RETURN
	ENDIF

C---- Cancel row.

	WRITE(5,902) IAM(),ROW,(DDBNMS(K,ROW),K=1,3)

	CALL WIMG(5,'Is this correct (Y/N) ')

	CALL YESNO(FLAG)

	IF(FLAG.NE.1) RETURN

	DBROWSTS(ROW,N)=GAMCAN


C------------------------------ Format statements ------------------------

901	FORMAT(1X,A,' Row ',I2,1X,3A4,' Already cancelled')

902	FORMAT(1X,A,' Cancellation of row ',I4,1X,3A4)

	RETURN
	END
