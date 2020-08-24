C
C SUBROUTINE TRP_CANROW
C $Log:   GXAFXT:[GOLS]TRP_CANROW.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:43:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   23 Nov 1995 14:47:00   PXB
C  Initial revision.
C  
C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE TRP_CANROW(EVENT,ROW,TRROWSTS)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'

C---- Local variables.

	INTEGER*4 TRROWSTS(MAXTRPRW,3)
	INTEGER*4 EVENT, ROW, K, FLAG


C----------------------- Start of code ------------------------------

C---- Check to see if event is cancelled.

	IF (TRROWSTS(ROW,EVENT) .EQ. GAMCAN) THEN
	   WRITE(5,901) IAM(),ROW,
     *                  (DTRNMS(K,ROW,EVENT),K=1,TRPNMS_LEN/4)
	   RETURN
	END IF

C---- Check if row active.

	IF(TRROWSTS(ROW,EVENT).EQ.GAMNUL) THEN
	   TYPE*,IAM(),' Row ',ROW,' not active for this event'
	   RETURN
	ENDIF

C---- Cancel row.

	WRITE(5,902) IAM(),ROW,(DTRNMS(K,ROW,EVENT),K=1,TRPNMS_LEN/4)
	CALL WIMG(5,'Is this correct (Y/N) ')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) RETURN
	TRROWSTS(ROW,EVENT)=GAMCAN
	RETURN


C------------------------------ Format statements ------------------------

901	FORMAT(1X,A,' Row ',I2,1X,<TRPNMS_LEN/4>A4,' Already cancelled')
902	FORMAT(1X,A,' Cancellation of row ',I2,1X,<TRPNMS_LEN/4>A4)

	END
