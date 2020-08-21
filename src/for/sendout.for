C SUBROUTINE SENDOUT
C
C V03 06-JAN-2011 FJG MILLENNIUM MXSRV
C
C $Log:   GXAFXT:[GOLS]SENDOUT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:54:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:35:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - encpro.for **
C
C
C****************************************************************
C
C	SENDOUT(BUF)	QUEUE BUFFER TO X2X, IF X2X IS NOT
C			UP RELEASE THE BUFFER
C
C	IN:
C	BUF -	BUFFER NO
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SENDOUT(BUF)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'     ! MXSRV
        INCLUDE 'INCLIB:TASKID.DEF'     ! MXSRV
C   
	INTEGER*4 BUF
 
        IF (X2X_GAME_STATE.EQ.X2X_GAMES_UP) THEN
C         CALL X2ADDPRO(BUF)                        ! MXSRV
          IF (HPRO(PRCSRC,BUF).EQ.MXS_COM) THEN     ! MXSRV
            CALL QUETRA(MXS,BUF)                    ! MXSRV
          ELSE                                      ! MXSRV
            CALL X2ADDPRO(BUF)                      ! MXSRV
          ENDIF                                     ! MXSRV
        ELSE
            CALL RELBUF(BUF)
        ENDIF
 
	RETURN
	END
