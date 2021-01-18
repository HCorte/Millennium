C SUBROUTINE SENDOUT
C
C V04 26-NOV-2020 SCML NEW TERMINAL PROJECT - Added support for OLM
C V03 06-JAN-2011 FJG  MILLENNIUM MXSRV
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
 
        CALL OPSTXT(' SENDOUT.FOR ')
        IF (X2X_GAME_STATE.EQ.X2X_GAMES_UP) THEN
C         CALL X2ADDPRO(BUF)                        ! MXSRV
          CALL OPSTXT(' THE GAME IS UP ')
          IF (HPRO(PRCSRC,BUF).EQ.OLM_COM) THEN ! V05 - OLM
            CALL OPSTXT(' GOOD SEND to OLM_COM: OLM queue ')
            CALL QUETRA(OLM,BUF)                    ! V05 - OLM (rever se deve usar o ABL ou é QUETRA)
C V05          IF (HPRO(PRCSRC,BUF).EQ.MXS_COM) THEN     ! MXSRV            
          ELSEIF (HPRO(PRCSRC,BUF).EQ.MXS_COM) THEN     ! MXSRV
            CALL OPSTXT(' wrong its not MXS Channel ')
            CALL QUETRA(MXS,BUF)                    ! MXSRV
          ELSE                                      ! MXSRV
            CALL OPSTXT(' wrong its not X2X Channel ')
            CALL X2ADDPRO(BUF)                      ! MXSRV
          ENDIF                                     ! MXSRV
        ELSE
            CALL OPSTXT(' THE GAME IS NOT UP!!!!!!!! ')
            CALL RELBUF(BUF)
        ENDIF
 
	RETURN
	END
