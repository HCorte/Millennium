C
C SUBROUTINE RESET_STARTCOM
C
C V02 30-JAN-98 UXN Added WAIT_FOREVER logic.
C  
C     Rev 1.0   17 Apr 1996 14:44:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:30:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C   
C      Rev 1.0   20 Jan 1993 15:28:50   EBD
C      changed call of reset's startcom to reset_startcom to signify 
C      special function used only by reset task
C
C ** Source - reset.for **
C
C
C
C SUBROUTINE TO START COMMUNICATIONS TASK(S)
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE RESET_STARTCOM
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4  MAX_WAIT	
	PARAMETER (MAX_WAIT=50) !WAIT UP TO MAX_WAIT*200 MSECS FOR X2X
C
	LOGICAL	   WAIT_FOREVER/.TRUE./ ! Wait until LANPRO is up.
C
        INTEGER*4  FLAG, TO_WAIT, ST
C
C
	TO_WAIT=MAX_WAIT
40	CONTINUE
	IF (LANGO.NE.LANPROUP) THEN
	 CALL XWAIT(200,1,ST)
	 TO_WAIT=TO_WAIT-1
	 IF (TO_WAIT.LE.0) THEN
	   IF(WAIT_FOREVER) THEN
	      TO_WAIT = MAX_WAIT
	      CALL WIMG(5,'LANPRO is not up, waiting ....')
	      GOTO 40
	   ENDIF	      
	   CALL WIMG(5,'LANPRO is not up, do you want to disable LAN')
	   CALL YESNO(FLAG)
	   IF (FLAG.EQ.1) THEN
	      LANGO=LANPRODIS
	      GOTO 60
	   ENDIF
	   TO_WAIT=MAX_WAIT
	 ENDIF
	 GOTO 40
	ELSE
         TYPE *
	 TYPE *,IAM(),'LANPRO is up now...'
         TYPE *
         TYPE *,'======================================================='
         TYPE *,IAM(),'If you wish to enable X2X communications do the'
         TYPE *,'following:'
         TYPE *,'1. Bring up VISION on PRIMARY system'
         TYPE *,'2. Goto the X2GBLSNP snapshot (GAMESTAT should be idle)'
         TYPE *,'3. Enter "GAMST 2" to request X2XMGR to come online    '
         TYPE *,'4. Watch GAME STATE change to "req X2XMGR" to "onlin" '
         TYPE *,'======================================================='
	 TYPE *
C***   IF (X2X_GAME_STATE.NE.X2X_GAMES_IDLE) THEN
C***    CALL WAIT(200,1,ST)
C***    TO_WAIT=TO_WAIT-1
C***    IF (TO_WAIT.LE.0) THEN
C***      CALL WIMG(5,'X2XPRO is not up, do you want to disable LAN')
C***      CALL YESNO(FLAG)
C***      IF (FLAG.EQ.1) THEN
C***        LANGO=LANPRODIS
C***        GOTO 60
C***      ENDIF
C***      TO_WAIT=MAX_WAIT
C***    ENDIF
C***    GOTO 40
C***   ENDIF
	ENDIF
C***  X2X_GAME_STATE=X2X_GAMES_REQUP
60	CONTINUE
	RETURN
	END
