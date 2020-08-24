C
C SUBROUTINE PRINTRA_LTBET
C $Log:   GXAFXT:[GOLS]PRINTRA_LTBET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:29:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   12 Jul 1993 15:23:26   SXH
C  Released for Finland
C  
C     Rev 1.0   21 Jan 1993 17:21:00   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - printra.for **
C
C V02 07-DEC-2010 MAC LUCKY NUMBER
C V01 07-JAN-93 MODIFIED CALL OF SUBROUTINE TO PRINTRA_LTBET TO DIFFERNETIATE
C		THIS LTBET FROM THE VISION LTBET ROUTINE.  THE PRINTRA SUB
C		IS THE ONLY ROUTINE THAT CALLS THIS SUBROUTINE
C
C BUILD BET IMAGE FOR LOTTO TRANSACTIONS
C ==============================================================
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PRINTRA_LTBET(TRABUF,CBETS,LINES)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
        ! arguments
        INTEGER*4       LINES             !
	CHARACTER*80	CBETS(14)         !

        ! variables

	INTEGER*4  K                      !
	INTEGER*4  I                      !
	INTEGER*4  ST                     !
	INTEGER*4  MARKS                  !
	INTEGER*4  FLAGS(32)              !
	INTEGER*4  BDS(24)                !
	INTEGER*4  QP(2)
                                          
	INTEGER*2  BOARD(4,12)            !

	CHARACTER * 1 PLAYMONDAY	

	DATA QP/'    ','QP  '/

C
	CALL GETFLG(TRABUF(TWQPF),FLAGS,TRABUF(TWNBET))

	CALL SINTMAP(TRABUF(TWBORD),1,TRABUF(TWNBET),TRABUF(TWNMRK),
     *	             64,BOARD,ST)

	PLAYMONDAY = 'N'
        IF(TRABUF(TWLMFI) .EQ. 1) PLAYMONDAY = 'Y'

	IF(ST.EQ.0) THEN
	    DO I = 1, TRABUF(TWNBET)
	        MARKS = TRABUF(TWNMRK+I-1)
	        CALL UNMAP(BOARD(1,I),8,BDS,MARKS)
	        IF(MARKS.GT.24) MARKS=24

                ! DISPLAY IF PLAY LOTTO 2 ONLY IN LOTTO 1
                IF(I .EQ. 1 .AND. TRABUF(TGAMIND) .EQ. 1) THEN 
	          WRITE (CBETS(I),901) 
     *                   PLAYMONDAY,
     *                   QP(FLAGS(I)+1),
     *                   (BDS(K),K=1,MARKS)
                ELSE
	          WRITE (CBETS(I),902) QP(FLAGS(I)+1),(BDS(K),K=1,MARKS)
                ENDIF
            END DO
	    LINES=TRABUF(TWNBET)+1
            IF (TRABUF(TWLUCK).GT.0) THEN       !V02...
              WRITE (CBETS(I),903) TRABUF(TWLUCK)
              LINES=LINES+1
            ENDIF                               !...V02
	ENDIF
C
	RETURN

901	FORMAT(X, 'PLAY LOTTO 2:',X, A1, 3X,A4,24(I2.2,1X))
902	FORMAT(19X, A4, 24(I2.2,1X))
903     FORMAT(1X,'Lucky Number ',I2)          !V02
	END
