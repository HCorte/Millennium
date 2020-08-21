C SUBROUTINE TWDSNP
C
C V07 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V06 09-MAR-1994 HXK CHANGED CRITERIA FOR NOT SHOWING TIME, DATE.
C V05 09-MAR-1994 HXK CHANGED REPRESENTATION OF MANUALLY SUPPRESSED BETS.
C V04 07-MAR-1994 JXP Corrected displato screen
C V03 05-MAR-1994 JXP Improved checking for displaying warning and closure.
C V02 04-MAR-1994 JXP Closed states now depend on the dynamic comparison
C                     of TSMXLI and The total liability amount for 
C                     that combination.
C V01 04-MAR-1994 HXK CHANGED CHECKING OF SUPPRESSION STATE.
C
C VIS_TWDSNP.FOR    TOTO-SELECT COMBINATION TOP 100 SNAPSHOT
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE TWDSNP(PAGE,GIND,ROW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
	INCLUDE 'INCLIB:HASHMEM.DEF'
C
	INTEGER*4 LAT, GNUM, I
	INTEGER*4 AMT, GIND
	INTEGER*4 J, K, ROW, LIN, OFFSET, NUMROW
	PARAMETER (LAT=100)
	INTEGER*4 INDTOP(LAT+1)
	INTEGER*4 AMTTOP(LAT+1)
	CHARACTER*29 BETLIN
	INTEGER*4 PAGE
	INTEGER*4 NEXT_REC, NEXT_BLOCK, PSTAT
        INTEGER*4  BEG_OFFSET
        INTEGER*4  END_OFFSET

        INTEGER*4  SEC_DAY/86400/  !seconds in a day
        INTEGER*4  TIME
        INTEGER*2  TDATE(12)
         

        CHARACTER*4 STATE,CLOSE/'clos'/,WARN/'warn'/
        CHARACTER*8 CLOSE_TIME

	SMODE=.FALSE.
C	IF(MLIM.LT.50.OR.MLIM.GT.90000) MLIM=5000

	PAGE=ROW/15+1
        IF (PAGE .LT. 1 .OR. PAGE .GT. 7) PAGE = 1
        BEG_OFFSET = 15*(PAGE-1)+1
        END_OFFSET = 15*(PAGE-1)+15
        IF (PAGE .EQ. 7) THEN
            BEG_OFFSET = 85
            END_OFFSET = 99
        END IF
	 
C
C  SCAN FOR THE TOP 120 TERMINALS AND SORT THEM
C
	CALL FASTSET(0,INDTOP,LAT+1)
	CALL FASTSET(0,AMTTOP,LAT+1)
	IF(GIND.LE.0.OR.GIND.GT.MAXIND) THEN
	  WRITE(CLIN23,3011) GIND
	  RETURN
	ENDIF
	GNUM=GTNTAB(TTSL,GIND)
	IF(GNUM.LT.1) THEN
	  WRITE(CLIN23,3010) GIND
	  RETURN
	ENDIF
	DO 20 I=1,100
	      AMT=(TSLWTOP(2,I,GIND)/16)
	      DO 30 J=1,LAT
	        IF(AMT.GT.AMTTOP(J)) GOTO 40
30	      CONTINUE
              GOTO 20
40	      CONTINUE
 	      DO 50 K=LAT+1,(J+1),-1
	        INDTOP(K)=INDTOP(K-1)
	        AMTTOP(K)=AMTTOP(K-1)
50	      CONTINUE
	      INDTOP(J)=I
	      AMTTOP(J)=AMT
20	CONTINUE
C
C   ENCODE THE HEADER OF THE TWD SNAPSHOT
C
	WRITE(CLIN1,9000)
	WRITE(CLIN2,9001) 
	WRITE(CLIN4,9002) CMONY(P(TSMXLI),12,BETUNIT)
	WRITE(CLIN5,9003)
C
C   ENCODE TOP 100 ROWS
C
C	IF(ROW.GE.(LAT-14)) ROW=LAT-14
	IF(ROW.LT.1)        ROW=BEG_OFFSET
	LIN=5
	DO 200 I=BEG_OFFSET,END_OFFSET
	  LIN=LIN+1
	  IF(AMTTOP(I).EQ.0) THEN
	     WRITE(XNEW(  LIN),9005) I
	  ELSE
		OFFSET=TSLWTOP(1,INDTOP(I),GIND)
		NUMROW=IAND(TSLWTOP(2,INDTOP(I),GIND),15)
		IF(OFFSET.EQ.0.OR.NUMROW.GT.6) THEN
		    WRITE(XNEW(  LIN),9005) I
		    GOTO 200
		ENDIF
		IF (NUMROW.LE.3) THEN
                    STATE = '    '
                    CLOSE_TIME = '        '
                    DO K = 9,11
                       TDATE(K) = '  '
                    ENDDO
                    IF(HASH_DIR_SUP(1,OFFSET).GT.(P(TSMXLI)/2)) STATE = WARN
                    IF(HASH_DIR_SUP(1,OFFSET).EQ.HASH_WARN) STATE=WARN
                    IF(HASH_DIR_SUP(1,OFFSET).GT.P(TSMXLI) .OR.
     *		       HASH_DIR_SUP(1,OFFSET).EQ.HASH_CLOSE) THEN
                       STATE = CLOSE
                       TIME = MOD(HASH_DIR_SUP(2,OFFSET),SEC_DAY)
                       WRITE(CLOSE_TIME,8000) DISTIM(TIME)
                       TDATE(5)=HASH_DIR_SUP(2,OFFSET)/SEC_DAY
                       CALL CDATE(TDATE)
                       IF(TDATE(5).EQ.0) THEN
                          CLOSE_TIME = '        '
                          DO K = 9,11
                             TDATE(K) = '  '
                          ENDDO
                       ENDIF
                    ENDIF
	  	    WRITE(XNEW(  LIN),9006) I,
     *		       BETLIN(OFFSET,NUMROW),
     *		       CMONY(HASH_DIR(OFFSET)*TSLPRC(GIND),12,BETUNIT),
     *		       CMONY(AMTTOP(I)*TSLPRC(GIND),12,BETUNIT),
     *                 STATE,(TDATE(K),K=9,11),CLOSE_TIME
		ELSE
                    STATE = '    '
                    CLOSE_TIME = '        '
                    DO K = 9,11
                       TDATE(K) = '  '
                    ENDDO
	            CALL HASHGET(OFFSET,NUMROW,AMT,NEXT_REC,NEXT_BLOCK,PSTAT)
		    IF(PSTAT.EQ.HASH_RETURN_OK) THEN
                      IF(HASH_TAB_SUP(1,NEXT_REC,NEXT_BLOCK).GT.(P(TSMXLI)/2)) 
     *                    STATE = WARN
                      IF(HASH_TAB_SUP(1,NEXT_REC,NEXT_BLOCK).EQ.HASH_WARN)
     *                    STATE = WARN
                      IF(HASH_TAB_SUP(1,NEXT_REC,NEXT_BLOCK).GT.P(TSMXLI) .OR.
     *		         HASH_TAB_SUP(1,NEXT_REC,NEXT_BLOCK).EQ.HASH_CLOSE) THEN
                         STATE = CLOSE
                         TIME = MOD(HASH_TAB_SUP(2,NEXT_REC,NEXT_BLOCK),SEC_DAY)
                         WRITE(CLOSE_TIME,8000) DISTIM(TIME)
                         TDATE(5)=HASH_TAB_SUP(2,NEXT_REC,NEXT_BLOCK)/SEC_DAY
                         CALL CDATE(TDATE)
                         IF(TDATE(5).EQ.0) THEN
                            CLOSE_TIME = '        '
                            DO K = 9,11
                               TDATE(K) = '  '
                            ENDDO
                         ENDIF
                      ENDIF
  		      WRITE(XNEW(  LIN),9006) I,
     *	                 BETLIN(OFFSET,NUMROW),
     *		         CMONY(HASH_TAB(2,NEXT_REC,NEXT_BLOCK)/16*TSLPRC(GIND),
     *						 12,BETUNIT),
     *		         CMONY(AMTTOP(I)*TSLPRC(GIND),12,BETUNIT),
     *                   STATE,(TDATE(K),K=9,11),CLOSE_TIME
		    ENDIF
		ENDIF
	  ENDIF
200	CONTINUE
C
3010	FORMAT('Select ',I1,' game not active')
3011	FORMAT('Invalid game index > ',I4)
8000    FORMAT(A8)
9000	FORMAT('TWDSNP snapshot')
9001	FORMAT(15X,'Today TOTO SELECT Sales/Agent Combination Watchdog')
9002	FORMAT(22X,'    Liability limit ',A12)
9003	FORMAT('   -COMBINATION----------------- -AMOUNT BET-',
     *	       ' --LIABILITY- STATE -DATE- --TIME--')
9004	FORMAT('Number of combinations played: ',I4,10X,
     *	       'Total amount bet today: ',A14)
9005	FORMAT(I2.2,'  --- --- ---  NOT INITIALIZED --- --- ---    ',
     *	            '            ')
9006	FORMAT(I2.2,1X,A29,1X,A12,1X,A12,2X,A4,1X,3A2,1X,A8)
C
	END
