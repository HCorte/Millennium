C SUBROUTINE TGL_CHKROW
C
C V01 02-DEC--2000 UXN INITIAL RELEASE FOR PORTUGAL.
C
C SUBROUTINE TO CHECK TOTOGOLO BOARDS
C	The following SYNTERRCODs are currently generated:
C	550	Row(s) with no marks bet for simple bets or full systems
C	551	Incorrect number of matches in the row
C	552	Incorrect System bet
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE TGL_CHKROW(TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TGLCOM.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'


        ! variables
	INTEGER*4  POWER2(0:16)        !
	INTEGER*4  POWER3(0:16)        !
	INTEGER*4  POWER4(0:16)        !
	INTEGER*4  MARKS(0:15)         !
	INTEGER*4  TAB(0:4,16)         !
	INTEGER*4  ROWS(2,TGGNBR,12)     !
	INTEGER*4  FULL                !
	INTEGER*4  SIMPLE              !
	INTEGER*4  ROWCNT              !
	INTEGER*4  CNT                 !
	INTEGER*4  I                   !
	INTEGER*4  J                   !
	INTEGER*4  SYS                 !
	INTEGER*4  GIND

C                  - 1 2 12 X 1X 12 1X2 - - - - - - - -
	DATA MARKS/0,1,1, 2,1, 2, 2,  3,1,2,2,3,2,3,3,4/
CV05	DATA MARKS/0,1,1, 2,1, 2, 2,  3,0,1,1,1,1,1,1,1/
	DATA POWER2/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
     *	            16384,32768,65536/
	DATA POWER3/1,3,9,27,81,243,729,2187,6561,19683,59049,
     *	            177147,531441,1594323,4782969,14348907,43046721/
	DATA POWER4/1,4,16,64,256,1024,4096,16384,65536,262144,1048576,
     *              4194304,16777216,67108864,268435456,1073741824,0/
C
C First check the validity in general
C
	GIND = TRABUF(TGAMIND)
	IF (TRABUF(TWSRW).NE.TGLMAX(GIND)) THEN
                TRABUF(TERR)=SYNT
                SYNTERRCOD=551
                RETURN
	ENDIF
C
	SYS = TRABUF(TWSYSN)
	CALL TGL_GETROW(TRABUF,ROWS)
	CALL FASTSET(0,TAB,5*16)
C
C FULL SYSTEM AND SIMPLE WAGERS
C
	DO J = 1, TRABUF(TWNBET)
	    DO I = 1, TRABUF(TWSRW)
	        CNT        = MARKS(ROWS(1,I,J))
	        TAB(CNT,J) = TAB(CNT,J)+1
	        CNT        = MARKS(ROWS(2,I,J))
	        TAB(CNT,J) = TAB(CNT,J)+1
            END DO
        END DO
C
C CALCULATE TOTAL NUMBER OF ROWS
C
	ROWCNT = 0
	SIMPLE = 0
	FULL   = 0
	DO I = 1, TRABUF(TWNBET)
	    IF(TAB(0,I).NE.0) THEN
	        TRABUF(TERR)=SYNT
	        SYNTERRCOD=550
	        RETURN
	    ENDIF

	    IF(TAB(1,I) .EQ. 2*TRABUF(TWSRW)) THEN
	        ROWCNT = ROWCNT+1
	        SIMPLE = SIMPLE+1
	    ELSE
	        ROWCNT = ROWCNT + POWER2(TAB(2,I)) * 
     *                            POWER3(TAB(3,I)) * POWER4(TAB(4,I))
	        FULL = FULL+1
	    ENDIF
C
C CHECK FOR VALID FULL SYSTEM BET
C
	    IF(TRABUF(TWSYST).EQ.FULSYS) THEN
  	        IF(TGSFSF(TAB(2,I),TAB(3,I),TAB(4,I),GIND).NE.1) THEN
	           TRABUF(TERR)=SYNT
	           SYNTERRCOD=552
	           RETURN
                ENDIF
	    ENDIF
C
        END DO

	TRABUF(TWSIMP)=ROWCNT
	IF(TRABUF(TWSYST).EQ.NOSYS.AND.FULL.NE.0) THEN
	    TRABUF(TERR)=SYNT
	    SYNTERRCOD=555
	    RETURN
	ENDIF
	IF(TRABUF(TWSYST).EQ.FULSYS.AND.FULL.EQ.0) TRABUF(TWSYST)=NOSYS

	RETURN

	END
