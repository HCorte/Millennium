C SPTEAMS.FOR
C 
C SUBROUTINE SPTEAMS
C
C V01 30-MAR-2015 MTK Modified Super 14 game
C V00 26-AUG-2013 FJG  CR15 SPORTS NEW TEAMS NAME REPORTS
C
C CALLING SEQUENCE:
C     CALL SPTEAMS(TRABUF,MESTAB,OUTLEN)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT MESSAGE LENGTH
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
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SPTEAMS(TRABUF,MESTAB,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'              
C
        INTEGER*2   OUTLEN         ! MESSAGE LENGTH
        INTEGER*1   MESTAB(*)      ! MESSAGE OUTPUT
        INTEGER*4   MESS(EDLEN)    ! INTERNAL MESSAGE
        INTEGER*4   ERRT           ! ERRTYP
        INTEGER*4   CHKL           ! CHKLEN MESSGE
        INTEGER*4   CHKC           ! CHECKSUM
        INTEGER*4   ERRN           ! I/O ERROR NUMBER
        INTEGER*4   WEEK           ! WEEK
        INTEGER*4   YEAR           ! YEAR
        INTEGER*4   DRAW           ! INTERNAL DRAW
        INTEGER*4   GNUM           ! GAME NUMBER
        INTEGER*4   GTYP           ! GAME TYPE
        INTEGER*4   GIND           ! GAME INDEX
        INTEGER*4   SEGN           ! SEGMENT NUMBER
        INTEGER*4   OFFS           ! MESSAGE OFFSET
        INTEGER*4   SIND           ! TEMP SEGMENT INDEX
        INTEGER*4   MOFF           ! MINIMUM OFFSET
        INTEGER*4   STMP           ! TEMP SEGMENT IN PLACE
        INTEGER*4   XTWO           ! LOOP INDEX
        INTEGER*4   XONE           ! LOOP INDEX
        INTEGER*4   XTMP           ! TEMP
        INTEGER*4   XLEN           ! NEXT FIELD LENGTH
        INTEGER*4   LAST_DRAW      ! Last Draw Requested
	INTEGER*4   BCNT           ! BONUS ROW COUNT
        CHARACTER*4 RESN(5)        ! NORMAL RESULTS
        CHARACTER*1 RESS(5)        ! SUPER RESULTS
C+++++++FUNCTIONS
        INTEGER*4   GETDRW        
C+++++++WORKING WITH INTEGERS
        INTEGER*1   I1TEMP(4)
        INTEGER*2   I2TEMP(2)
        INTEGER*4   I4TEMP
        CHARACTER*4 C4TEMP
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP,C4TEMP)        
C
        DATA ERRT/Z90/
        DATA LAST_DRAW/0/
        DATA RESN/' -  ',' 1  ',' X  ',' C  ',' 2  '/
        DATA RESS/'-','0','1','C','M'/
C
C       SET / CLEAR VARIABLES
C
D	TYPE*,IAM(),'>>> SPTEAMS'
        ERRN = 0
        WEEK = 0
        YEAR = 0
	DRAW = 0
C
C       GET VALUES
C
        GTYP = ZEXT(MESTAB(5))
        GIND = ZEXT(MESTAB(6))
        SEGN = ZEXT(MESTAB(7))
        WEEK = ZEXT(MESTAB(8))
        YEAR = ZEXT(MESTAB(9))
D	TYPE*,IAM(),'>>> GTYP/GIND:',GTYP,GIND        
D	TYPE*,IAM(),'>>> WEEK/YEAR:',WEEK,YEAR        
C
        TRABUF(TSDT1)=GTYP
        TRABUF(TSDT2)=GIND
C
C       CHECK RANGE OF GAME TYPE, GAME INDEX AND GAME
C
        IF(GTYP.NE.TSPT) THEN
          TRABUF(TERR)=SYNT
          GOTO 9999
        ENDIF
        IF(GIND.LT.1.OR.GIND.GT.MAXIND) THEN
         TRABUF(TERR)=SYNT
         GOTO 9999
        ENDIF
        GNUM = GTNTAB(GTYP,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
          TRABUF(TERR)=SYNT
          GOTO 9999
        ENDIF
        TRABUF(TSDT3)=GNUM
D	TYPE*,IAM(),'>>> GNUM:',GNUM        
C
C       CHECK MULTISEGMENT
C
	IF(SEGN.LT.0.OR.SEGN.GT.20) THEN
	   TRABUF(TERR) = SYNT
           GOTO 9999 
	ENDIF     
D	TYPE*,IAM(),'>>> SEGN:',SEGN	   
C
C       CHECK DATE
C
        IF(WEEK.NE.0.OR.YEAR.NE.0) THEN
          IF(WEEK.LE.0.OR.YEAR.LE.0) THEN
            TRABUF(TERR) = INVL
            GOTO 9999
          ENDIF
          DRAW = GETDRW (YEAR, WEEK, GNUM)
        ENDIF
        IF(DRAW.EQ.0) THEN
          IF(DAYDRW(GNUM).GT.1) THEN
            DRAW = DAYDRW(GNUM) - 1
          ELSE
            DRAW = DAYHDR(GNUM)
          ENDIF
        ENDIF
D	TYPE*,IAM(),'>>> DRAW:',DRAW       
C
C       CHECK IF SUPRESS REPORTS 
C
        IF(TSBIT(P(SUPRPT),GAMREP)) THEN
          TRABUF(TERR) = SUPR
          GOTO 9999
        ENDIF
        IF(TSBIT(P(SUPRPT),GNUM)) THEN
          TRABUF(TERR) = SUPR
          GOTO 9999
        ENDIF
C
        IF(DRAW.GT.DAYHDR(GNUM).AND.DAYHDR(GNUM).NE.0) THEN
          TRABUF(TERR) = RNIN
          GOTO 9999
        ENDIF
C
        IF(DAYHDR(GNUM).NE.0.AND.DRAW.EQ.0) THEN
          TRABUF(TERR) = RNIN
          GOTO 9999
        ENDIF
C
        IF(DRAW.LT.1) THEN
          TRABUF(TERR) = INVL
          GOTO 9999
        ENDIF
C
        IF(DRAW.EQ.DAYDRW(GNUM)) THEN
          TRABUF(TERR) = RNIN
          GOTO 9999
        ENDIF        
C
C       READ GAME FILE FOR REQUESTED DRAW
C
        IF(P(SUPFIL).EQ.1) THEN
          TRABUF(TERR) = SUPR
          GOTO 9999
        ENDIF
C
C CHECK IF DRAW IS ALREADY READ
C
        IF(DRAW.EQ.LAST_DRAW) GOTO 60
C
C CHECK IF FILE IS OPEN (TO PREVENT CRASH)
C
        IF(DAYHDR(GNUM).LT.1) THEN
           TRABUF(TERR) = INVL
           GOTO 9999
        ENDIF
C
        CALL READW(GAMFDB(1,GNUM),DRAW,DSPREC,ERRN)
C
C       CHECK READ STATUS AND REPORT ANY ERROR TO THE CONSOLE
C
D	TYPE*,IAM(),'>>> ERRN:',ERRN            
        IF(ERRN.NE.0) THEN
           MESS(1)=SPE
           MESS(2)=TEGEN
           MESS(3)=4
           CALL FASTMOV(SFNAMES(1,GNUM),MESS(4),5)
           MESS(9)=ERRN
           MESS(10)=DRAW
           CALL QUEMES(MESS)
           TRABUF(TERR)=INVL
           GOTO 9999
        ENDIF  
D	TYPE*,IAM(),'>>> READ'
C
C SAVE DRAW READ AS LAST DRAW READ
C
60      CONTINUE
        LAST_DRAW = DRAW
C
C       BUILD SPORT 1X2 GAME TYPE RESULTS REPORT
C
        IF(DSPSTS.LT.GAMENV) THEN
           TRABUF(TERR) = RNIN
           GOTO 9999
        ENDIF
D	TYPE*,IAM(),'>>> BUILD'
C
        OFFS = 8
C
C       CDC DATE  ! PTLA-637
C
       I4TEMP = DSPDAT(CURDRW)
       MESTAB(OFFS+0) = I1TEMP(2)
       MESTAB(OFFS+1) = I1TEMP(1)
       OFFS=OFFS+2
C
C       WEEK NUMBER
C
C       CDC = LSPESD(GIDX)
C       CALL FIGWEK(CDC,WEEK,YEAR2)
C       MESTAB(IND+0) = MOD(YEAR2, 100)
C       MESTAB(IND+1) = WEEK
C       IND = IND+2
C
C       START BUILDING MESSAGE
C
        STMP = 1
        MESTAB(OFFS) = DSPMAX
        OFFS = OFFS+1
        SIND = OFFS
        MOFF = OFFS
D	TYPE*,IAM(),'>>> (1) OFFS/STMP/SIND',OFFS,STMP,SIND
C
C       SPORTS TEAM NAMES AND WINNING NUMBERS
C
        DO XTWO=1,DSPMAX
          DO XONE=1,2
C+++++++++++TEAM NAMES
            XLEN = SPNMS_LEN
            SIND = SIND+XLEN
            IF(SIND.GT.256) THEN
              STMP = STMP+1
              SIND = MOFF+XLEN
            ENDIF            
            IF(SEGN.EQ.STMP) THEN
              CALL MOVBYT(DSPNMS(1,XONE,XTWO),1,MESTAB,OFFS,XLEN)
              OFFS = OFFS+XLEN
            ENDIF
          ENDDO
D	  TYPE*,IAM(),'>>> (2) OFFS/STMP/SIND',OFFS,STMP,SIND
C+++++++++WINNING NUMBERS
          XLEN = 3
          SIND = SIND+XLEN
          IF(SIND.GT.256) THEN
            STMP = STMP+1
            SIND = MOFF+XLEN
          ENDIF
          IF(SEGN.EQ.STMP) THEN

            BCNT = 0
            IF(DSPFRG.NE.0) BCNT = 1

            IF(XTWO.LE.DSPMAX-BCNT) THEN
              C4TEMP = RESN(DSPWIN(XTWO)+1)
            ELSE
	      IF(DSPFRG.EQ.1) THEN
                WRITE(C4TEMP,900) RESS((ISHFT(DSPWIN(XTWO),-4))+1),RESS(IAND(DSPWIN(XTWO),'0F'X)+1)
	      ENDIF
	      IF(DSPFRG.EQ.2) THEN
	         C4TEMP = RESN(DSPWIN(XTWO)+1)
	      ENDIF
            ENDIF
            MESTAB(OFFS+0) = I1TEMP(1) 
            MESTAB(OFFS+1) = I1TEMP(2)
            MESTAB(OFFS+2) = I1TEMP(3)  
            OFFS = OFFS+XLEN
          ENDIF
D	  TYPE*,IAM(),'>>> (3) OFFS/STMP/SIND',OFFS,STMP,SIND
        ENDDO
C
C       SET SHARE COUNT AND AMOUNT
C
C+++++++NUMBER OF DIVISIONS
        XLEN = 1
        SIND = SIND+XLEN
        IF(SIND.GT.256) THEN
          STMP = STMP+1
          SIND = MOFF+XLEN
        ENDIF
        IF(SEGN.EQ.STMP) THEN
          MESTAB(OFFS) = DSPDIV
          OFFS = OFFS+XLEN
        ENDIF        
D	TYPE*,IAM(),'>>> (4) OFFS/STMP/SIND',OFFS,STMP,SIND
C        
D	TYPE*,IAM(),'>>> DSPDIV',DSPDIV
        DO XTMP = 1,DSPDIV
          IF(DSPMAT(XTMP).NE.0) THEN
C+++++++++++SHARE COUNTS            
            XLEN = 4
            SIND = SIND+XLEN
            IF(SIND.GT.256) THEN
              STMP = STMP+1
              SIND = MOFF+XLEN
            ENDIF      
            IF(SEGN.EQ.STMP) THEN                  
              I4TEMP = DSPTSR(XTMP)
              MESTAB(OFFS+0) = I1TEMP(4)
              MESTAB(OFFS+1) = I1TEMP(3)
              MESTAB(OFFS+2) = I1TEMP(2)
              MESTAB(OFFS+3) = I1TEMP(1)
              OFFS=OFFS+4
            ENDIF
D	    TYPE*,IAM(),'>>> (5) OFFS/STMP/SIND',OFFS,STMP,SIND            
C+++++++++++SHARE VALUES
            XLEN = 4
            SIND = SIND+XLEN
            IF(SIND.GT.256) THEN
              STMP = STMP+1
              SIND = MOFF+XLEN
            ENDIF      
            IF(SEGN.EQ.STMP) THEN                             
              I4TEMP = DSPSHV(XTMP)
              MESTAB(OFFS+0) = I1TEMP(4)
              MESTAB(OFFS+1) = I1TEMP(3)
              MESTAB(OFFS+2) = I1TEMP(2)
              MESTAB(OFFS+3) = I1TEMP(1)
              OFFS=OFFS+4
            ENDIF
D	    TYPE*,IAM(),'>>> (6) OFFS/STMP/SIND',OFFS,STMP,SIND            
          ENDIF
        ENDDO
C
C       WEEK AND YEAR OF CONCURSO
C
        XLEN = 2
        SIND = SIND+XLEN
        IF(SIND.GT.256) THEN
          STMP = STMP+1
          SIND = MOFF+XLEN
        ENDIF      
        IF(SEGN.EQ.STMP) THEN                             
          CALL FIGWEK(DSPBSD,WEEK,YEAR)
          MESTAB(OFFS+0) = WEEK
          MESTAB(OFFS+1) = MOD(YEAR, 100)
          OFFS=OFFS+2
        ENDIF
D       TYPE*,IAM(),'>>> (7) OFFS/STMP/SIND',OFFS,STMP,SIND            
C
        IF(SEGN.EQ.STMP) MESTAB(7) = 0             ! SEGMENT NUMBER TO ZERO IF LAST SEGMENT
        OUTLEN = OFFS - 1
D	TYPE*,IAM(),'>>> (8) OFFS/STMP/SIND',OFFS,STMP,SIND
D	TYPE*,IAM(),'>>> OUTLEN',OUTLEN
C        
        GOTO 10000
C
C       ERROR IN REPORT REQUEST FROM TERMINAL
C
9999    CONTINUE
        TRABUF(TSTAT) = REJT
        MESTAB(2) = ERRT
        MESTAB(5) = TRABUF(TERR)
        OUTLEN = 6
D	TYPE*,IAM(),'>>> ERROR'
C
C       CALCULATE CHECKSUM FOR MESSAGE BACK TO TERMINAL
C
10000   CONTINUE
        I4CCITT = TRABUF(TCHK)
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
        CHKL = OUTLEN - 1
        CALL GETCCITT(MESTAB,1,CHKL,CHKC)
        I4CCITT = CHKC
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
D	TYPE*,IAM(),'>>> OUT TSTAT/TERR:',TRABUF(TSTAT),TRABUF(TERR)       
C
        RETURN
C===============================================================================
C       FORMATS
C===============================================================================
900     FORMAT(A1,':',A1,1X)      
        END
