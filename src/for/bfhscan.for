C SUBROUTINE BFHSCAN.FOR
C
C V03 02-MAR-2000 RXK Bounds for scan increased, correction tables dropped 
C                     (for final results used now BFHSCANF).
C V02 01-FEB-2000 RXK Changed for new Bingo rules.
C V01        1997 
C  
C BINGO SCAN TO CREATE TABLES OF NUMBER OF WINNERS ON RANGE OF NUMBERS DRAWN 
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
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE BFHSCAN(TRABUF,BMSCAN,BWSCAN,BW2SCAN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
C
        INTEGER*4   BMSCAN(BGOPHS,BGODIV,BGONBR)!# of divs won on ball N
        INTEGER*4   BWSCAN(BGOPHS,BGONBR)       !# worst match on ball N
        INTEGER*4   BW2SCAN(BGOPHS,BGONBR)      !# second worst match on ball N

        INTEGER*4   BET_WRD
        PARAMETER   (BET_WRD = (BGONBR/32) + 1) !# Words for a bet.
        INTEGER*4   DIV, GIND, ST, I, J, B
        INTEGER*4   BNG, MAXBNG
        INTEGER*4   NUM                         !# of #'s Loop Variable
        INTEGER*4   BOARDS(BGONBB,BGOCOL,BGOROW)!Bet in col,row format.
        INTEGER*4   BET(BET_WRD,BGONBB,BGOMAXMAP)!Bet in bitmap format
        INTEGER*4   PHS                         !Phases Loop Variable
        INTEGER*4   MAP                         !Bitmaps Loop Variable
        INTEGER*4   ROW                         !Row Loop Variable/ Index
        INTEGER*4   COL                         !Column Looop Var/ Index
        INTEGER*4   WRD                         !Word Loop Variable
        INTEGER*4   TEMP(BET_WRD)               !Temp for Bet
        INTEGER*4   COUNT                       !# Bits set Counter
        INTEGER*4   MATCHED_BITMAPS             !#of bitmaps for cutoff
        INTEGER*4   MATCHED_NUMBERS             !#of numbers for cutoff
        INTEGER*4   ACTMAP                      !# of actually defined maps
        INTEGER*4   BEGNUM1                     !scale for phase1
        INTEGER*4   ENDNUM1                     ! ""
        INTEGER*4   BEGNUM2                     !scale for phase2
        INTEGER*4   ENDNUM2                     ! ""

        LOGICAL     FIRST /.TRUE./
C
        GIND=TRABUF(TGAMIND)
C
C FIND MAXIMUM NUMBER OF BINGO MATCHES USED (i.e. 1,2,3 OR 4)
C SET RANGE OF NUMBERS TO BE SCANNED
C
        IF(FIRST) THEN
           DO I=1,8
              IF(LBNFST(I,BGOFHS,GIND).NE.0.OR.
     *           LBNOTH(I,BGOFHS,GIND).NE.0)
     *           MAXBNG=(I-1)/2+1
           ENDDO
           DO I=1,BGOMAXMAP
              IF(LBNMAP(I,BGOFHS,GIND).NE.0) ACTMAP=I 
           ENDDO

           BEGNUM1 = 5
           ENDNUM1 = 50
           BEGNUM2 = 40
           ENDNUM2 = 55

           FIRST=.FALSE.
        ENDIF
C
        IF(LBNDRW(GIND).LT.0) RETURN
C
        IF(TRABUF(TWBEG).GT.LBNDRW(GIND)) RETURN
        IF(TRABUF(TWEND).LT.LBNDRW(GIND)) RETURN
        IF(TRABUF(TTYP).NE.TWAG) RETURN
        IF(TRABUF(TSTAT).NE.GOOD) RETURN
C
C CONVERT BOARD DATA
C
        CALL GETB_BTMBRD(TRABUF,BOARDS)
        CALL FASTSET(0,BET,BET_WRD*BGONBB*BGOMAXMAP)

        DO B=1,BGONBB
           DO MAP = 1,ACTMAP
              DO ROW = 1,BGOROW
                 DO COL = 1,BGOCOL
                   IF(BTEST(LBNMAP(MAP,BGOFHS,GIND),(COL-1)*BGOROW+ROW))
     *                CALL BSET(BET(1,B,MAP),BOARDS(B,COL,ROW))
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
C
C PHASE 1. CHECK BET BITMAPS AGAINST WINNING BITMAPS
C
        PHS = 1
        MATCHED_BITMAPS = LBNPMT(1,PHS,GIND)

        IF(MATCHED_BITMAPS.NE.0) THEN
           DO 70 B=1,BGONBB                     
 
              DO 60 NUM=BEGNUM1,ENDNUM1
C
C FIND BINGO, DOUBLE, TRIPLE AND QUADRUPLE MATCHES
C
                 BNG = 0
                 DO 40 MAP = 1,ACTMAP
                    DO WRD = 1,BET_WRD
                       TEMP(WRD) =
     *                    IAND(BET(WRD,B,MAP),MATCHTAB(WRD,NUM))
                       IF(TEMP(WRD).NE.BET(WRD,B,MAP)) GOTO 40
                     ENDDO
                     BNG=BNG+1
40               CONTINUE
C
                 IF(BNG.GT.0) THEN
                    BNG=MIN(BNG,MAXBNG)   ! do not give higher div than set
                    DIV=2*BNG-1
                    BMSCAN(PHS,DIV,NUM)=BMSCAN(PHS,DIV,NUM)+1
                 ENDIF
                 
60            CONTINUE
70         CONTINUE
        ENDIF
C
C PHASE 2. CHECK BET BITMAPS AGAINST n NUMBERS TO MATCH
C
        PHS = 2
        MATCHED_NUMBERS = LBNPMT(2,PHS,GIND)

        IF(MATCHED_NUMBERS.NE.0) THEN

           CALL FASTSET(0,BET,3*3)
           DO B=1,BGONBB
              DO ROW = 1,BGOROW
                 DO COL = 1,BGOCOL
                    CALL BSET(BET(1,B,1),BOARDS(B,COL,ROW))
                 ENDDO
              ENDDO
           ENDDO
C
C COUNT NUMBERS ON BET WHICH HAVE BEEN HIT
C
           DO 100 B=1,BGONBB
              DO 90 NUM=BEGNUM2,ENDNUM2
                 COUNT = 0
                 DO WRD = 1,BET_WRD
                    TEMP(WRD) = 
     *              IAND(BET(WRD,B,1),MATCHTAB(WRD,NUM))
                    CALL BITCNT(TEMP(WRD),4,I)
                    COUNT = COUNT + I
                 ENDDO
C
C IF WINNING COUNT THEN INCREASE DIVISION COUNTER
C
                 DO J=20,BGOCOL*BGOROW
                    IF(LBNMAT(BGOMAXMAP+2+J,BGOFHS,GIND).GT.0) THEN
                       IF(COUNT.EQ.J) THEN
                          DIV=LBNMAT(BGOMAXMAP+2+J,BGOFHS,GIND)
                          BMSCAN(PHS,DIV,NUM)=BMSCAN(PHS,DIV,NUM)+1
                       ENDIF
                    ENDIF
                 ENDDO
C
C UPDATE WORST MATCH AND SECOND WORST MATCH
C
                 IF(LBNMAT(BGOMAXMAP+1,BGOFHS,GIND).GT.0) THEN
                    IF(COUNT.LT.BWSCAN(PHS,NUM)) THEN
                       IF(LBNMAT(BGOMAXMAP+2,BGOFHS,GIND).NE.0) THEN
                          BW2SCAN(PHS,NUM)=BWSCAN(PHS,NUM)
                          DO I=NUM,ENDNUM2
                             BMSCAN
     *                         (PHS,LBNMAT(BGOMAXMAP+2,BGOFHS,GIND),I)=
     *                       BMSCAN 
     *                         (PHS,LBNMAT(BGOMAXMAP+1,BGOFHS,GIND),I)
                          ENDDO
                       ENDIF
                       BWSCAN(PHS,NUM)=COUNT
                       BMSCAN(PHS,LBNMAT(BGOMAXMAP+1,BGOFHS,GIND),NUM)=1
                    ELSEIF(COUNT.EQ.BWSCAN(PHS,NUM)) THEN
                       BMSCAN(PHS,LBNMAT(BGOMAXMAP+1,BGOFHS,GIND),NUM) =
     *                 BMSCAN(PHS,LBNMAT(BGOMAXMAP+1,BGOFHS,GIND),NUM)+1
                    ENDIF
                 ENDIF
                 IF(LBNMAT(BGOMAXMAP+2,BGOFHS,GIND).GT.0) THEN
                    IF(COUNT.EQ.BW2SCAN(PHS,NUM)) 
     *                 BMSCAN(PHS,LBNMAT(BGOMAXMAP+2,BGOFHS,GIND),NUM) =
     *                 BMSCAN(PHS,LBNMAT(BGOMAXMAP+2,BGOFHS,GIND),NUM)+1
                 ENDIF
90            CONTINUE

100        CONTINUE         !end of board      
        ENDIF               !end of check against matched numbers 
C
C
        RETURN
        END
