C SUBROUTINE BFHSCANF.FOR
C
C V01 02-MAR-2000 RXK initial version.
C  
C FINAL BINGO SCAN TO CREATE TABLES OF NUMBER OF WINNERS ON SUBPHASES
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
        SUBROUTINE BFHSCANF(TRABUF,BFSCAN,WRKPHS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
C
        INTEGER*4   BFSCAN(BGOPHS,BGODIV,BGONBR)!# of divs won on ball N

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
        INTEGER*4   ACTMAP                      !# of actually defined maps
	INTEGER*4   SPHS,NSUBPH
	INTEGER*4   WRKPHS(BGOSPH)
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
     *           LBNOTH(I,BGOFHS,GIND).NE.0)   MAXBNG=(I-1)/2+1
           ENDDO
           DO I=1,BGOMAXMAP
              IF(LBNMAP(I,BGOFHS,GIND).NE.0) ACTMAP=I 
           ENDDO
           PHS = 1
           MATCHED_BITMAPS = LBNPMT(1,PHS,GIND)
           NSUBPH=LBNNSP(GIND)
           IF(NSUBPH.EQ.0) NSUBPH=5
           FIRST=.FALSE.
        ENDIF
C
        IF(LBNDRW(GIND).LT.0) RETURN
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
C CHECK BET BITMAPS AGAINST WINNING BITMAPS
C
        IF(MATCHED_BITMAPS.NE.0) THEN
           DO 70 B=1,BGONBB                     
              DO 60 SPHS = NSUBPH-1,1,-1
                 NUM=WRKPHS(SPHS)
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
C DEFINE DIVISION FOR "OTHER" BINGOS
C
                 BNG=MIN(BNG,MAXBNG)
                 IF(BNG.EQ.SPHS) THEN
                    DIV=LBNOTH(2*BNG-1,BGOFHS,GIND)
                    BFSCAN(PHS,DIV,NUM)=BFSCAN(PHS,DIV,NUM)+1
                    GOTO 70
                 ENDIF
60            CONTINUE
70         CONTINUE
        ENDIF
C
        RETURN
        END
