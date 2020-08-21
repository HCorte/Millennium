C SUBROUTINE BPHWIN.FOR
C
C V06 13-apr-2000 OXK Allow multiple winning divisions / board
C V05 22-MAR-2000 RXK Special rules for quadruples. Make phase 2 (numbers match
C                     check) first and phase 1 (bitmaps check) afterwards.
C V04 01-FEB-2000 RXK Figured maps part removed, quadruples added. 
C V03 20-DEC-1994 HXK Always get triple and Fullhouse on scan
C V02 23-NOV-1994 HXK Fixed bugs in winning determination
C V01 27_OCT-1994 HXK Initial revision.
C
C SUBROUTINE TO CHECK IF BINGO TICKET IS A WINNER AND WHAT TYPE.
C TO GENERATE INFO FOR PHASE 2 OF WINNER SELECTION.
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
C     CALL BPHWIN(TRABUF,V4BUF,WIN)
C INPUT
C          TRABUF - TRANSACTION BODY
C          V4BUF  - VALIDATION RECORD
C OUTPUT
C          V4BUF  - UPDATED FOR NEW WINNER (IF ANY)
C          WIN    - ZERO -> NOT A WINNER
C          WINCOM WILL BE UPDATED TO HOLD INFO ON WHAT WINNERS THERE ARE
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE BPHWIN(TRABUF,V4BUF,WIN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
C
        INTEGER*4 BET_WRD
        PARAMETER (BET_WRD = (BGONBR/32) + 1)   !# Words for a bet.
C
        INTEGER*4   SHARES(BGODIV), PRZIND, DIV, GIND, ST, I, J, B
        INTEGER*4   BNG, MAXBNG

        INTEGER*4   NUM                         !# of #'s Loop Variable
        INTEGER*4   BOARDS(BGONBB,BGOCOL,BGOROW)!Bet in col,row format.
        INTEGER*4   BET(BET_WRD,BGONBB,BGOMAXMAP)!Bet in bitmap format
        INTEGER*4   WIN                         !# of winners on this bet
        INTEGER*4   PHS,SPHS                    !Phases,Subphases Loop Variables
        INTEGER*4   MAP                         !Bitmaps Loop Variable
        INTEGER*4   ROW                         !Row Loop Variable/ Index
        INTEGER*4   COL                         !Column Looop Var/ Index
        INTEGER*4   WRD                         !Word Loop Variable
        INTEGER*4   TEMP(BET_WRD)               !Temp for Bet
        INTEGER*4   COUNT                       !# Bits set Counter
        INTEGER*4   BINGO_MAPS                  !#of bitmaps for cutoff
        INTEGER*4   NUM_MATCH                   !#of numbers for cutoff
        INTEGER*4   NUM_COUNTER                 !Numbers counter
        INTEGER*4   NUMS_SCANNED(BGOSPH)        !#'s scanned in each subphase
        INTEGER*4   ACTMAP                      !# of actually defined maps
        INTEGER*4   FSTFND(BGONBB,BGODIV)       !# divisions won as "first"
        INTEGER*4   NUMS_PHS1                   !numbers in phase1
        INTEGER*4   NSUBPH                      !number of subphases   
        INTEGER*4   FHFOUND                     !# of board where fullh found 


        LOGICAL     CXLED
        LOGICAL     FIRST/.TRUE./
C
        GIND=TRABUF(TGAMIND)
C
C FIND MAXIMUM NUMBER OF BINGO MATCHES USED (i.e. 1,2,3 OR 4)
C
        IF(FIRST) THEN
           DO I=1,8
              IF(LBNFST(I,BGOFHS,GIND).NE.0.OR.
     *           LBNOTH(I,BGOFHS,GIND).NE.0) MAXBNG=(I-1)/2+1
           ENDDO
           DO I=1,BGOMAXMAP
              IF(LBNMAP(I,BGOFHS,GIND).NE.0) ACTMAP=I 
           ENDDO
           NSUBPH=LBNNSP(GIND)
           IF(NSUBPH.EQ.0) NSUBPH=5
           FIRST=.FALSE.
        ENDIF
C
        WIN = 0
        CXLED=.FALSE.
        ST=TRABUF(TSTAT)
        IF(LBNDRW(GIND).LT.0) RETURN
        IF(ST.NE.GOOD.AND.ST.NE.VOID.AND.
     *     ST.NE.INCA.AND.ST.NE.EXCH) RETURN
        IF(TRABUF(TWBEG).GT.LBNDRW(GIND)) RETURN
        IF(TRABUF(TWEND).LT.LBNDRW(GIND)) RETURN
        IF(TRABUF(TSTAT).EQ.VOID.OR.TRABUF(TSTAT).EQ.INCA) CXLED=.TRUE.
C
C CONVERT BOARD DATA
C
        CALL GETB_BTMBRD(TRABUF,BOARDS)
C
        CALL FASTSET (0,SHARES,BGODIV)
C
C PHASE 2. CHECK BET BITMAPS AGAINST n NUMBERS TO MATCH
C
      PHS = 2
      FHFOUND = 0
      NUMS_SCANNED(PHS) = LBNSPH(NSUBPH,GIND)
      NUM_MATCH = LBNPMT(2,PHS,GIND)

      IF(NUM_MATCH.NE.0) THEN

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
            COUNT = 0
            DO WRD = 1,BET_WRD
               TEMP(WRD) = 
     *         IAND(BET(WRD,B,1),MATCHTAB(WRD,NUMS_SCANNED(PHS)))
               CALL BITCNT(TEMP(WRD),4,I)
               COUNT = COUNT + I
            ENDDO
C
C FIND DIVISION FOR MATCHED NUMBERS (FOR 20-25 NUMBERS ONLY)
C
            DO J=20,BGOCOL*BGOROW
               IF(LBNMAT(BGOMAXMAP+2+J,BGOFHS,GIND).GT.0) THEN
                  IF(COUNT.EQ.25) FHFOUND = B
                  IF(COUNT.EQ.J) THEN
                     DIV=LBNMAT(BGOMAXMAP+2+J,BGOFHS,GIND)
                     SHARES(DIV)=SHARES(DIV)+1
                     WIN=WIN+1 
                     GOTO 100
                  ENDIF
               ENDIF
            ENDDO
C
C CHECK FOR WORST AND SECOND WORST MATCH (ON NUMBERS)
C
            IF(LBNMAT(BGOMAXMAP+1,BGOFHS,GIND).NE.0. AND.   !check worst match 
     *         COUNT.EQ.LBNWST(GIND)) THEN
               DIV=LBNMAT(BGOMAXMAP+1,BGOFHS,GIND)
               SHARES(DIV)=SHARES(DIV)+1
               WIN=WIN+1 
               GOTO 100
            ENDIF              
            IF(LBNMAT(BGOMAXMAP+2,BGOFHS,GIND).NE.0. AND.   !check second worst
     *         COUNT.EQ.LBNWS2(GIND))THEN
               DIV=LBNMAT(BGOMAXMAP+2,BGOFHS,GIND)
               SHARES(DIV)=SHARES(DIV)+1
               WIN=WIN+1 
               GOTO 100
            ENDIF

100      CONTINUE         !end of board      
      ENDIF               !end of check against matched numbers 
C
C PHASE 1. CHECK BET BITMAPS AGAINST WINNING BITMAPS
C
      PHS = 1
      BINGO_MAPS  = LBNPMT(1,PHS,GIND)
      NUM_MATCH   = LBNPMT(2,PHS,GIND)
      NUMS_PHS1   = LBNSPH(NSUBPH-1,GIND)     !last # for bitmap match 
C
      CALL FASTSET(0,FSTFND,BGONBB*BGODIV)
      CALL FASTSET(0,BET,BET_WRD*BGONBB*BGOMAXMAP)
      DO B=1,BGONBB
           DO MAP = 1,ACTMAP
              DO ROW = 1,BGOROW
                 DO COL = 1,BGOCOL
                    IF(BTEST(LBNMAP(MAP,BGOFHS,GIND),(COL-1)*BGOROW+ROW)) THEN
                      CALL BSET(BET(1,B,MAP),BOARDS(B,COL,ROW))
                    ENDIF 
                 ENDDO
              ENDDO
           ENDDO
      ENDDO

      IF(BINGO_MAPS.GT.0) THEN
          DO 70 B=1,BGONBB                     
C
C CHECK BET BITMAPS AGAINST WINNING BITMAPS WHICH CORRESPOND TO 
C NUMBERS DRAWN FOR FIRST MATCH 
C
              DO 60 I=1,BGODIV

                 IF(LBNNDF(I,GIND).NE.0.AND.           ! div is set as "first"
     *              LBNNDF(I,GIND).LE.NUMS_PHS1) THEN
                    NUM=LBNNDF(I,GIND)
                    BNG=0
                    DO 40 MAP = 1,BINGO_MAPS
                       DO WRD = 1,BET_WRD
                          TEMP(WRD) =
     *                        IAND(BET(WRD,B,MAP),MATCHTAB(WRD,NUM))
                          IF(TEMP(WRD).NE.BET(WRD,B,MAP)) GOTO 40
                       ENDDO
                       BNG=BNG+1             ! bingo found
40                  CONTINUE
  
                    IF(BNG.GT.0) THEN
                       BNG=MIN(BNG,MAXBNG)   !do not give higher div than set
                       IF(2*BNG-1.EQ.I) THEN
                          SHARES(I)=SHARES(I)+1      
                          WIN = WIN + 1
                          FSTFND(B,I) = 1
                       ENDIF
                    ENDIF 
                 ENDIF                      !end of search of "first" matches
60            CONTINUE
C
C FIND ALL "OTHER" BINGOS. IF FULLHOUSE FOUND ON BOARD THEN SKIP SUBPHASE 4 
C (I.E. QUADRUPLES) 
C
              DO 90 SPHS = NSUBPH-1,1,-1  
                 NUM=LBNSPH(SPHS,GIND)
                 IF(FHFOUND.EQ.B.AND.SPHS.EQ.4) GOTO 90   
                 BNG=0
                 DO 80 MAP = 1,BINGO_MAPS
                    DO WRD = 1,BET_WRD
                       TEMP(WRD) = IAND(BET(WRD,B,MAP),MATCHTAB(WRD,NUM))
                       IF(TEMP(WRD).NE.BET(WRD,B,MAP)) GOTO 80
                    ENDDO
                    BNG=BNG+1
80               CONTINUE
C
C DEFINE DIVISION FOR "OTHER" BINGOS
C
                 BNG=MIN(BNG,MAXBNG)
                 IF(BNG.EQ.SPHS) THEN
                    IF(FSTFND(B,2*BNG-1).GT.0) goto 70  !corresp "first" found
                    IF(LBNOTH(2*BNG-1,BGOFHS,GIND).NE.0) THEN   !if "other" set
                       DIV=LBNOTH(2*BNG-1,BGOFHS,GIND)
                       SHARES(DIV)=SHARES(DIV)+1
                       WIN=WIN+1
                       GOTO 90		      ! V06 Allow multiple divs / board
                    ENDIF
                 ENDIF
90            CONTINUE      

70         CONTINUE    ! end of board
      ENDIF            ! end of check against bitmaps
C
      IF(WIN.EQ.0) RETURN
C
C UPDATE VALIDATION RECORD
C
        IF(V4BUF(VFSSER).NE.0) THEN
          CALL LOGVAL(VALREC,V4BUF)
          CALL DLOGVAL(VALREC,VDETAIL)
        ELSE
          CALL FASTSET(0,VALREC,VALLEN)
          CALL FASTSET(0,VDETAIL,VPLEN*VMAX)
        ENDIF
C
        DO 1100 DIV = 1,BGODIV
            IF(SHARES(DIV).EQ.0) GOTO 1100 
            IF(.NOT.CXLED)
     *        LBNSHR(DIV,BGOFHS,GIND) = LBNSHR(DIV,BGOFHS,GIND) + 
     *        SHARES(DIV)
            VALREC(VPZOFF) = VALREC(VPZOFF) + 1
            PRZIND = VALREC(VPZOFF)
            IF(PRZIND.GT.VMAX) THEN
                TYPE*,IAM(),' Prize table overflow ',TRABUF(TCDC),TRABUF(TSER)
                CALL GPAUSE
                VALREC(VSTAT) = VHOLD
                VALREC(VPZOFF) = VMAX
                GOTO 1100 
            ENDIF
C
            VDETAIL(VKIK,PRZIND)=0
            VDETAIL(VKI2,PRZIND)=0
            VDETAIL(VPRG,PRZIND)=0
            VDETAIL(VUPD,PRZIND)=0
            VDETAIL(VDIV,PRZIND)=DIV
            VDETAIL(VBDR,PRZIND)=0
            VDETAIL(VSHR,PRZIND)=SHARES(DIV)
            VDETAIL(VDRW,PRZIND)=LBNDRW(GIND)
            VDETAIL(VSUB,PRZIND)=BGOFHS
1100    CONTINUE
        VALREC(VWCDC)=DAYCDC
C
C UPDATE VALIDATION HEADER IF NEW WINNER
C
        IF(VALREC(VSTAT).EQ.VNOWIN) THEN
          VALREC(VSCDC)=TRABUF(TCDC)
          VALREC(VSTER)=TRABUF(TTER)
          VALREC(VSSER)=TRABUF(TSER)
          VALREC(VEXP )=TRABUF(TWEND)
          VALREC(VGAM )=TRABUF(TGAM)
          VALREC(VGTYP)=TRABUF(TGAMTYP)
          VALREC(VGIND)=TRABUF(TGAMIND)
          VALREC(VFRAC)=TRABUF(TFRAC)
          VALREC(VBNKID)=TRABUF(TWBNKID)
          VALREC(VBNKNUM)=TRABUF(TWBNKNM)
          VALREC(VKEXP)=0
          VALREC(VKGME)=TRABUF(TWKGME)
        ENDIF
C
C
        IF(VALREC(VSTAT).EQ.VPRPAY) THEN
          VALREC(VSTAT)=VPPNPZ
        ELSEIF(VALREC(VSTAT).NE.VHOLD) THEN
          VALREC(VSTAT)=VNOPRZ
        ENDIF
        IF(TRABUF(TSTAT).EQ.VOID) VALREC(VSTAT)=VCXL
        IF(TRABUF(TSTAT).EQ.INCA) VALREC(VSTAT)=VDEL
        CALL DVALLOG(VALREC,VDETAIL)
        CALL VALLOG (VALREC,V4BUF  )
        RETURN
        END
