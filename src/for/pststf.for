C SUBROUTINE PSTSTF
C  
C V07 13-OCT-99 RXK Table for World Tour added. 
C V06 20-APR-94 HXK WRITE TO FIRST RECORD ONLY
C V05 22-AUG-93 HXK changed stats arrays dimensions
C V04 16-JUL-93 HXK ADDED VAKIO STATS MATCH LIST
C V03 13-JUL-93 SXH released for Finland
C V02 21-JAN-93 DAB Initial Release Based on Netherlands Bible,
C                   DEC Baseline
C V01 05-MAY-92 HDB  RELEASED FOR VAX (FROM PSTSTF.FRN, SWEDEN)
C
C PSTSTF.FOR
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C

        SUBROUTINE PSTSTF
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:STACOM.DEF' 
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSTF.DEF'
C
        INTEGER*4   STATUS      !return value from calls (general use)
        INTEGER*4   FDB(7)      !file discriptor block
        INTEGER*4   TEMPCDC     !temp cdc var
C
        TYPE*,IAM(),' Posting System Statistics File'
        CALL OPENW(1,SFNAMES(1,STF),4,0,0,STATUS)
        CALL IOINIT(FDB,1,STFSEC*256)
        IF(STATUS.NE.0)THEN
            CALL FILERR(SFNAMES(1,STF),1,STATUS,0)
        ENDIF
C
        CALL FASTMOV(STALTO,STFLTO,(NUMTOT*2*MAXBRD*(MAXBRD+1)*NUMLTO))
        CALL FASTMOV(STAGAM,STFGAM,(NUMTOT*4*MAXGAM))
        CALL FASTMOV(STAKIK,STFKIK,(2*MAXGAM))
        CALL FASTMOV(STASPT,STFSPT,(NUMTOT*2*MAXBRD*7*NUMSPT))
        CALL FASTMOV(STASPT_CUP,STFSPT_CUP,NUMSPT)
        CALL FASTMOV(STASPT_TAB1,STFSPT_TAB1,(SPGNBR*7*NUMSPT))
        CALL FASTMOV(STASPT_TAB2,STFSPT_TAB2,(SPGNBR*3*NUMSPT))
        CALL FASTMOV(STATSL,STFTSL,(NUMTOT*5*NUMTSL))
C
C       TEMPCDC = DAYCDC
        TEMPCDC = 1   !only use 1st record
        CALL WRITEW(FDB,TEMPCDC,STFREC,STATUS)
        IF(STATUS.NE.0) THEN
            CALL FILERR(SFNAMES(1,STF),3,STATUS,TEMPCDC)
            CALL CLOSEFIL(FDB)
            CALL GPAUSE
        ENDIF
C
        TYPE*,IAM(),' Posting Complete'

        RETURN

        END
