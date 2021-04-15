C  GXSRC:DIVAL.FOR
C
C V13 11-DEZ-2020 SCML New Terminals Project - Olimpo
C V12 29-JAN-2013 SCML Syntax Error Codes differentiation.
C V11 30-DEC-2013 SCML Init BNKVALM variable to FALSE.
C V10 08-NOV-2013 SCML Changed Amount in cents from 2 to 4 bytes long.
C                      Check syntax error for new bank validation mode.
C V09 08-OCT-2103 SCML Added new Option Flags/Data:
C                      NEW BANK VALIDATION MODE, BANK DATA.
C V08 19-FEB-2001 UXN Portugal changes.
C V07 06-OCT-2000 UXN AlphaIPS release.
C V06 30-Dec-1993 ceb Added ability to handle up to 7 INSTANT
C                     validations in one transaction.   RFSS TX1100-59
C V05 01-JUL-1992 NJA ADDED OI TERMINAL OFFSET FOR TICKET INDICATOR.
C V04 10-FEB-1992 JPJ ADDED (GVT)
C V03 04-FEB-1992 JPJ ADDED (GVT REVBYT)
C V02 04-FEB-1992 JPJ ADDED (2 BYTE CHECKSUM)
C V01 13-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO DECODE INSTANT VALIDATION MESSAGE FROM TERMINAL
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
	SUBROUTINE DIVAL(TERMES,TRABUF,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
	INTEGER*2 MESLEN
	BYTE	  TERMES(*)
C
	INTEGER*4   TEMP
	INTEGER*4   CHKLEN, MYCHKSUM, ENCMES, ENCACT
	INTEGER*4   OPTIONS, I, IND, TKTIND, ACT_TIME(3)
        
        LOGICAL     BNKVALM /.FALSE./ !V09
        INTEGER*4   TEMP1,TEMP2,TEMP3 !V09
        INTEGER*8   NIB               !V09
        BYTE        I1NIB(8)          !V09
        EQUIVALENCE (NIB,I1NIB)       !V09
C
        BNKVALM = .FALSE. !V11
C
C GET SEQUENCE NUMBER
C
	TEMP = ZEXT(TERMES(1))
	TRABUF(TTRN)=IAND(TEMP,15)
C
C GET CHECKSUM
C
	CALL TERM_TO_HOST(TERMES(3), TRABUF(TCHK), 2)
C
C GET STATISTICS
C
 	TRABUF(TTSTCS)=ZEXT(TERMES(5))
C
C GET OPTION FLAGS
C
	CALL TERM_TO_HOST(TERMES(6), OPTIONS, 2)
	IND=8
	
C
C CHECK FOR NODE NUMBER (NOT USED)
C
        IF(IAND(OPTIONS,'0001'X).NE.0) THEN
	   IND=IND+4
	ENDIF
C
C CHECK FOR RETAILER NUMBER (NOT USED)
C
        IF(IAND(OPTIONS,'0002'X).NE.0) THEN
 	   IND=IND+4
	ENDIF
C
C CHECK FOR PASSWORD (NOT USED)
C
        IF(IAND(OPTIONS,'0004'X).NE.0) THEN
 	   IND=IND+2
	ENDIF
C
C CHECK FOR ORIGINATOR   (NOT USED)
C
        IF(IAND(OPTIONS,'0008'X).NE.0) THEN
 	   IND=IND+2
	ENDIF
C
C CHECK FOR LOCATION NUMBER (NOT USED)
C
	IF(IAND(OPTIONS,'0010'X).NE.0) THEN
	   IND=IND+4
	ENDIF
C
C CHECK FOR USER ID (NOT USED)
C
	IF(IAND(OPTIONS,'0020'X).NE.0) THEN
	   IND=IND+4
	ENDIF
C
C CHECK FOR OPERATOR ID (NOT USED)
C
	IF(IAND(OPTIONS,'0040'X).NE.0) THEN
	   IND=IND+1
	ENDIF
!-------->>V09 ---------------------------------------------------------
C
C CHECK FOR PAYMENT TYPE (NOT USED?)
C
        IF(IAND(OPTIONS,'0080'X).NE.0) THEN
          IND=IND+2
        ENDIF
C
C CHECK FOR BANK VALIDATION MODE
C
        IF(IAND(OPTIONS,'0100'X).NE.0) THEN
          TRABUF(TIVMT) = IBVMT ! NEW LAYOUT (BANK VALIDATION MODE)
          CALL TERM_TO_HOST(TERMES(IND), TRABUF(TIVALM), 1)
          IF(TRABUF(TIVALM) .NE. IVBM_REG .AND.
     *       TRABUF(TIVALM) .NE. IVBM_CSH .AND.
     *       TRABUF(TIVALM) .NE. IVBM_BNK) THEN
            TRABUF(TERR)=SYNT
!            SYNTERRCOD=84
            SYNTERRCOD=86 !V12
            GOTO 8000
          ENDIF
          BNKVALM = .TRUE.
          IND=IND+1
        ENDIF
C
C CHECK FOR BANK DATA
C
        IF(IAND(OPTIONS,'0200'X) .NE. 0) THEN
          IF(TRABUF(TIVALM) .EQ. IVBM_BNK) THEN
            ! PLAYER ID TYPE
            CALL TERM_TO_HOST(TERMES(IND+0), TRABUF(TIPLIDTYP), 1)
            IND = IND + 1
            
            ! GET PLAYER CARD
            CALL TERM_TO_HOST(TERMES(IND+0), TRABUF(TIPLCARD), 4)
            IND = IND + 4
            
            ! BANK BRANCH
            TEMP1 = ZEXT(TERMES(IND+0))
            TEMP2 = ZEXT(TERMES(IND+1))
            TRABUF(TINIBBB) = ISHFT(TEMP1,8) + TEMP2
            IND = IND + 2
            
            ! BANK OFFICE
            TEMP1 = ZEXT(TERMES(IND+0))
            TEMP2 = ZEXT(TERMES(IND+1))
            TRABUF(TINIBBO) = ISHFT(TEMP1,8) + TEMP2
            IND = IND + 2
            
            ! BANK ACCOUNT
            I1NIB(5) = ZEXT(TERMES(IND+0))
            I1NIB(4) = ZEXT(TERMES(IND+1))
            I1NIB(3) = ZEXT(TERMES(IND+2))
            I1NIB(2) = ZEXT(TERMES(IND+3))
            I1NIB(1) = ZEXT(TERMES(IND+4))
            IND = IND + 5
            TRABUF(TINIBBA1) = NIB/100
            TRABUF(TINIBBA2) = MOD(NIB,100)
            
            ! CHECK DIGITS
            TRABUF(TINIBCD) = ZEXT(TERMES(IND))
            IND = IND + 1
          ELSE !V10
            ! BANK DATA FLAG IS SET AND VALIDATION MODE IS NOT CORRECT
            TRABUF(TERR)=SYNT
!            SYNTERRCOD=84
            SYNTERRCOD=87 !V12
            GOTO 8000
          ENDIF
        ENDIF
!-------- V09<<---------------------------------------------------------
C
C BATCH FLAG
C
C Values        Description
C x00           Not Batched
C x01           Last
C x02           Batched
C x40           GVT Not Batched
C x42           GVT Batched
C x80           Fail

        TEMP=0
        TKTIND=ZEXT(TERMES(IND+0))
        IF(IAND(TKTIND,'40'X).NE.0) THEN
          IF(IAND(TKTIND,'02'X).NE.0) THEN
            TEMP=0 !GVT Batched
          ELSE
            TEMP=1 !GVT Not Batched
          ENDIF
          TKTIND=0
        ELSE
          TKTIND=2 !Not Batched/Last/Batched/Fail
        ENDIF
        TKTIND=TKTIND+TEMP ! =0 GVT Batched 
                           ! =1 GVT Not Batched
                           ! =2 Not Batched/Last/Batched/Fail 
        TRABUF(TIIND)=TKTIND
        IND=IND+1
C
C Retailer number, who gets the credit 
C
	CALL TERM_TO_HOST(TERMES(IND), TRABUF(TIVAGT), 4)
	IND = IND + 4
C
C Validation mode
C
        IF(.NOT. BNKVALM) THEN
          TRABUF(TIVMT) = IRVMT ! OLD LAYOUT
          CALL TERM_TO_HOST(TERMES(IND), TRABUF(TIVALM), 1)
        ENDIF
        IND = IND + 1
        TRABUF(TIVALT) = 1 ! no bank validations
C
C Envelope ID
C
	CALL TERM_TO_HOST(TERMES(IND), TRABUF(TIVENV), 4)
        IND = IND + 4
C
C GET NUMBER OF VALIDATIONS IN BATCH
C
        TRABUF(TIBCH)=ZEXT(TERMES(IND))
        IF(.NOT. BNKVALM) THEN
          IF(TRABUF(TIBCH).GT.TIVMX.OR.TRABUF(TIBCH).LE.0) THEN
            TRABUF(TIBCH)=1
            TRABUF(TERR)=SYNT
            SYNTERRCOD=84
            GOTO 8000
          ENDIF
        ELSE !V09
          IF(TRABUF(TIBCH) .NE. 1) THEN
            TRABUF(TIBCH)=1
            TRABUF(TERR)=SYNT
!            SYNTERRCOD=84
            SYNTERRCOD=85 !V12
            GOTO 8000
          ENDIF
        ENDIF
        IND=IND+1
C
C GET ACTUAL TIME WHEN TRANSACTION IS DONE
C
        CALL ICLOCK(2, ACT_TIME)
C
C GET INSTANT GAME NUMBER
C
	DO 100 I=0,TRABUF(TIBCH)-1

	   CALL TERM_TO_HOST(TERMES(IND), TRABUF(TIGAM1+I), 2)
	   IND=IND+2
C
C GET INSTANT LOT/PACK NUMBER
C
	   CALL TERM_TO_HOST(TERMES(IND), TRABUF(TIPCK1+I), 4)
	   IND=IND+4
C
C GET INSTANT VIRN NUMBER (9 DIGITS)
C
	   CALL TERM_TO_HOST(TERMES(IND), TRABUF(TIVRN1+I), 4)
	   IND=IND+4
	   
C
C GET INSTANT LATEX NUMBER
C
	   CALL TERM_TO_HOST(TERMES(IND), TRABUF(TILTX1+I), 2)
	   IND=IND+2

C
C GET INSTANT AMOUNT
C
!-------->>V10 ---------------------------------------------------------
!	   CALL TERM_TO_HOST(TERMES(IND), TRABUF(TIPRZ1+I), 2)
!           IND=IND+2
	   CALL TERM_TO_HOST(TERMES(IND), TRABUF(TIPRZ1+I), 4)
           IND=IND+4
!-------- V10<<---------------------------------------------------------
C
C SET INSTANT TIME CASHED ( TIME IS SET BYT HOST, NOT BY TERMINAL )
C
           TRABUF(TITIM1 + I) = ACT_TIME(1)
C
C SET INSTANT CDC CASHED ( CDC IS SET BY HOST, NOT BY TERMINAL )
C
           TRABUF(TICDC1 + I) = DAYCDC
100	CONTINUE
C
C CHECK MESSAGE CHECKSUM
C
	IF(P(SUPSUM).EQ.0) THEN
          IF(.NOT.BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTSUM)) THEN
	    I4CCITT=IAND(BASECHKSUM+TRABUF(TTER),'FFFF'X)
	    CALL HOST_TO_TERM(TERMES(3), I4CCITT, 2)
	    CHKLEN=MESLEN-1
	    CALL GETCCITT(TERMES,1,CHKLEN,MYCHKSUM)
	    IF(MYCHKSUM.NE.TRABUF(TCHK)) TRABUF(TERR)=CBAD
	  ENDIF
	ENDIF
C
C CHECK FOR DES ERROR
C
        IF(P(DESACT).EQ.0) THEN
          ENCMES = ZEXT(TERMES(1))
          ENCMES = IAND(ENCMES,'08'X)
          IF(P(DESFLG).EQ.0.AND.
     *       BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTDES)) THEN
             ENCACT='08'X
          ELSE
             ENCACT=0
          ENDIF
          IF(ENCMES.NE.ENCACT) TRABUF(TERR) = DESMOD
        ENDIF
C
8000	CONTINUE
C----+------------------------------------------------------------------
C V13| New Terminals Project - Olimpo
C----+------------------------------------------------------------------ 
          IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
            IF(TRABUF(TIBCH).GE.1.AND.TRABUF(TIBCH).LT.4) THEN
              TRABUF(TSIZE) = 2
            ELSE IF (TRABUF(TIBCH).GE.4) THEN
              TRABUF(TSIZE) = 3
            ELSE
              TRABUF(TSIZE) = 2
            ENDIF
          ELSE
            IF(TRABUF(TIBCH).GE.1.AND.TRABUF(TIBCH).LE.4) THEN
              TRABUF(TSIZE) = 2
            ELSE IF (TRABUF(TIBCH).GE.5) THEN
              TRABUF(TSIZE) = 3
            ENDIF
          ENDIF  
C	IF(TRABUF(TIBCH).GE.1.AND.TRABUF(TIBCH).LE.4) THEN
C	  TRABUF(TSIZE) = 2
C	ELSE IF (TRABUF(TIBCH).GE.5) THEN
C	  TRABUF(TSIZE) = 3
C	ENDIF
C----+------------------------------------------------------------------
C V13| New Terminals Project - Olimpo
C----+------------------------------------------------------------------ 
        IF(TRABUF(TERR).NE.NOER) TRABUF(TSTAT)=REJT
        RETURN
        END
