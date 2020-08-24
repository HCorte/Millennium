C
C SUBROUTINE OUTCAN
C
C V09 26-JAN-2001 UXN REJT,RETY added.
C V08 18-MAR-1999 RXK Game type/game index change. Removed hack for V5.
C V07 24-APR-1995 HXK Merge of V5 development with March 10th 1995 bible
C V06 22-FEB-1995 HXK Hack for terminal dudes
C                     Central V5: GTYP = 9, GIND = 2
C                     TerminalV5: GTYP =14, GIND = 1
C V05 16-JUL-1993 GXA Added TSUBERR to error message (generic).
C V04 28-JUN-1993 HXK changed err message length from 5 to 6
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 01-NOV-1991 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SUBROUTINE TO BUILD TERMINAL CANCELLATION MESSAGES.
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE OUTCAN(TRABUF,OUTTAB,OUTLEN,ERCODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
        ! arguments
        BYTE       OUTTAB(*)      !

        INTEGER*4  ERCODE         !

	INTEGER*2  OUTLEN         !

        ! variables
	INTEGER*4 MYCHKSUM        !
        INTEGER*4 CHKLEN          !
        INTEGER*4 CHECK           !
        INTEGER*4 TEMP            !
	INTEGER*4 CONTRL          !
        INTEGER*4 ERRTYP          !
        INTEGER*4 CANTYP          !
        INTEGER*4 CANERR          !
        INTEGER*4 IND             ! INDEX TO MESSAGE BUFFER

	BYTE      I1TEMP(4)       !

	EQUIVALENCE (TEMP,I1TEMP)

	DATA CANTYP/Z20/
	DATA ERRTYP/Z90/
	DATA CANERR/Z2F/
	DATA CONTRL/Z20/
C
C
	TEMP      = TRABUF(TTRN)+CONTRL
	OUTTAB(1) = TEMP                     
C
C IF STATUS IS GOOD THEN BUILD CANCELLATION MESSAGE
C ELSE SEND BACK THE APPROPIATE ERROR MESSAGE.
C
	IF(TRABUF(TSTAT).EQ.GOOD .OR.
     *     TRABUF(TSTAT).EQ.REJT.AND.TRABUF(TERR).EQ.RETY) THEN
	    OUTTAB(2) = CANTYP 
            IND = 5
            CALL PUTIME(TRABUF(TTIM), OUTTAB, IND)              
C
	    CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),TEMP,CHECK)
C
	    OUTTAB(8)  = I1TEMP(3)           
	    OUTTAB(9)  = I1TEMP(2)
	    OUTTAB(10) = I1TEMP(1)
	    OUTTAB(11) = CHECK
C
C SET GAME TYPE GAME INDEX
C
            OUTTAB(12) = TRABUF(TGAMTYP)
            OUTTAB(13) = TRABUF(TGAMIND)
            OUTTAB(14) = 0                ! cancel status = 0

C
C SET TOTAL CANCEL AMOUNT
C
	    TEMP = TRABUF(TWTOT) - TRABUF(TWDAMT)
	    OUTTAB(15) = I1TEMP(4)
	    OUTTAB(16) = I1TEMP(3)
	    OUTTAB(17) = I1TEMP(2)
	    OUTTAB(18) = I1TEMP(1)
C
C SET MONDAY FLAG INDICATOR ( ONLY FOR LOTTO WAGERS, OTHERWISE SEND ZERO )
C
            TEMP = 0
            IF(TRABUF(TGAMTYP) .EQ. TLTO) TEMP = TRABUF(TWLMFI)
            OUTTAB(19) = I1TEMP(1)
C
C SET LENGHT OUTPUT BUFFER
C
	    OUTLEN = 19
C
	ELSEIF(ERCODE.NE.0) THEN
            OUTTAB(2)=CANERR
C
            IND = 5
            CALL PUTIME(TRABUF(TTIM), OUTTAB, IND)
C
 	    CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),TEMP,CHECK)
            OUTTAB(8)  = I1TEMP(3)
            OUTTAB(9)  = I1TEMP(2)
            OUTTAB(10) = I1TEMP(1)
            OUTTAB(11) = CHECK

            OUTTAB(12)=TRABUF(TGAMTYP)
            OUTTAB(13)=TRABUF(TGAMIND)
	    OUTTAB(14)=ERCODE

	    OUTLEN=14
	ELSE
	    OUTTAB(2) = ERRTYP
	    OUTTAB(5) = TRABUF(TERR)
            OUTTAB(6) = TRABUF(TSUBERR)
	    OUTLEN = 6
	ENDIF
C
C CALCULATE CHECKSUM
C
        I4CCITT   = TRABUF(TCHK)
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)
        CHKLEN=OUTLEN-1
        CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT   = MYCHKSUM
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)

        RETURN

	END
