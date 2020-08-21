C
C BNKSUBS.FOR
C
C V05 30-MAR-2011 FRP Taken from NRM_OPEN_OPS
C V04 22-DEC-2010 FRP Lotto2 Changes
C V03 13-DEC-2010 FJG Lotto2 Batch change of KEY
C V02 17-DEC-2003 FRP Modify for Batch2 Totobola Changes.
C V01 30-JAN-2001 EPH INITIAL RELEASE FOR PORTUGAL
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C       ************************************************************
	SUBROUTINE OPEN_BRANCH (ST)
C       ************************************************************
C       Routine to open BRANCH.FIL file
C       ************************************************************
	IMPLICIT NONE

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:BRANCH_REC.DEF'

        INTEGER*4    ST            !(OUTPUT) STATUS FOR OPEN

	ST = 0

	OPEN (UNIT           =  BRH_LUN,
     *        FILE           = 'FILE:BRANCH.FIL',
     *        STATUS         = 'UNKNOWN',
     *        ORGANIZATION   = 'INDEXED',
     *        ACCESS         = 'KEYED',
     *        FORM           = 'UNFORMATTED',
     *        RECL           =  SIZEOF(BRANCH_REC),
     *        KEY            =  (1:8:CHARACTER:ASCENDING),
     *        RECORDTYPE     = 'FIXED',
     *        IOSTAT         =  ST)

        RETURN
        END



C       ************************************************************
	SUBROUTINE LOAD_BANK_TABLE (ST)
C       ************************************************************
C       Routine to LOAD BANK_TAB FROM BANK.FIL file
C       ************************************************************
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:BANK_REC.DEF'

        INTEGER*4    ST            !(OUTPUT) STATUS FOR OPEN
	INTEGER*4    I

	ST = 0
C
C	LOAD FROM FILE ONLY IF TABLE IS NOT FILLED YET
C
	DO I=1,MAXBANKS
CC	   IF (CTOI(BANK_TAB(I).BANK,SZ).NE.0) RETURN     !FOUND SOMEONE (TABLE IS FILLED)
	   IF (BANK_TAB(I).BANK.GT.'0000' .AND.
     *         BANK_TAB(I).BANK.LE.'9999'      ) RETURN
	   BANK_TAB(I).BANK = '0000'
        ENDDO
C
	CALL OPEN_BANK('READ', ST)
	IF (ST.NE.0) RETURN
	   
	I = 1
50	READ (BNK_LUN, END=100, ERR=70) BANK_REC
	IF (I.GT.MAXBANKS) THEN
	   TYPE*,'>>> More Banks then expected (MAXBANKS) during LOAD_BANK_TABLE'
	   ST = -100
           GOTO 100
	ENDIF
 	BANK_TAB(I) = BANK_REC
	I = I + 1
	GOTO 50

70	ST = -90
100	CLOSE(BNK_LUN)
        RETURN
        END


C       ************************************************************
	SUBROUTINE GET_BANK_POS (BANK, BANKPOS)
C       ************************************************************
C       Routine to get the position in BANK_TAB 
C       (loaded by LOAD_BANK_TABLE) for the BANK
C       ************************************************************
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:BANK_REC.DEF'

        INTEGER*4    BANK            !(INPUT)
	INTEGER*4    BANKPOS	     !(OUTPUT)

	INTEGER*4    SZ
C
C	BANK MUST HAVE A VALUE
C
	IF (BANK.LE.0) THEN
	   BANKPOS = 0
           RETURN
        ENDIF

	DO BANKPOS = 1,MAXBANKS
	   IF (CTOI(BANK_TAB(BANKPOS).BANK,SZ).EQ.BANK) RETURN     !FOUND THE BANK
        ENDDO
C
C	DID NOT FOUND IT
C
	BANKPOS = 0
	RETURN
	END	



C       ************************************************************
	SUBROUTINE OPEN_BANK (ACCESS, ST)
C       ************************************************************
C       Routine to OPEN BANK.FIL file
C       ************************************************************
	IMPLICIT NONE

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:BANK_REC.DEF'

	CHARACTER*5  ACCESS        !(INPUT)  'READ'/'WRITE'
        INTEGER*4    ST            !(OUTPUT) STATUS FOR OPEN

	ST = 0

        IF (ACCESS(1:4).EQ.'READ') THEN
	   OPEN (UNIT           =  BNK_LUN,
     *           FILE           = 'FILE:BANK.FIL',
     *           STATUS         = 'OLD',
     *           ORGANIZATION   = 'SEQUENTIAL',
     *           ACCESS         = 'SEQUENTIAL',
     *           FORM           = 'UNFORMATTED',
     *           RECL           =  SIZEOF(BANK_REC),
     *           RECORDTYPE     = 'FIXED',
     *           IOSTAT         =  ST)
	ELSE
	   OPEN (UNIT           =  BNK_LUN,
     *           FILE           = 'FILE:BANK.FIL',
     *           STATUS         = 'NEW',
     *           ORGANIZATION   = 'SEQUENTIAL',
     *           ACCESS         = 'SEQUENTIAL',
     *           FORM           = 'UNFORMATTED',
     *           RECL           =  SIZEOF(BANK_REC),
     *           RECORDTYPE     = 'FIXED',
     *           IOSTAT         =  ST)
	ENDIF

        RETURN
        END
