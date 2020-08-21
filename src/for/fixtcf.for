C
C V01 22-Jan-1999 UXN Initial release. 
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
	PROGRAM FIXTCF
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMVLF.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4   ST,SERIAL(2)
	INTEGER*4   BUF(2048)
	INTEGER*4   LBUF(LREC*3)
        INTEGER*4       TOTAL(TCRS+NUMCRS+1,2)
C
	CALL COPYRITE
C
	CALL INPNUM('Enter CDC          ',SERIAL(1),0,999999999,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
	CALL INPNUM('Enter serial number',SERIAL(2),0,999999999,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
C
        CALL IOPEN(SFNAMES(1,TCF),TCF,LREC*2,LCDC,LSER*2-1,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,TCF),1,ST,0)
C
	CALL IREAD(SERIAL,LBUF,TCF,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,TCF),2,ST,SERIAL)
C
	CALL LOGTRA(TRABUF,LBUF)

	TYPE*,IAM(),'This is the transaction currently in TCF'
        CALL PRINTRA(TRABUF,6,.TRUE.,.FALSE.,TOTAL,.FALSE.)

	TRABUF(TWBEG) = 2
	TRABUF(TWDUR) = 4

	TYPE*,IAM(),'This is the transaction before putting into TCF'
        CALL PRINTRA(TRABUF,6,.TRUE.,.FALSE.,TOTAL,.FALSE.)
	CALL INPYESNO('Are you sure you want to change ticket status in TCF',ST)
	IF(ST.NE.1) CALL GSTOP(GEXIT_OPABORT)

	CALL TRALOG(TRABUF,LBUF)
	CALL IWRITE(LBUF,TCF,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,TCF),3,ST,SERIAL)

	CALL IREAD(SERIAL,LBUF,TCF,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,TCF),2,ST,SERIAL)
	CALL LOGTRA(TRABUF,LBUF)
	TYPE*,IAM(),'This is the transaction after changes in TCF'
        CALL PRINTRA(TRABUF,6,.TRUE.,.FALSE.,TOTAL,.FALSE.)
        CALL ICLOSE(TCF,BUF,ST)
	END
