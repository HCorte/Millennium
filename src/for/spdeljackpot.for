C SPDELJACKPOT.FOR
C
C V01 30-SEP-2004 FRP Initial revision.
C
C PROGRAM TO DELETE "DSPPOL" VALUE (POOL CARRIED OVER) FROM A TOTOBOLA GAME
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
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM SPDELJACKPOT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
C
	INTEGER*4 FLAG,ST
	INTEGER*4 GNUM,GTYP,DRAW,DIV
	INTEGER*4 FDB(7)
C
C START
C
	CALL COPYRITE
C
	TYPE*, IAM()
	TYPE*, IAM(),'This program allows to delete rollover'
	TYPE*, IAM(),'amounts from a Totobola game.'
C
	TYPE*, IAM()
	CALL PRMYESNO('Are you sure you want to continue [Y/N]? ',FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
C READ SCF
C
	CALL GETSCONF(SCFREC,ST)
	IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C ASK GAME TO DELETE "DSPPOL" VALUE
C
10      CONTINUE
        TYPE*, IAM()
        CALL INPNUM('Enter game number to delete rollover amount: ',
     *              GNUM,1,MAXGAM,ST)
        IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
        GTYP=SCFGNT(GAMTYP,GNUM)
        IF(GTYP.NE.TSPT) THEN
          TYPE*, IAM()
          TYPE*, IAM(),'Sorry, this is not a Totobola game'
          CALL XWAIT(2,2,ST)
          GOTO 10
        ENDIF
C
C OPEN TOTOBOLA GAME FILE
C
	CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DSPSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
C
C ASK DRAW TO DELETE "DSPPOL" VALUE
C
	TYPE*, IAM()
	CALL INPNUM('Enter draw number to delete rollover amount: ',DRAW,1,10000,ST)
	IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C READ TOTOBOLA GAME FILE
C
	CALL READW(FDB,DRAW,DSPREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
C ASK DIVISION TO DELETE "DSPPOL" VALUE
C
	TYPE*, IAM()
	CALL INPNUM('Enter division number to delete rollover amount: ',DIV,1,DSPDIV,ST)
	IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C DISPLAY "DSPPOL" VALUE FOR THAT DRAW AND DIVISION
C
	TYPE*, IAM()
	TYPE*, IAM(),'Current value for rollover amount is ',DSPPOL(DIV),
     *               ' for draw ',DRAW,' and for division ',DIV
C
C ASK TO DELETE "DSPPOL" VALUE
C
20	CONTINUE
	TYPE*, IAM()
	CALL PRMYESNO('Do you want to delete current value for rollover amount [Y/N]? ',FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
C WRITE TOTOBOLA GAME FILE
C
	DSPPOL(DIV)=0
	CALL WRITEW(FDB,DRAW,DSPREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
C
	TYPE*, IAM()
	TYPE*, IAM(),'Value for rollover amount already deleted',
     *               ' for draw ',DRAW,' and for division ',DIV
C
C CLOSE TOTOBOLA GAME FILE AND EXIT
C
        CALL CLOSEFIL(FDB)
        CALL GSTOP(GEXIT_SUCCESS)
C
        END

