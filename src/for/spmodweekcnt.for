C SPMODWEEKCNT.FOR
C
C V01 21-JUL-2004 FRP Initial revision.
C
C PROGRAM TO MODIFY "DSPPRN" VALUE (NUMBER OF CONSECUTIVE
C WEEKS WITHOUT SUPER14 WINNERS FOR TOTOBOLA NORMAL)
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
	PROGRAM SPMODWEEKCNT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
C
	INTEGER*4 FLAG,ST
	INTEGER*4 GNUM,DRAW,WCNT
	INTEGER*4 FDB(7)
C
C START
C
	CALL COPYRITE
C
	TYPE*, IAM()
	TYPE*, IAM(),'This program allows to modify the counter'
	TYPE*, IAM(),'for the number of consecutive weeks without'
	TYPE*, IAM(),'Super14 winners in Totobola Normal game.'
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
C OPEN TOTOBOLA NORMAL GAME FILE
C
	GNUM=1
	CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,3,DSPSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
C
C ASK DRAW TO MODIFY "DSPPRN" VALUE
C
	TYPE*, IAM()
	CALL INPNUM('Enter draw number to modify week counter: ',DRAW,1,10000,ST)
	IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C READ TOTOBOLA NORMAL GAME FILE
C
	CALL READW(FDB,DRAW,DSPREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
C DISPLAY "DSPPRN" VALUE FOR THAT DRAW
C
	TYPE*, IAM()
	TYPE*, IAM(),'Current value for week counter is ',DSPPRN,' for draw ',DRAW
C
C ASK NEW "DSPPRN" VALUE
C
10	CONTINUE
	TYPE*, IAM()
	CALL INPNUM('Enter new value for week counter: ',WCNT,0,100,ST)
	IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
	IF(WCNT.EQ.DSPPRN) THEN
	  TYPE*, IAM()
	  TYPE*, IAM(),'Current and new values for week counter are equal.'
	  CALL XWAIT(2,2,ST)
	  GOTO 10
	ENDIF
C
	TYPE*, IAM()
	CALL PRMYESNO('Do you want to modify current to new value for week counter [Y/N]? ',FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
C WRITE TOTOBOLA NORMAL GAME FILE
C
	DSPPRN=WCNT
	CALL WRITEW(FDB,DRAW,DSPREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
C
	TYPE*, IAM()
	TYPE*, IAM(),'Value for week counter modified to ',DSPPRN,' for draw ',DRAW
C
C CLOSE TOTOBOLA NORMAL GAME FILE AND EXIT
C
        CALL CLOSEFIL(FDB)
        CALL GSTOP(GEXIT_SUCCESS)
C
        END
