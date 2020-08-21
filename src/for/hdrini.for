C HDRINI.FOR
C
C V01 25-NOV-2010 FRP Initial revision.
C
C PROGRAM TO UNSET GAME HIGH DRAWS
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
	OPTIONS /CHECK=NOOVERFLOW
	PROGRAM HDRINI
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
C
	INTEGER*4 ST,FLAG
	INTEGER*4 GTYP,GIND,GNUM
        INTEGER*4 CDC,INICDC
	INTEGER*4 DFDB(7)
C
	COMMON SCFREC
C
C START
C
	CALL COPYRITE
C
	TYPE*, IAM()
	TYPE*, IAM(),'This program will initialize all HIGH draws'
	TYPE*, IAM(),'that are already set for a certain game,'
	TYPE*, IAM(),'starting from the user input draw.'
C
	TYPE*, IAM()
	CALL PRMYESNO('Are you sure you want to continue [Y/N]? ',FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
C READ SCF
C
	CALL GETSCONF(SCFREC,ST)
C
C ASK GAME
C
100	CONTINUE
	TYPE*
	CALL INPNUM('Enter game type: ',GTYP,1,MAXTYP,ST)
	IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
	CALL INPNUM('Enter game index: ',GIND,1,MAXIND,ST)
	IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
	GNUM=SCFGTN(GTYP,GIND)
	IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
	  TYPE*,'Sorry, selected game not active'
	  CALL XWAIT(2,2,ST)
	  GOTO 100
	ENDIF
C
C OPEN DAF
C
	CALL OPENW(2,SCFSFN(1,DAF),4,0,0,ST)
	CALL IOINIT(DFDB,2,DAFSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFSFN(1,DAF),1,ST,0)
C
C ASK FIRST CDC TO INITIALIZE
C
200	CONTINUE
	CALL INPNUM('Enter first CDC to initialize: ',INICDC,1,4000,ST)
	IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
	CDC=INICDC
C
C INITIALIZE DAF FILE (ONLY FOR THIS GAME AND FROM THIS DRAW)
C
300	CONTINUE
        CALL READW(DFDB,CDC,DAFREC,ST)
	IF(ST.NE.0 .AND. DAFSTS.GT.DSOPEN .AND.
     *     DAFSTS.LE.DSKILL) CALL FILERR(SCFSFN(1,DAF),2,ST,CDC)
	IF(DAFSTS.GT.DSKILL) GOTO 999  !any other value treated like EOF
C
	DAFDRW(GNUM)=0
	DAFHDR(GNUM)=-1
C
	CALL WRITEW(DFDB,CDC,DAFREC,ST)
	IF(ST.NE.0 .AND. DAFSTS.GT.DSOPEN .AND.
     *     DAFSTS.LE.DSKILL) CALL FILERR(SCFSFN(1,DAF),3,ST,CDC)
	IF(DAFSTS.GT.DSKILL) GOTO 999  !any other value treated like EOF
C
	CDC=CDC+1
	GOTO 300
C
C CLOSE FILES AND EXIT
C
999	CONTINUE
	CALL CLOSEFIL(DFDB)
C
	CALL GSTOP(GEXIT_SUCCESS)
	END
C
