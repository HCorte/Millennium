C GAMINI.FOR
C
C V03 03-APR-2017 HXK ADD MENU FOR Joker Deactivation + 1X2 Cancel Rows/Draw
C V02 24-MAR-2017 FRP Modified for Joker Deactivation
C V01 13-DEC-2003 FRP Initial revision.
C
C PROGRAM TO UNSET GAME DRAWS
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
	PROGRAM GAMINI
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:DTGREC.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
C
	INTEGER*4 ST,FLAG
	INTEGER*4 GTYP,GIND,GNUM
	INTEGER*4 DRAW,DRW,CDC
        INTEGER*4 INICDC,ENDCDC,LSTCDC
	INTEGER*4 DFDB(7),GFDB(7),GBUF(1100)
	INTEGER*4 GAMSTS,GAMDRW,GAMBSD,GAMESD,GAMDAT(DATLEN)
        INTEGER*4 OPT
C
	EQUIVALENCE(DLTREC,DSPREC,DTGREC,DKKREC,GBUF)
	EQUIVALENCE(GBUF(1),GAMSTS)
	EQUIVALENCE(GBUF(4),GAMDRW)
	EQUIVALENCE(GBUF(5),GAMBSD)
	EQUIVALENCE(GBUF(6),GAMESD)
	EQUIVALENCE(GBUF(9),GAMDAT)
C
	COMMON SCFREC
C
C START
C
	CALL COPYRITE
C
        TYPE*, IAM()
	TYPE*, IAM(),'GAMINI'

        TYPE*, IAM()
        TYPE*, IAM(),'1.- Setup system for Joker deactivation'
        TYPE*, IAM(),'2.- Setup system to cancel sports events and draw'
        TYPE*, IAM(),'3.- Exit'
        TYPE*, IAM()

        CALL INPNUM('Enter option: ',OPT,1,3,ST)
        IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
	IF(OPT.EQ.2) THEN
            CALL SETUP_SPOTS_CANCEL_EVENTS
            CALL GSTOP(GEXIT_SUCCESS)
        ELSE
	    IF(OPT.NE.1)  CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C
C EXECUTING OPTION 1 - JOKER DEACTIVATION
C
	TYPE*, IAM()
	TYPE*, IAM(),'This option will initialize all draws'
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
C OPEN GAME FILE
C
	CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
	IF(GTYP.EQ.TLTO) CALL IOINIT(GFDB,3,DLTSEC*256)
	IF(GTYP.EQ.TSPT) CALL IOINIT(GFDB,3,DSPSEC*256)
	IF(GTYP.EQ.TTGL) CALL IOINIT(GFDB,3,DTGSEC*256)
	IF(GTYP.EQ.TKIK) CALL IOINIT(GFDB,3,DKKSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
C
C ASK FIRST DRAW TO INITIALIZE
C
200	CONTINUE
	CALL INPNUM('Enter first draw to initialize: ',DRAW,1,10000,ST)
	IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
	DRW=DRAW
C
C INITIALIZE GAME AND DAF FILES (ONLY FOR THIS GAME AND FROM THIS DRAW)
C
300	CONTINUE
	CALL READW(GFDB,DRW,GBUF,ST)
	IF(ST.EQ.144) THEN
	  TYPE*,IAM(), 'Last draw initialized - ',DRW-1
	  GOTO 999
	ENDIF
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRW)
C
	IF(GAMSTS.GT.GAMOPN) THEN
	  TYPE*,'Game already closed for draw ',DRW
	  GOTO 200
	ENDIF
C
        INICDC=GAMBSD
        ENDCDC=GAMESD
        IF(INICDC.EQ.0 .OR. ENDCDC.EQ.0) THEN
          INICDC=LSTCDC+1
          ENDCDC=INICDC+10000
        ENDIF
C
	DO CDC=INICDC,ENDCDC
	  CALL READW(DFDB,CDC,DAFREC,ST)
          IF(ST.EQ.144) THEN
            TYPE*, IAM(), 'Last draw initialized - ',DRW
            GOTO 999
          ENDIF
	  IF(ST.NE.0 .AND. DAFSTS.GT.DSOPEN .AND.
     *       DAFSTS.LE.DSKILL) CALL FILERR(SCFSFN(1,DAF),2,ST,CDC)
	  IF(DAFSTS.GT.DSKILL) GOTO 999  !any other value treated like EOF
C
          IF(DAFTYP(DOLAMT,1,GNUM).GT.0) THEN
             TYPE*,'ERROR: Sales found for draw:',DRW,', cdc:',CDC
             TYPE*,'Aborting process'
             GOTO 999
          ELSEIF(DAFTYP(DOLAMT,2,GNUM).GT.0) THEN
             TYPE*,'ERROR: Cancels found for draw:',DRW,', cdc:',CDC 
             TYPE*,'Aborting process'
             GOTO 999
          ENDIF
C
	  DAFDRW(GNUM)=0
	  DAFHDR(GNUM)=DRAW-1  !last draw that remains as set
C
	  CALL WRITEW(DFDB,CDC,DAFREC,ST)
	  IF(ST.NE.0 .AND. DAFSTS.GT.DSOPEN .AND.
     *       DAFSTS.LE.DSKILL) CALL FILERR(SCFSFN(1,DAF),3,ST,CDC)
	  IF(DAFSTS.GT.DSKILL) GOTO 999  !any other value treated like EOF
	ENDDO
C
        LSTCDC=GAMESD
C
	GAMSTS=0  !GAMNUL
	GAMDRW=0
	GAMBSD=0
	GAMESD=0
	CALL FASTSET(0,GAMDAT,DATLEN)
C
	CALL WRITEW(GFDB,DRW,GBUF,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
C
	DRW=DRW+1
	GOTO 300
C
C CLOSE FILES AND EXIT
C
999	CONTINUE
	CALL CLOSEFIL(DFDB)
	CALL CLOSEFIL(GFDB)
C
	CALL GSTOP(GEXIT_SUCCESS)
C
	END
C

C ******************************************************************************
C
C     SUBROUTINE: SETUP_SPOTS_CANCEL_EVENTS
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 03 / 04 / 2017
C
C ******************************************************************************
C
C FUNCTION GET THE CANCEL EVENT DATE AND TIME
C
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SETUP_SPOTS_CANCEL_EVENTS
        IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER * 4 GIND
        INTEGER * 4 NUM_GAM_SETUP
        INTEGER * 4 USER_ANSWER
C
	TYPE*, IAM()
	TYPE*, IAM(),'This option will setup all sports draws'
	TYPE*, IAM(),'for all the games with the configuration needed'
	TYPE*, IAM(),'to be able to cancel the draw events.'
C
	TYPE*, IAM()
	CALL PRMYESNO('Are you sure you want to continue [Y/N]? ',USER_ANSWER)
	IF(USER_ANSWER.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
        TYPE *, IAM()
C
        NUM_GAM_SETUP = 0
C
        DO GIND = 1, NUMSPT
           CALL SETUP_SPOTS_CANCEL_EVENTS_FOR_ONE_GAME(GTNTAB(TSPT, GIND), NUM_GAM_SETUP)
        ENDDO     
C
        IF(NUM_GAM_SETUP .LE. 0) THEN
          TYPE *, IAM()
          TYPE *, IAM(), 'Error, No Sports Games Have Been Updated ....'
          TYPE *, IAM()
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        TYPE *, IAM()
C
        END


C ******************************************************************************
C
C     SUBROUTINE: SETUP_SPOTS_CANCEL_EVENTS_FOR_ONE_GAME
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 03 / 04 / 2017
C
C ******************************************************************************
C
C FUNCTION GET THE CANCEL EVENT DATE AND TIME
C
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SETUP_SPOTS_CANCEL_EVENTS_FOR_ONE_GAME(GNUM, NUM_GAM_SETUP)
        IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DISKQIO.DEF'
C
        INTEGER * 4 GNUM 
        INTEGER * 4 NUM_GAM_SETUP
C
        INTEGER * 4 IDX
        INTEGER * 4 FSTS
        INTEGER * 4 FDB(7)
        INTEGER * 4 IDFIL
        INTEGER * 4 DRAW
        INTEGER * 4 MAX_DRAW
        INTEGER * 4 FIL_SECT_SIZE
        INTEGER * 4 ROW_NUM
        INTEGER * 4 MTX_IDX
C
        INTEGER * 4 NAME_TEAM(SPNMS_LEN / 4)
C
        CHARACTER * 16 TEAM_NAME
C
        EQUIVALENCE(NAME_TEAM, TEAM_NAME)
C
        IF(GNUM .LE. 0) RETURN
C
        IDFIL = 1
        DRAW  = 1
C
        CALL OPENW(IDFIL, GFNAMES(1,GNUM), 4, 0, 0, FSTS)
        CALL IOINIT(FDB, 1, DSPSEC * 256)
C
        IF(FSTS .NE. 0) THEN
          TYPE   *, IAM()
          TYPE 100, IAM(), (GFNAMES(IDX, GNUM), IDX = 1, 5), FSTS
          TYPE   *, IAM()
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        CALL VAXGETFSIZ(FDB(FDB_LUN), FIL_SECT_SIZE)
C
        MAX_DRAW = FIL_SECT_SIZE / (DSPSEC / 2 )
C
1000    CONTINUE
C
	CALL READW(FDB, DRAW, DSPREC, FSTS)
C
	IF(FSTS .NE. 0) THEN
          TYPE   *, IAM()
          TYPE 101, IAM(), (GFNAMES(IDX, GNUM), IDX = 1, 5), FSTS
          TYPE   *, IAM()
          CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
        IF((DSPSTS .LT. GAMCLD .OR. DSPSTS .GT. GFINAL) .AND. DRAW .NE. DSPDRW .AND. DRAW .GT. 5000) THEN
          CALL FASTSET(0, DSPREC, DSPLEN)
        ENDIF
C
        DSPMCE = 0    
        DSPDCD = 0
        DSPRWD = 0
C
        IF(DSPSTS .GT. 0) DSPMCE = 5 ! 5 EVENTS AS DEFAULT TO CANCEL THE FULL DRAW 
C
        CALL FASTSET(0, DSPECD, SPGNBR)
C
        IF(DSPSTS .GT. 0 .AND. DSPMAX .GT. 0) THEN
           DO ROW_NUM = 1, MIN(DSPMAX, SPGNBR)
           DO MTX_IDX = 1, 2
              IF(DSPNMS(1, MTX_IDX, ROW_NUM) .EQ. 0) THEN
                 IF(MTX_IDX .EQ. 1) WRITE(TEAM_NAME, 200) ROW_NUM 
                 IF(MTX_IDX .EQ. 2) WRITE(TEAM_NAME, 201) ROW_NUM 
                 CALL FASTMOV(NAME_TEAM, DSPNMS(1, MTX_IDX, ROW_NUM), SPNMS_LEN / 4)
              ENDIF
           ENDDO
           ENDDO
        ENDIF
C
	CALL WRITEW(FDB, DRAW, DSPREC, FSTS)
C
	IF(FSTS .NE. 0) THEN
          TYPE   *, IAM()
          TYPE 102, IAM(), (GFNAMES(IDX, GNUM), IDX = 1, 5), FSTS
          TYPE   *, IAM()
          CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
2000    CONTINUE
        DRAW = DRAW + 1
C
        IF(DRAW .LE. MAX_DRAW) GOTO 1000
C
        CALL USRCLOS1(IDFIL) 
C
        TYPE 103, IAM(), DRAW - 1, GNUM
C
        NUM_GAM_SETUP = NUM_GAM_SETUP + 1
C
100	FORMAT(X, A, 'Error Opening File:', X, 5A4, 'Status:', X, I)
101	FORMAT(X, A, 'Error Reading File:', X, 5A4, 'Status:', X, I)
102	FORMAT(X, A, 'Error Writting File:', X, 5A4, 'Status:', X, I)
103     FORMAT(X, A, I5, X, 'Draws Have Been Updated For Game', X, I2)
C
200     FORMAT('Home', X, I2.2, 9X)
201     FORMAT('Away', X, I2.2, 9X)
C
        END


