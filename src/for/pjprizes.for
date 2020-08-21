C
C PROGRAM PJPRIZES
C
C PJprizes.FOR
C
C V01 28-AUG-2003 CPH
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2003 DJ - SCML. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C CREATES FILES PJMC_PM_GGWWYYYY.ASC
C

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM PJPRIZES
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'    
	
        INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'

C
        ! variables
	INTEGER*4  FLAG                    !
	INTEGER*4  DRAW                    !
	INTEGER*4  GNUM                    !
	INTEGER*4  GIND                    !
	INTEGER*4  EXT                     !
	INTEGER*4  GTYP                    !
	INTEGER*4  K,ST                    !
        INTEGER*4  FILE(5)                 !
C
C
        CALL CLRSCR(5)
        TYPE*,IAM()
        TYPE*,IAM(),'*******************************************************'
        TYPE*,IAM(),'                 INTERFACE WITH PORTAL                 '
        TYPE*,IAM(),'          GENERATES FILES OF PRIZES FOR DIVISION       '
        TYPE*,IAM(),'                 PJMC_PM_GGWWYYYY.ASC                  '
        TYPE*,IAM(),'            GG=GAME NUMBER/WW=WEEK/YYYY=YEAR           '
        TYPE*,IAM(),'*******************************************************'
        TYPE*,IAM()
C
C GET SYSTEM CONFIGURATION INFO.
C*******************************
        CALL GETSCONF(SCFREC,ST)
        IF(ST.NE.0) THEN
           TYPE*,IAM(),'Unable to get System Configuration info.'
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C WRITE GAME NUMBER MENU 
C******************************

100	CONTINUE
	WRITE(6,900) (K,GTNAMES(K),K=1,MAXTYP)
	CALL INPNUM('Enter Game type ',GTYP,1,MAXTYP,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	CALL INPNUM('Enter game index ',GIND,1,MAXIND,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C VERIFIES IF GAME NUMBER ENTERED IS VALID
C******************************************
	GNUM=SCFGTN(GTYP,GIND)
	IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
	    TYPE*,IAM(),'>>> Sorry, game selected is not active <<<'
	    GOTO 100
	ENDIF
        CALL FASTMOV(SCFGFN(1,GNUM),FILE,5)
C
C WE DONT WANT TO GENERATE FILES FOR THESE GAMES
C**********************************************
	IF(GTYP.EQ.TINS.OR. GTYP.EQ.TTGL.OR. GTYP.EQ.TPAS) THEN
            WRITE(6,905) (GTNAMES(GTYP))
            GOTO 100
        ENDIF
C
C ASK DRAW 
C**********
        DRAW=DAYDRW(GNUM)
        TYPE *,IAM(),DRAW,' -> CURRENT DRAW'

        CALL INPNUM('Enter draw number [C-current draw]: ',DRAW,1,99999,EXT)
        IF(EXT.LT.0.AND.EXT.NE.-5) GOTO 100
C
         IF(DRAW .LE. 0) THEN
             TYPE *,IAM()
             TYPE *,IAM(),'Sorry, Incorrect draw number entered: ', DRAW
             TYPE *,IAM()
             GOTO 100
          ENDIF
C
C ASK FOR OPERATOR TO CONFIRM DATA
C*********************************
	WRITE(6,910) GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),DRAW
	CALL INPYESNO('Is this correct (Y/N) ',FLAG)
	IF(FLAG.NE.1) GOTO 100
C
110     CONTINUE

	IF(GTYP.EQ.TLTO) THEN 
	   CALL PJLTOPRIZES(FILE,DRAW,GNUM,GIND)
        ELSEIF(GTYP.EQ.TKIK) THEN 
	   CALL PJKIKPRIZES(FILE,DRAW,GNUM,GIND)
        ELSEIF(GTYP.EQ.TSPT) THEN 
	   CALL PJSPTPRIZES(FILE,DRAW,GNUM,GIND)
	ELSE
	   TYPE*,IAM(),' Invalid game type ',GTYP
	   GOTO100
	ENDIF

        GOTO100
C
C
900     FORMAT(/, <MAXTYP>(1X,I2,' - ',A8,/),2X,'E - EXIT',/)
905	FORMAT(1X,'>>> Sorry, not applicable to ',A8,' <<<'/)
910	FORMAT(1X,'>>>'1X,A8,1X,I1,2X,4A4,'Draw: ',I5,' <<<'/)

	END
