C PROGRAM ENABONUS
C  
C V04 16-JUN-2000 OXK Cleanup w/ WARNINGS=ALL
C V03 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V02 13 Jul 1995 HXK Changes for Viking Bonus
C V01 03 Jul 1995 HXK Initial revision.
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM ENABONUS
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
C
        INTEGER*4 SCFNAM(5)
        INTEGER*4 SCFFDB(7), ST, I, EXT, GIND, DRAW
        INTEGER*4 GNUM, K, FSTDRW, LSTDRW, FLAG, BONCNT, FDB(7)
        DATA         SCFNAM/'SCF.','FIL ',3*'    '/
C
        CALL COPYRITE
C
C READ SCF RECORD
C
        CALL OPENW(1,SCFNAM,4,0,0,ST)
        CALL IOINIT(SCFFDB,1,SCFSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)
        CALL READW(SCFFDB,1,SCFREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)
        CALL CLOSEFIL(SCFFDB)
        DO 10 I=1,MAXFIL
        IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))
10      CONTINUE
C
C GET OPTION
C
100     CONTINUE
        TYPE*,IAM(),'Enable/Disable Bonus Draws for LOTTO '       
C
C ENABLE/DISABLE BONUS DRAWS 
C
1000    CONTINUE
C
        CALL INPNUM('Enter game index ',GIND,1,MAXIND,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
        GNUM = SCFGTN(TLTO,GIND)
        IF(GNUM.LT.1) THEN
              TYPE*,IAM(),' Sorry, Lotto ',GIND,' not active '
              CALL GPAUSE
              GOTO 1000
        ENDIF
        WRITE(5,901) IAM(),GIND,(SCFLGN(K,GNUM),K=1,4)
1100    CONTINUE
C
C GET START AND END DRAWS
C
        CALL INPNUM('Enter start bonus draw ',FSTDRW,1,9999,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
        CALL INPNUM('Enter last  bonus draw ',LSTDRW,FSTDRW,9999,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C GET NUMBER OF BONUS DRAWINGS
C
        CALL INPNUM('Enter number of bonus drawings [enter 0 to disable] ',
     *               BONCNT,0,MAXBDR,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C ASK IF INFO ENTERED IS CORRECT
C
        CALL WIMG(5,'Do you want to update file with info entered Y/N ')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) CALL GSTOP (GEXIT_OPABORT)
C
C OPEN LOTTO FILE
C
        CALL OPENW(1,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,1,DLTSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
C
C UPDATE BONUS DRAW FIELD
C
        TYPE*,IAM(),'Updating lotto ',GIND,' file with bonus draw info '
        IF(BONCNT.EQ.0) THEN
           TYPE*,IAM(),'Disabling Bonus draw ... '
        ELSE
           TYPE*,IAM(),'Enabling Bonus draw ...'
        ENDIF
        DO 1200 DRAW = FSTDRW,LSTDRW
C
           CALL READW(FDB,DRAW,DLTREC,ST)
           IF(ST.EQ.144) THEN
             TYPE*,IAM(),'Last draw initialized - ',DRAW-1
             CALL CLOSEFIL(FDB)
             CALL XWAIT(2,2,ST)
             GOTO 100
           ENDIF
           IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
           IF(DLTSTS.GT.GAMOPN) THEN
             TYPE*,IAM(),'Game already closed for draw ',DRAW
             CALL GPAUSE
             CALL CLOSEFIL(FDB)
             GOTO 100
           ENDIF
C
           DLTBDR = BONCNT
C
           CALL WRITEW(FDB,DRAW,DLTREC,ST)
           TYPE*,IAM(),'Updating draw ',DRAW
           IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
C
1200    CONTINUE
        CALL CLOSEFIL(FDB)
        GOTO 100
C
C FORMAT SECTION
C
901     FORMAT(1X,A,1X,I2,3X,4A4)
        END
