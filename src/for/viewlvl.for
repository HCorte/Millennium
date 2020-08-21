C
C SUBROUTINE VIEWLVL
C $Log:   GXAFXT:[GOLS]VIEWLVL.FOV  $
C  
C     Rev 1.1   19 May 1996 17:46:28   HXK
C  Wojtek's security stuff added
C  
C     Rev 1.0   21 Jan 1993 18:01:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_viewlvl.for **
C
C VIEWLVL.FOR
C
C V02 20-JAN-92 GCAN INCREASED VISION PASS# FIELD TO 3.
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE VIEWLVL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DESLVL.DEF'
C
        INTEGER*4   LVLIM
        PARAMETER   (LVLIM=168)
C
        INTEGER*4   I, J, K, ST, PROG, LEVEL, CNT
        INTEGER*4   LVL_NO, ANS
        CHARACTER   FLINE*80, SPACES*80
        CHARACTER   FNAM(5)*25
        CHARACTER*5 LEV(0:15,5,LVLIM)
	LOGICAL     OPNIT
        DATA FNAM(1)/'    VISION SNAPSHOTS     '/
        DATA FNAM(2)/'    VISION COMMANDS      '/
        DATA FNAM(3)/' AGENT FILE MAINTENANCE  '/
        DATA FNAM(4)/' USER FILE MAINTENANCE   '/
        DATA FNAM(5)/'       CHGLVL FILE       '/
C
C Initializing
C
	OPNIT=.TRUE.
        I=0
        DO 5 I=1,80
          SPACES(I:I)=' '
          FLINE(I:I)=' '
5       CONTINUE
        J=0
        K=0
        DO 30 J=0,15
          DO 20 I=1,5
            DO 10 K=1,LVLIM
              LEV(J,I,K)='     '
10          CONTINUE
20        CONTINUE
30      CONTINUE
C
C Ask fof program name/number and level
C
100     CALL CLRSCR(5)
        WRITE(5,930)
        CALL INPNUM('Please enter program number   : ',PROG,1,5,ST)
        IF(ST.LT.0) THEN
	  GOTO 200
        ENDIF
C 
        CALL INPNUM('Please enter level number     : ',LEVEL,1,15,ST)
        IF(ST.LT.0) THEN
	  GOTO 200
        ENDIF
C
C Open LEVEL file
C
	IF(OPNIT) THEN
          OPEN(9,FILE='GXPROJ:[TSK]LEVEL.FIL',IOSTAT=ST,
     *       STATUS='OLD',RECL=80, BLOCKSIZE=512,READONLY,
     *       ORGANIZATION='SEQUENTIAL',
     *       ACCESS='SEQUENTIAL')
          IF(ST.NE.0) THEN
             WRITE(5,910)
	     GOTO 200
          ENDIF
	  OPNIT=.FALSE.
	ELSE
	  GOTO 65
	ENDIF
C
C Read level file to array lev(level,program,1-168)
C
50      CONTINUE
        READ(9,920,END=100) FLINE
        IF(FLINE(1:2).EQ.'/*') GOTO 65
        IF(FLINE(1:4).EQ.'****') THEN
           LVL_NO=CTOI(FLINE(5:6),K)
           CNT = 0
           GOTO 50
        ENDIF
        CALL ENCLVL(LVL_NO,FLINE)
        CNT = CNT + 1   
        LEV(LVL_NO,1,CNT)=FLINE(1:3)              ! vision snap
        LEV(LVL_NO,2,CNT)=FLINE(5:9)              ! vision comm
        LEV(LVL_NO,3,CNT)=FLINE(11:12)            ! hasf
        LEV(LVL_NO,4,CNT)=FLINE(14:15)            ! userpass
        LEV(LVL_NO,5,CNT)=FLINE(17:18)            ! chglvl
        GOTO 50
C
C Display header
C
65	CONTINUE
        CALL CLRSCR(5)
        WRITE(5,940) FNAM(PROG),LEVEL
C
C Print out the selected level information.
C
C
         DO 60 J=1,LVLIM,12
              WRITE(5,950)LEV(LEVEL,PROG,J),LEV(LEVEL,PROG,J+1),
     *                LEV(LEVEL,PROG,J+2),LEV(LEVEL,PROG,J+3),
     *                LEV(LEVEL,PROG,J+4),LEV(LEVEL,PROG,J+5),
     *                LEV(LEVEL,PROG,J+6),LEV(LEVEL,PROG,J+7),
     *                LEV(LEVEL,PROG,J+8),LEV(LEVEL,PROG,J+9),
     *                LEV(LEVEL,PROG,J+10),LEV(LEVEL,PROG,J+11)
60        CONTINUE
C
C More ?
C
        CALL WIMG(5,'Do you want to see another level ? [Y/N] ')
        CALL YESNO(ANS)
        IF(ANS.EQ.1) GOTO 100
C
C Exit
C
200     CONTINUE
        CALL USRCLOS1(9)
        CALL XWAIT(1,2,ST)
        CALL CLRSCR(5)
        RETURN
C
C
910     FORMAT(10X,'The LEVEL file is not on this pack.')
920     FORMAT(A80)
930     FORMAT(///21X,'<<<     DISPLAY LEVEL    >>>',/,
     *            21X,'                            ',/,
     *            21X,'                            ',/,
     *            21X,'    1.  Vision Snapshots    ',/,
     *            21X,'    2.  Vision Commands     ',/,
     *            21X,'    3.  Hasf Selection      ',/,
     *            21X,'    4.  User Password       ',/,
     *            21X,'    5.  Change Level        ',/,
     *            21X,'                            ',/,
     *            21X,'     Level Range: 0 - 15    ',/,
     *            21X,'                            ',/,
     *            21X,'         E - Exit           ',///)
940   FORMAT(///,28X,A25,//,5X,'Level ',I2.2,' contains the following',
     *       ' information......',/)
950     FORMAT(12(1X,A5))
        END
