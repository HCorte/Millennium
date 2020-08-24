C  GXSRC:ADDLVL.FOR
C  
C  $Log:   GXAFXT:[GOLS]ADDLVL.FOV  $
C  
C     Rev 1.1   19 May 1996 17:52:42   HXK
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    19 May 1996 17:48:52   HXK
C  Initial revision.
C  
C
C SUBROUTINE ADDLVL
C  
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_altrlvl.for **
C
C ADDLVL.FOR
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
	SUBROUTINE ADDLVL
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
	INTEGER*4   LVL_NO, NUM, NUM1, NUM2, NUM3
	CHARACTER   FLINE*80, SPACES*80
	CHARACTER   CNUM*5, CNUM1*5, CNUM2*5, COMMA
	CHARACTER*5 LEV(0:15,5,LVLIM)
        CHARACTER*25 FNAM(5)
        DATA FNAM(1)/'    VISION SNAPSHOTS     '/
        DATA FNAM(2)/'    VISION COMMANDS      '/
        DATA FNAM(3)/' AGENT FILE MAINTENANCE  '/
        DATA FNAM(4)/' USER FILE MAINTENANCE   '/
        DATA FNAM(5)/'       CHGLVL FILE       '/
	DATA COMMA/','/
C
C Initializing
C
	I=0
	DO 5 I=1,80
	  SPACES(I:I)=' '
	  FLINE(I:I)=' '
5	CONTINUE
	J=0
	K=0
	DO 30 J=0,15
	  DO 20 I=1,5
	    DO 10 K=1,LVLIM
	      LEV(J,I,K)='00000'
10	    CONTINUE
20	  CONTINUE
30	CONTINUE
C
C Ask fof program name/number and level
C
100	CALL CLRSCR(5)
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
	OPEN(9,FILE='LEVEL.FIL',IOSTAT=ST,
     *       STATUS='OLD',RECL=80, BLOCKSIZE=512,
     *       ORGANIZATION='SEQUENTIAL',
     *       ACCESS='SEQUENTIAL',SHARED)
	IF(ST.NE.0) THEN
	  WRITE(5,910)
	  GOTO 200
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
        LEV(LVL_NO,1,CNT)=FLINE(1:3)		  ! vision snap
        LEV(LVL_NO,2,CNT)=FLINE(5:9)		  ! vision comm
        LEV(LVL_NO,3,CNT)=FLINE(11:12)		  ! hasf
        LEV(LVL_NO,4,CNT)=FLINE(14:15)		  ! userpass
        LEV(LVL_NO,5,CNT)=FLINE(17:18)  	  ! chglvl
	GOTO 50
C
C Display header
C
65	CONTINUE
	CLOSE(9)
        CALL CLRSCR(5)
        WRITE(5,940) FNAM(PROG),LEVEL
C
C Print out the selected level information.
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
C Set variables
C
        IF(PROG.EQ.1) THEN           !Vision snap
            NUM1=3
            NUM2=999
            NUM3=000
        ELSEIF(PROG.EQ.2) THEN       !Vision commands
            NUM1=5
            NUM2=99999
            NUM3=00000
        ELSEIF(PROG.EQ.3) THEN       !Hasf
            NUM1=2
            NUM2=13
            NUM3=00
        ELSEIF(PROG.EQ.4) THEN       !Userpass
            NUM1=2
            NUM2=7
            NUM3=00
        ELSEIF(PROG.EQ.5) THEN       !Chglvl
            NUM1=2
            NUM2=5
            NUM3=00
        ENDIF
C
C Inquire number to add
C
	NUM = 0
        CALL INPNUM('Please enter number     : ',NUM,1,NUM2,ST)
        IF(ST.LT.0) THEN
	  GOTO 200
	ENDIF
C
C Convert given number and fill it with zeros, cnum1 has only zeros     
C
	CALL PAD0(CNUM,NUM,NUM1,'0')
C
        CALL PAD0(CNUM1,NUM3,NUM1,'0')
C
C Make sure number is not in already the file and find next available loc
C
	DO 70 I=1,LVLIM
          IF(LEV(LEVEL,PROG,I).EQ.CNUM) THEN
	    CALL CLRSCR(5)
            WRITE(5,960) CNUM
            CALL XWAIT(1,2,ST)
	    CALL USRCLOS1(9)
            GOTO 100
	  ELSEIF(LEV(LEVEL,PROG,I).EQ.CNUM1) THEN
	    LEV(LEVEL,PROG,I)=CNUM
	    GOTO 80
	  ENDIF
70	CONTINUE
C
C Get to the beginning of the file and write new info along with the old
C
80	CONTINUE
C
C Open LEVEL file
C
	OPEN(9,FILE='LEVEL.FIL',IOSTAT=ST,
     *       STATUS='OLD',RECL=80, BLOCKSIZE=512,
     *       ORGANIZATION='SEQUENTIAL',
     *       ACCESS='SEQUENTIAL',SHARED)
	IF(ST.NE.0) THEN
	  WRITE(5,910)
	  GOTO 200
	ENDIF
        DO 90 I=15,1,-1
          FLINE(1:80)=SPACES
          FLINE(1:4)='****'
          CALL PAD0(CNUM2,I,2,' ')
          FLINE(5:6)=CNUM2(1:2)
          FLINE(7:7)=COMMA
          WRITE(9,920)FLINE
          DO 110 J=1,LVLIM
            FLINE(1:80)=SPACES
            FLINE(1:3)=LEV(I,1,J)       !write vision snap
            FLINE(4:4)=COMMA
            FLINE(5:9)=LEV(I,2,J)       !write vision commands
            FLINE(10:10)=COMMA
            FLINE(11:12)=LEV(I,3,J)     !write hasf
            FLINE(13:13)=COMMA
            FLINE(14:15)=LEV(I,4,J)     !write userpass
            FLINE(16:16)=COMMA
            FLINE(17:18)=LEV(I,5,J)     !write chglvl
            CALL ENCLVL(I,FLINE)
            WRITE(9,920)FLINE
110       CONTINUE
90      CONTINUE
C
        FLINE(1:80)=SPACES
        FLINE(1:2)='/*'
        WRITE(9,920)FLINE
	CLOSE(9)
	CALL CLRSCR(5)
        WRITE(5,970) CNUM,LEVEL
	CALL XWAIT(1,2,ST)
	GOTO 100
C
C Exit
C
200     CONTINUE
        CALL XWAIT(1,2,ST)
        CALL CLRSCR(5)
        CALL USRCLOS1(9)
        RETURN
C
C
910	FORMAT(10X,'The LEVEL file is not on this pack.')
920	FORMAT(A80)
930     FORMAT(///21X,'<<<       ADD LEVEL      >>>',/,
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
960     FORMAT(///1X,' Number  ',A5,' already in the file')
970     FORMAT(///1X,' Number  ',A5,' added to level ',I2.2)
        END
