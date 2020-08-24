C
C PROGRAM SETLVL
C  
C SETLVL.FOR
C
C V02 26-MAY-1999 UXN SUPER TRIPLE CHANGES.
C V01 14-JUL-1996 HXK Initial revision.
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
        PROGRAM SETLVL
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
        INTEGER*4   I, J, K, ST, PROG, LEVEL, CNT, ANS
        INTEGER*4   LVL_NO, NUM, NUM1, NUM2, NUM3
        INTEGER*4   DEFS(LVLIM),DEFC(LVLIM)
        INTEGER*4   DEFM(LVLIM),DEFU(LVLIM),DEFL(LVLIM)
        INTEGER*4   MAS_DEF(LVLIM)
C
        CHARACTER   FLINE*80, SPACES*80
        CHARACTER   CNUM*5, CNUM2*5, COMMA
        CHARACTER*5 LEV(0:15,5,LVLIM)
        CHARACTER*8 PASPAS
        CHARACTER*20 PASENT
        EQUIVALENCE (PASPAS,PASENT)
C
        CHARACTER*25 FNAM(5)
C
        DATA FNAM(1)/'    VISION SNAPSHOTS     '/
        DATA FNAM(2)/'    VISION COMMANDS      '/
        DATA FNAM(3)/' AGENT FILE MAINTENANCE  '/
        DATA FNAM(4)/' USER FILE MAINTENANCE   '/
        DATA FNAM(5)/'       CHGLVL FILE       '/
        DATA COMMA/','/
C
C
C Default screens   (vision)
C
        DATA  DEFS/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
     *             20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,
     *             37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,
     *             54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,
     *             71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,
     *             88,89,90,91,92,93,94,95,96,97,98,99,
     *             100,101,102,103,104,105,106,107,108,109,110,111,112,
     *             113,114,115,116,117,118,119,120,
     *             121,122,123,124,125,126,127,128,129,130,131,132,133,
     *             134,135,136,137,138,139,140,141,142,143,144,145,146,
     *             147,148,149,150,151,152,153,154,155,156,157,158,159,
     *             160,161,162,163,164,165,166,167,168/
C
C Default commands (vision)
C
        DATA    DEFC/01012,01013,01014,01002,02001,01004,01003,01304,
     *               01305,01307,01041,01306,01316,01341,01023,01022,
     *               01312,01339,01360,01328,01348,01347,01355,01350,
     *               01345,01363,01356,01351,01047,01049,01050,01052,
     *               01053,01048,01366,01041,01060,01042,01062,01043,
     *               01064,02003,02004,01365,01336,01338,01342,01337,
     *               01335,01343,01344,01364,01058,02009,02011,02010,
     *               04002,03002,03003,03004,03005,04001,04002,04003,
     *               04006,06001,06002,06003,06004,06005,06006,11003,
     *               11004,11005,07010,17008,17006,19001,19003,19004,
     *               19005,19006,19007,19008,15002,20002,21002,10001,
     *               10002,10004,10005,01372,01373,01374,01375,06008,
     *               01376,01379,01380,01383,01384,01386,01387,01388,
     *               01399,01381,23002,24002,60*00000/
C
C Default menus   (hasf)
C
        DATA DEFM/01,2,3,4,5,6,7,8,9,10,11,12,13,155*0/
C
C Default user levels   (userpass)
C
        DATA DEFU/1,2,3,4,5,6,7,161*0/
C
C Default menus  (chglv)
C
        DATA DEFL/1,2,3,4,164*0/
C
C Initializing
C
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
              LEV(J,I,K)='00000'
10          CONTINUE
20        CONTINUE
30      CONTINUE
C
C Ask for password
C
        CALL PASSWORD(5,PASENT)
        IF(PASPAS.NE.'EMISSION') THEN
          TYPE*,' ********** ACCESS DENIED SORRY **********'
          TYPE*,' YOU MUST HAVE CORRECT PASSWORD FOR ACCESS'
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
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
        OPEN(9,FILE='GXPROJ:[TSK]LEVEL.FIL',IOSTAT=ST,
     *       STATUS='OLD',RECL=80, BLOCKSIZE=512,
     *       ORGANIZATION='SEQUENTIAL',
     *       ACCESS='SEQUENTIAL')
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
        LEV(LVL_NO,1,CNT)=FLINE(1:3)              ! vision snap
        LEV(LVL_NO,2,CNT)=FLINE(5:9)              ! vision comm
        LEV(LVL_NO,3,CNT)=FLINE(11:12)            ! hasf
        LEV(LVL_NO,4,CNT)=FLINE(14:15)            ! userpass
        LEV(LVL_NO,5,CNT)=FLINE(17:18)            ! chglvl
        GOTO 50
C
C Display header
C
65      CONTINUE
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
            DO 35 J=1, LVLIM
              MAS_DEF(J)=DEFS(J)
35          CONTINUE
        ELSEIF(PROG.EQ.2) THEN       !Vision commands
            NUM1=5
            NUM2=99999
            NUM3=00000
            DO 36 J=1, LVLIM
              MAS_DEF(J)=DEFC(J)
36          CONTINUE
        ELSEIF(PROG.EQ.3) THEN       !Hasf
            NUM1=2
            NUM2=13
            NUM3=00
            DO 37 J=1, LVLIM
              MAS_DEF(J)=DEFM(J)
37          CONTINUE
        ELSEIF(PROG.EQ.4) THEN       !Userpass
            NUM1=2
            NUM2=7
            NUM3=00
            DO 38 J=1, LVLIM
              MAS_DEF(J)=DEFU(J)
38          CONTINUE
        ELSEIF(PROG.EQ.5) THEN       !Chglvl
            NUM1=2
            NUM2=5
            NUM3=00
            DO 39 J=1, LVLIM
              MAS_DEF(J)=DEFL(J)
39          CONTINUE
        ENDIF
C
C Set to default values
C
        CALL WIMG(5,' Press RETURN to continue ........')
        READ(5,970) ANS
C
        DO 70 I=1,LVLIM
          NUM = 0
          NUM = MAS_DEF(I)
          CALL PAD0(CNUM,NUM,NUM1,'0')
          LEV(LEVEL,PROG,I) = CNUM
70      CONTINUE
C
C Get to the beginning of the file and write new info along with the old
C
        REWIND(9)
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
        CALL CLRSCR(5)
        WRITE(5,960) LEVEL,FNAM(PROG)
        CALL XWAIT(2,2,ST)
        CALL USRCLOS1(9)
        GOTO 100
C
C Exit
C
200     CONTINUE
        CALL XWAIT(1,2,ST)
        CALL CLRSCR(5)
        CALL USRCLOS1(9)
        CALL GSTOP(GEXIT_SUCCESS)
C
C
910     FORMAT(10X,'The LEVEL file is not on this pack.')
920     FORMAT(A80)
930     FORMAT(///21X,'<<<      SET  LEVEL      >>>',/,
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
960     FORMAT(///1X,' LEVEL ',I2.2,' DEFAULT VALUES SET FOR',A25)
970     FORMAT(A4)
        END
