C
C SUBROUTINE BUILDLEV
C $Log:   GXAFIP:[GOLS]BUILDLEV.FOV  $
C  
C     Rev 1.6   14 Feb 1997 14:48:54   RXK
C  Change of PRMSTR allowed 
C  
C     Rev 1.5   28 Jan 1997 17:54:26   RXK
C  Update values of TIERLIM,GVTSUP,FWDCNT,PASSEXP allowed
C  
C     Rev 1.4   13 Jan 1997 16:35:32   RXK
C  GVT id online change allowed 
C  
C     Rev 1.1   19 May 1996 17:44:48   HXK
C  Wojtek's security stuff added
C  
C     Rev 1.2   12 Dec 1995 15:08:42   PXB
C  Changed for new menu in vision
C  
C     Rev 1.1   11 Jun 1993 17:16:22   HXK
C  ADDED AGTINF.DEF, PRMAGT.DEF
C  
C     Rev 1.0   21 Jan 1993 15:46:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_buildlev.for **
C
C BUILDLEV.FOR
C
C V03 25-APR-92 WLM  MADE IT RUN
C V02 25-OCT-91 GCAN INCREASED TO 120 SCREENS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C
C             CALLING SEQUENCE :
C                             INPUT  - INDEX - program index
C                             OUTPUT - STATUS- return status
C                                      1 - default levels
C                                      0 - group levels
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE BUILDLEV(INDEX,STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:RECUSE.DEF'
        INCLUDE 'INCLIB:PRMLVL.DEF'
        INCLUDE 'INCLIB:PRMHSF.DEF'
        INCLUDE 'INCLIB:DESLVL.DEF'
C
        INTEGER*4 LVLIM
        PARAMETER (LVLIM=168)
C
        INTEGER*4 STATUS,INDEX, KT, TIND, IND, ST, J, I
        INTEGER*4 UERR, UNUM, HERR, HNUM, COERR, CONUM, SERR, SNUM
        INTEGER*4 LERR, LNUM
        INTEGER*4 BUF(20),CNUMM
C
        CHARACTER*80 CBUF
        EQUIVALENCE (BUF,CBUF)
C
        INTEGER*4 DEFS(LVLIM),DEFC(LVLIM)
        INTEGER*4 DEFM(LVLIM),DEFU(LVLIM),DEFL(LVLIM)
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
     *               02020,02021,02022,02023,04002,03002,03003,03004,
     *               03005,04001,04002,04003,04006,06001,06002,06003,
     *               06004,06005,06006,06007,06009,06010,06011,06012,
     *               06013,06015,06016,06017,06018,11003,11004,11005,
     *               07010,17008,17006,19001,19003,19004,19005,19006,
     *               19007,19008,15002,20002,21002,10001,10002,10004,
     *               10005,01372,01373,01374,01375,06008,01376,01377,
     *               01378,01379,01380,01383,01384,01386,01387,01388,
     *               01399,01381,23002,24002,44*00000/
C
C Default menus   (hasf)
C
        DATA DEFM/01,2,3,4,5,6,7,8,9,10,11,12,13,14,15,153*0/
C
C Default user levels   (userpass)
C
        DATA DEFU/1,2,3,4,5,6,7,161*0/
C
C Default menus  (chglv)
C
        DATA DEFL/1,2,3,4,5,163*0/
C
C Initialize group level array
C
        DO 10 I=1,16
          LEVELS(I)=0
 10     CONTINUE
C
C Initialize program level arrays  VISION
C
        IF(INDEX.EQ.VINDEX) THEN
          DO 12 I=1,16
          DO 12 J=1,LVLIM
              SNAPS(J,I)=0
              CMDS(J,I)=0
 12       CONTINUE
        ENDIF
C
C Initialize program level arrays  HASF
C
        IF(INDEX.EQ.HINDEX) THEN
          DO 14 I=1,16
          DO 14 J=1,LVLIM
            HMENUS(J,I)=0
 14       CONTINUE
        ENDIF
C
C Initialize program level arrays   USERPASS
C
        IF(INDEX.EQ.UINDEX) THEN
          DO 16 I=1,16
          DO 16 J=1,LVLIM
            USERS(J,I)=0
 16       CONTINUE
        ENDIF
C
C Initialize program level arrays  CHGLVL
C
        IF(INDEX.EQ.LINDEX) THEN
          DO 18 I=1,16
          DO 18 J=1,LVLIM
            LEVMEN(J,I)=0
 18       CONTINUE
        ENDIF
C
C if LEVEL file not on pack  default screens and commands
C
500     OPEN(1,FILE='LEVEL.FIL',IOSTAT=ST,
     *       STATUS='OLD',RECL=80, BLOCKSIZE=512,READONLY,
     *       ORGANIZATION='SEQUENTIAL',
     *       ACCESS='SEQUENTIAL',SHARED)
        IF(ST.NE.0) THEN
           TYPE *,'ERROR OPENING FILE LEVEL.FIL WILL USE LEVEL DEFAULTS'
           CALL XWAIT(1,2,ST)
           CALL CLRSCR(5)
C
C Fill in default vision screens and commands
C
           IF(INDEX.EQ.VINDEX) THEN
              DO 20 I=1,LVLIM
                SNAPS(I,1)=DEFS(I)
                CMDS(I,1)=DEFC(I)
CWXW            CMDS(I,1)=-1            !*V03* BACKWARD COMPATIBILITY
 20           CONTINUE
           ENDIF
C
C Fill in default hasf menus
C
           IF(INDEX.EQ.HINDEX) THEN
               DO 22 I=1,LVLIM
                 HMENUS(I,1)=DEFM(I)
 22            CONTINUE
           ENDIF
C
C Fill in userpass menu numbers
C
           IF(INDEX.EQ.UINDEX) THEN
               DO 24 I=1,LVLIM
                 USERS(I,1)=DEFU(I)
 24            CONTINUE
           ENDIF
C
C Fill in chglvl menu numbers
C
           IF(INDEX.EQ.LINDEX) THEN
               DO 26 I=1,LVLIM
                 LEVMEN(I,1)=DEFL(I)
 26            CONTINUE
           ENDIF
C
           STATUS=1
           RETURN
        ENDIF
C
C Fill in security levels with appropriate screens and commands
C
        IND=0
        TIND = 0
30      CONTINUE
        READ(1,901,END=100) CBUF
        IF(CBUF(1:2).EQ.'/*') GOTO 100
C
C Fill in levels  and shut off times
C
        IF(CBUF(1:4).EQ.'****') THEN
          IND=IND+1
          TIND=0
          IF(IND.GT.16) RETURN
          LEVELS(IND)=CTOI(CBUF(5:6),KT)
C         TYPE *,'FOUND LEVEL ',LEVELS(IND)
901       FORMAT(A80)
          GOTO 30
        ENDIF
C
C Get snapshot and commands
C
        TIND=TIND+1
        IF(TIND.GT.LVLIM) GOTO 30
        CALL ENCLVL(LEVELS(IND),CBUF)
        DO 40 I=1,12
        IF(CBUF(I:I).EQ.' ') CBUF(I:I)='0'
40      CONTINUE
C
C Fill in vision screens and command numbers
C
        IF(INDEX.EQ.VINDEX) THEN
          CALL ASCBIN(BUF,1,3,SNUM,SERR)
          CALL ASCBIN(BUF,5,5,CONUM,COERR)
          SNAPS(TIND,IND)=SNUM
          CMDS(TIND,IND)=CONUM
        ENDIF
C
C Fill in hasf menu numbers
C
        IF(INDEX.EQ.HINDEX) THEN
          CALL ASCBIN(BUF,11,2,HNUM,HERR)
          HMENUS(TIND,IND)=HNUM
        ENDIF
C
C Fill in userspass menu numbers
C
        IF(INDEX.EQ.UINDEX) THEN
          CALL ASCBIN(BUF,14,2,UNUM,UERR)
          USERS(TIND,IND)=UNUM
        ENDIF
C
C Fill in chglvl menu numbers
C
        IF(INDEX.EQ.LINDEX) THEN
          CALL ASCBIN(BUF,17,2,LNUM,LERR)
          LEVMEN(TIND,IND)=LNUM
        ENDIF
C
        GOTO 30
C
100     CONTINUE
        CALL USRCLOS1(1)
        RETURN
        END
