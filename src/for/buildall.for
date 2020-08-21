C  GXSRC:BUILDALL.FOR
C  
C V12 31-MAR-2016 SCML M16 PROJECT: added the following default commands
C                      relative to vision commands of EUROMIL snapshot:
C                      01466,01467,01468,01469,01470,01474,01475,01493,01496,
C                      01497,01498,01499 and 01500
C V11 20-JUL-2015 SCML Adding support for IGS internal cancel flags 
C V10 03-APR-2014 SCML Placard Project
C V09 22-NOV-2010 FJG  ePassive
C V08 01-JAN-2010 FJG  ePassive
C
C  $Log:   GXAFIP:[GOLS]BUILDALL.FOV  $
C  
C     Rev 1.7   14 Feb 1997 14:48:50   RXK
C  Change of PRMSTR allowed 
C  
C     Rev 1.6   28 Jan 1997 17:53:32   RXK
C  Update values of TIERLIM,GVTSUP,FWDCNT,PASSEXP allowed
C  
C     Rev 1.5   13 Jan 1997 16:34:56   RXK
C  GVT id online change allowed 
C  
C     Rev 1.2   27 Aug 1996 11:08:32   WXW
C  Added missing value for command in BL1 snap.
C  
C     Rev 1.1   19 May 1996 17:52:56   HXK
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    19 May 1996 17:48:56   HXK
C  Initial revision.
C  
C
C SUBROUTINE BUILDALL
C
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_buildlev.for **
C
C BUILDALL.FOR
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE BUILDALL(INDEX,STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:RECUSE.DEF'
        INCLUDE 'INCLIB:PRMLVL.DEF'
        INCLUDE 'INCLIB:PRMHSF.DEF'
        INCLUDE 'INCLIB:DESLVL.DEF'
C
        INTEGER*4 STATUS,INDEX, J, I
        INTEGER*4 LVLIM
        PARAMETER (LVLIM=168)
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
     *               01399,01381,23002,01349,01350,01051,00000,00000,
C V10 - Start
C     *               01354,39*00000/
     *               01354,01477,01478,01479,01480,01481,01482,01483,
     *               01484,01485,01486,01487,01488,01489,01490,01491,
C----+------------------------------------------------------------------
C V11| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C    *               01492,23*00000/
CV12     *               01492,00000,00000,01494,01495,19*00000/
C----+------------------------------------------------------------------
C V11| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C V10 - End     
c
C----+---+-------------+------------------------------------------------
C V12|BEG| M16 PROJECT | Added new comands regarding Euromillions
C----+---+-------------+------------------------------------------------

     *               01492,01494,01495,01466,01467,01468,01469,01470,
     *               01474,01475,01493,01496,01497,01498,01499,01500,
     *               8*00000/
C----+---+-------------+------------------------------------------------
C V12|END| M16 PROJECT | Added new comands regarding Euromillions
C----+---+-------------+------------------------------------------------
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
C Fill in default vision screens and commands
C
        IF(INDEX.EQ.VINDEX) THEN
          DO 20 I=1,LVLIM
            SNAPS(I,1)=DEFS(I)
            CMDS(I,1)=DEFC(I)
 20       CONTINUE
        ENDIF
C
C Fill in default hasf menus
C
        IF(INDEX.EQ.HINDEX) THEN
          DO 22 I=1,LVLIM
            HMENUS(I,1)=DEFM(I)
 22       CONTINUE
        ENDIF
C
C Fill in userpass menu numbers
C
        IF(INDEX.EQ.UINDEX) THEN
          DO 24 I=1,LVLIM
            USERS(I,1)=DEFU(I)
 24       CONTINUE
        ENDIF
C
C Fill in chglvl menu numbers
C
        IF(INDEX.EQ.LINDEX) THEN
          DO 26 I=1,LVLIM
            LEVMEN(I,1)=DEFL(I)
 26       CONTINUE
        ENDIF
C
        STATUS=0
        RETURN
        END
