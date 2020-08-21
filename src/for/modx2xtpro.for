C
C PROGRAM MODX2XTPRO
C
C V01 02-FEB-2014 FRP INITIAL RELEASE FOR SCML
C
C PROGRAM TO MODIFY THE VALUE OF X2XT_PRO (PROCOM BUF USED)
C FOR A CERTAIN TERMINAL TO ALLOW HIM CONTINUING X2X COMMUNICATIONS
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
      PROGRAM MODX2XTPRO
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:PRMAGT.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:X2XCOM.DEF'
C
      INTEGER*4    EXIT
      CHARACTER*20 PASPAS
      CHARACTER*6  PASSENT
      CHARACTER*6  DEFTPASS
      EQUIVALENCE(PASPAS,PASSENT,EXIT)
      DATA DEFTPASS/'DONUTS'/
C
      INTEGER*4 STS,FLAG,TERNUM
      INTEGER*4 OLDVAL,NEWVAL
C
      CALL COPYRITX(5)
100   CONTINUE
      CALL CLRSCR(5)
      CALL PASSWORD(5,PASPAS)
      IF(EXIT.EQ.'EXIT') THEN
        CALL CLRSCR(5)
        CALL GSTOP(GEXIT_SUCCESS)
      ENDIF
      IF(PASSENT.EQ.DEFTPASS .AND. PASSENT.NE.'        ') GOTO 200
      GOTO 100
C
200   CONTINUE
      CALL CLRSCR(5)
C
      TYPE*,IAM()
      TYPE*,IAM(),'This program allows to modify the value'
      TYPE*,IAM(),'of X2XTPRO buffer for a certain terminal.'
C
      TYPE*,IAM()
      CALL INPNUM('Enter terminal number: ',TERNUM,1,NUMAGT,STS)
      IF(STS.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
      OLDVAL=X2XT_PRO(TERNUM)
      TYPE*,IAM()
      TYPE*,IAM(),'Current X2XTPRO value: ',OLDVAL
C
      TYPE*,IAM()
      CALL INPNUM('Enter new X2XTPRO value: ',NEWVAL,0,999999,STS)
      IF(STS.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
      TYPE*,IAM()
      CALL PRMYESNO('Do you want to continue [Y/N]? ',FLAG)
      IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
      X2XT_PRO(TERNUM)=NEWVAL
      TYPE*, IAM()
      TYPE*, IAM(),'Value of X2XTPRO changed from ',OLDVAL,' to ',NEWVAL,' for terminal ',TERNUM
C
      TYPE*, IAM()
      CALL GSTOP(GEXIT_SUCCESS)
      END
