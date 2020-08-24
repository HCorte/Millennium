C FBIGWIN.FOR
C
C V20 29-NOV-2000 UXN Totogola added.
C V19 02-DEC-1999 OXK Games selection changed
C V18 22-NOV-1999 OXK Winning limit changed 1000 -> 10000
C V17 17-NOV-1999 UXN Super Triple added (again!)
C V16 13-OCT-1999 RXK World Tour added.
C V15 09-JUN-1999 RXK Format fixed.
C V14 29-APR-1999 RXK If multiwin then take amount 1000 mk and skip questions
C V13 15-JAN-1999 GLS Auto reporting if MULTIWIN
C V12 09-SEP-1998 RXK Changed for new Kicker
C V11 23-JAN-1998 UXN Super Score and Todays Triple added.
C V10 18-JAN-1996 RXK Today's Couple and Super Double added 
C V09 08-JAN-1995 HXK Added Bingo game
C V08 20-JAN-1994 HXK further correction to format statements.
C V07 12-JAN-1994 HXK FIXED FORMAT ERROR.
C V06 08-OCT-1993 HXK FIXED STUFF.
C V05 28-SEP-1993 HXK Initial revision.
C V04 02-OCT-1997 UXN OPTION LAYOUT FOR GAME SELECTION CHANGED.
C V03 04-FEB-1991 MTK CHANGED FOR KENO GAME
C V02 07-JUN-1990   LOU R.   SEGMENT FAULT FIXED RFSS 900039.
C V01 03-NOV-1989 MGM INITIAL RELEASE FOR FINLAND
C
C FBIGWIN.FOR 
C
C BIG PRIZE WINNERS REPORTS FOR ALL GAMES
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode
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
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      PROGRAM FBIGWIN
      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'

      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:LTOCOM.DEF'
      INCLUDE 'INCLIB:SPTCOM.DEF'
      INCLUDE 'INCLIB:TGLCOM.DEF'
      INCLUDE 'INCLIB:KIKCOM.DEF'
      INCLUDE 'INCLIB:BNGCOM.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:GTNAMES.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'                                !V12

      INTEGER*4 ST, K
      INTEGER*4 DRAWS(MAXGAM)
      INTEGER*4 PIFKEY, EXT, AMOUNT, DIVNUM, FLAG, I, ANS
      INTEGER*4 GAM, GIND, GTYP
      INTEGER*4 GNUM

      CHARACTER*8 PASPAS
      CHARACTER*20 PASENT
      LOGICAL FULSER,DIVOPT
      LOGICAL USECDC
      EQUIVALENCE (PASPAS,PASENT)
      COMMON /SCF/ SCFREC
      DATA DIVNUM/0/,AMOUNT/0/

      INTEGER*4 FAMOUNT                               !amount for multiwin fixed
      PARAMETER(FAMOUNT=10000*DOLL_BASE/DYN_VALUNIT)
C
C CALL  COPYRITE  SUBROUTINE
C
      CALL COPYRITE
C
C OPEN FILES
C
      CALL FASTSET(0,DRAWS,MAXGAM)
      TYPE *,IAM()
      TYPE *,IAM(),'<< Portugal Big Winners Reporting Version 1.5 >>'
      TYPE *,IAM()
C
C ASK IF KEY IS ON AMOUNT OR DIVISION.
C
      USECDC=.FALSE.
      IF(STOPMOD.EQ.WINMANUAL) THEN
         CALL PRMYESNO('Do you want to use settings by default ?',FLAG)
         IF(FLAG.EQ.1) THEN
            AMOUNT=FAMOUNT
            FULSER=.FALSE.
            ANS=0
	    USECDC=.TRUE.
         ELSE
            CALL PRMNUM('Enter Key 1=Amount, 2=Classes ',PIFKEY,1,2,EXT)
            IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
            IF(PIFKEY.EQ.1) THEN
               CALL PRMMONY('Enter winning amount limit ',AMOUNT,VALUNIT,EXT)
               IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
               DIVOPT=.FALSE.
            ELSE
               CALL PRMNUM('Enter low division number to report from ',
     *                      DIVNUM,1,10,EXT)
               IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
               DIVOPT=.TRUE.
            ENDIF
            FULSER=.FALSE.
            CALL PRMYESNO('Do you want full serial numbers (Y/N) ?',FLAG)   !V12
            IF(FLAG.EQ.1) THEN
               FULSER=.TRUE.
               CALL PASSWORD(5,PASENT)
               IF(PASPAS.NE.'ZQYAWE1C') THEN
                  TYPE*,IAM(),' ********** ACCESS DENIED SORRY **********'
                  TYPE*,IAM(),' YOU MUST HAVE CORRECT PASSWORD FOR ACCESS'
                  CALL GSTOP(GEXIT_SUCCESS)
               ENDIF
            ENDIF
            CALL PRMYESNO('Overide games to report on (Y/N)?',ANS) 
         ENDIF
      ELSE
         AMOUNT=FAMOUNT
         FULSER=.FALSE.
         ANS=0
	 USECDC=.TRUE.
      ENDIF
C
C
C READ SCF RECORD
C
      CALL GETSCONF(SCFREC,ST)
C
      IF(ANS.NE.1) THEN
        DO 20 GAM=1,MAXGAM
        GIND = SCFGNT(GAMIDX,GAM)
        GTYP = SCFGNT(GAMTYP,GAM)
        IF(GTYP.EQ.0.OR.GIND.EQ.0) GOTO 20
        IF(GTYP.EQ.TLTO) THEN
          IF(LTODAT(CURDRW,GIND).NE.DAYCDC) GOTO 20
          DRAWS(GAM)=LTODRW(GIND)
        ENDIF
        IF(GTYP.EQ.TSPT) THEN
          IF(SPTDAT(CURDRW,GIND).NE.DAYCDC) GOTO 20
          DRAWS(GAM)=SPTDRW(GIND)
        ENDIF
        IF(GTYP.EQ.TTGL) THEN
          IF(TGLDAT(CURDRW,GIND).NE.DAYCDC) GOTO 20
          DRAWS(GAM)=TGLDRW(GIND)
        ENDIF
        IF(GTYP.EQ.TKIK) THEN
          IF(KIKDAT(CURDRW,GIND).NE.DAYCDC) GOTO 20 
          DRAWS(GAM)=KIKDRW(GIND)
        ENDIF
        IF(GTYP.EQ.TBNG) THEN
          IF(BNGDAT(CURDRW,GIND).NE.DAYCDC) GOTO 20
          DRAWS(GAM)=BNGDRW(GIND)
        ENDIF
20      CONTINUE
	DO GAM=1,MAXGAM
	  IF((DRAWS(GAM).NE.0).OR.USECDC) GOTO 2000
	ENDDO
	TYPE*,IAM(),'Sorry, no drawings today.'
	CALL GSTOP(GEXIT_SUCCESS)
      ENDIF
      GNUM = 0
200   CONTINUE
      WRITE(6,900) IAM(), ( IAM(),K,GTNAMES(K),K=1,MAXTYP )
      CALL PRMNUM('Enter game type  (E - no more)',GTYP,1,MAXTYP,EXT)   !V12
      IF(EXT.LT.0) GOTO 1900
      CALL PRMNUM('Enter game index ',GIND,1,MAXIND,EXT)                !V12
      IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
      GNUM = SCFGTN(GTYP,GIND)
      IF(GNUM.LT.1) THEN
        TYPE*,IAM(),'Sorry, selected game not active'
        GOTO 200
      ENDIF
      CALL PRMNUM('Enter draw to report on',DRAWS(GNUM),1,99999,ST)
      IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
      GOTO 200
1900  CONTINUE
      DO I=1,MAXGAM
	IF(DRAWS(I).NE.0) GOTO 2000
      ENDDO
      TYPE*,IAM(),'Sorry, no games selected'
      CALL GSTOP(GEXIT_SUCCESS)
2000  CONTINUE
C
C GENERATE REPORTS FOR SELECTED GAMES
C
      CALL FINDBIG(DRAWS,AMOUNT,FULSER,DIVNUM,DIVOPT,USECDC)
      TYPE *,IAM(),'Big Prize Winners Reports Generated ...'
      CALL GSTOP(GEXIT_SUCCESS)
C
C
900   FORMAT(//1X,A,4X,'  Big winners report ',//,
     *       <MAXTYP>(1X,A,5X,I2,' - ',A8,/))
C
      END
