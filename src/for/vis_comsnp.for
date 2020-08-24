C
C SUBROUTINE COMSNP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]COMSNP.FOV                                   $
C  $Date::   17 Apr 1996 12:41:46                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_comsnp.for **
C
C COMSNP.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added AGTINF.DEF for Finland.
C
C V03 03-FEB-94 GPR USE I5 FORMAT FOR STATION AND TERMINAL TYPE-OUTS
C V02 30-JAN-91 KWP REMOVED LINCOM/TERCOM
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C
C     COMSNP.FTN    TERMINAL AVAILABILITY SNAPSHOT
C
C     AUTHOR DAVID HARRIS, GAMING SYSTEMS CORPORATION
C
C     DISPLAYS ALL TERMINALS ON SLOW POLL
C
C     SUBROUTINE LEVEL MAP
C
C     VISION-> SCREEN-> VPAGE-> COMSNP
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE COMSNP(TER)
C
C     TER       INTEGER   FIRST TERMINAL NUMBER TO DISPLAY
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:SLOCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4 ITEM, ROW, LIN, TOTAL, IX, COUNT, K, TER
	INTEGER*4 STN, TSTATE
C
	INTEGER TRMARY(68),STNARY(68),T(4,17),L(4,17)
	INTEGER MINARY(68),M(4,17)
	CHARACTER*1 DRPARY(68),D(4,17)
C
	EQUIVALENCE (T,TRMARY),(L,STNARY),(D,DRPARY),(M,MINARY)
C
	CHARACTER*20 HDR/'  Term   Stn D Mins '/			! V03
C
C     CLEAR ALL ARRAYS
C
	DO 10 K=1,68
	  TRMARY(K)=0
	  STNARY(K)=0
	  MINARY(K)=0
	  DRPARY(K)=' '
10	CONTINUE
C
C     GATHER DATA FROM TERMINAL DATA BASE
C
	COUNT=0
	IX=0
	TOTAL=0
	DO 100 K=1,X2X_TERMS
	  STN=X2XT_STATION_NO(K)
	  IF(STN.LT.1) GOTO 90
	  TOTAL=TOTAL+1
	  CALL ILBYTE(TSTATE,IX2XT_STATE,K-1)
	  IF(TSTATE.NE.X2XTS_SLOW_POLL) GOTO 90
	  COUNT=COUNT+1
	  IF (K.LT.TER) GO TO 90
	  IX=IX+1
	  IF (IX.GT.68) GO TO 90
	  TRMARY(IX)=K
	  STNARY(IX)=STN
	  DRPARY(IX)=X2XT_DROP_AD(K)
	  MINARY(IX)=SLOTAB(K)
90	  CONTINUE
100	CONTINUE
C
C     BUILD SCREEN IMAGE
C
	WRITE(CLIN1,5010)
	WRITE(CLIN3,5030) TOTAL,COUNT
	WRITE(CLIN5,5050) HDR,HDR,HDR,HDR
C
	DO 300 ROW=1,17
	LIN=ROW+5
	   DO 200 ITEM=1,4
	   IX=(ITEM-1)*20+1						! V03
         WRITE(XNEW(LIN)(IX:IX+19),5555) T(ITEM,ROW),L(ITEM,ROW),	
     *		  D(ITEM,ROW), M(ITEM,ROW)				! V03
200	   CONTINUE
300	CONTINUE
C
C     NOTE THAT ONLY 68 ITEMS CAN BE DISPLAYED AT ONCE
C
	RETURN    ! RETURN TO CALLER
C
C     FORMAT STATEMENTS
C
5010	FORMAT(1X,'Communications - Terminal Availability')
5030	FORMAT(T5,I5,' Terminals are configured',
     >	       T41,I5,' Terminals are on slow poll')			! V03
5050	FORMAT(4A20)
5555	FORMAT(1X,I5.0,1X,I5.0,1X,A1,1X,I4.0,1X)				! V03
C
	END
