C
C v05 24-NOV-2020 SCML Added support for OLM
C V04 25-FEB-2014 SCML Added support for IGS
C V03 12-AUG-2011 RXK "Millennium" replaced with "ES Evolution"
C V02 24-JAN-2011 RXK Change for DATE_AND_TIME.
C V01 02-SEP-2000 UXN INITIAL RELEASE.
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
C=======OPTIONS/CHECK=NOOVERFLOW
	SUBROUTINE SETUP(PAR,MODE)
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:SETUP.DEF'
	
	RECORD/SETUP/ PAR
	LOGICAL       MODE
C
	CALL LIB$MOVC5(0,0,0,SIZEOF(PAR),PAR)
C
C Set default values.
C
	PAR.TAPLOG = P(TAPESW)
	PAR.BAKLOG = P(DISKSW)
	PAR.IPSCON = P(PRMSTR)
C      
C EURO MIL PROJECT
C
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
C       PAR.EURCON = 1
       PAR.EURCON = P(EUMILF)
	   PAR.IGSCON = P(IGSCONF)
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V05| Adding support for OLM
C----+------------------------------------------------------------------
		PAR.OLMCON = P(OLMCONF)
		PAR.REGLOG = P(REGLOG)
C----+------------------------------------------------------------------
C V05| Adding support for OLM
C----+------------------------------------------------------------------
C
C Set CDC
C
	IF(P(NXTCDC).EQ.0) P(NXTCDC) = 1
	PAR.DAYCDC = P(NXTCDC)
C
	IF(MODE) PAR.SYSMOD = 1
C
C DISPLAY CONFIGURATION MENU
C
	CALL DISP_CONFIG(PAR)
C
C EURO MIL PROJECT 
C
	P(EUMILF) = PAR.EURCON
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
        P(IGSCONF) = PAR.IGSCON
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V05| Adding support for OLM
C----+------------------------------------------------------------------
		P(OLMCONF) = PAR.OLMCON
		P(REGLOG) = PAR.REGLOG
C----+------------------------------------------------------------------
C V05| Adding support for OLM
C----+------------------------------------------------------------------	
C
	END
C
C
C DISP_CONFIG
C
	SUBROUTINE DISP_CONFIG(PAR)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:SETUP.DEF'

	RECORD/SETUP/ PAR
C
	CHARACTER*3 YESNO(0:1)
	DATA YESNO  /' No','Yes'/
	INTEGER*4 OPT,ST
	CHARACTER*1 ESC/Z1B/
	INTEGER*2   DATE(LDATE_LEN),LEN
	INTEGER*4   K
	CHARACTER*15 NODE
	INTEGER*4    TODAY(8)
        CHARACTER*12 CLOCK(3) 
C
	INTEGER*4 GVTENDTIM(7)
	DATA GVTENDTIM/    79200,   !monday
     *                     79200,   !tueday
     *                     79200,   !wednessday
     *                     79200,   !thursday
     *                     79200,   !friday
     *                     72000,   !saturday
     *                     72000/   !sunday
	CHARACTER*4 SYSM(0:1)
	DATA SYSM/'LIVE','TEST'/
C
C
	CALL GET_NODENAME(NODE,LEN)
	CALL DATE_AND_TIME(CLOCK(1),CLOCK(2),CLOCK(3),TODAY)
	IF(PAR.TAPLOG.NE.0) THEN
	   CALL TAPCHECK('MAG'//CHAR(PAR.TAPLOG+48)//':',ST)
	   IF(ST.NE.0) THEN
	      TYPE*,IAM(),'Cannot open the tape ',PAR.TAPLOG
	      CALL XWAIT(2,2,ST)
	      PAR.TAPLOG = 0
	   ENDIF
	ENDIF
C
10	CONTINUE
	CALL CLRSCR(6)
	WRITE(6,9000) ESC,ESC
	WRITE(6,9001) ESC,ESC
	WRITE(6,9010) ESC,ESC,NODE
	WRITE(6,9011) ESC,ESC,NODE
	WRITE(6,9020) ESC,ESC,TODAY(3),TODAY(2),TODAY(1)
	WRITE(6,9021) ESC,ESC,TODAY(3),TODAY(2),TODAY(1)
	DATE(VCDC) = PAR.DAYCDC
	CALL LCDATE(DATE)
	WRITE(6,9002) PAR.DAYCDC,(DATE(K),K=7,13)
	WRITE(6,9003) YESNO(PAR.RESCHK)
	IF(PAR.TAPLOG.NE.0) THEN
	   WRITE(6,9004) YESNO(1), PAR.TAPLOG
	ELSE
	   WRITE(6,90041) YESNO(0)
	ENDIF
	WRITE(6,9005) YESNO(PAR.BAKLOG)

	IF(PAR.IPSCON.GT.0) THEN
	   WRITE(6,9006) YESNO(1),PAR.IPSCON
	ELSE
	   WRITE(6,90061) YESNO(0)
	ENDIF
	PAR.GVTTIM = GVTENDTIM(DATE(VDOW))
	WRITE(6,9007) DISTIM(PAR.GVTTIM)
C
C EURO MIL PROJECT (700)
C	
	WRITE(6,9008) YESNO(PAR.EURCON)
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
C
C IGS PROJECT (800)
C       
        WRITE(6,9009) YESNO(PAR.IGSCON)
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V05| Adding support for OLM
C----+------------------------------------------------------------------
        WRITE(6,9012) YESNO(PAR.OLMCON)
        WRITE(6,9013) YESNO(PAR.REGLOG)
C----+------------------------------------------------------------------
C V05| Adding support for OLM
C----+------------------------------------------------------------------
90	CONTINUE	
	WRITE(6,9100) SYSM(PAR.SYSMOD)
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
C	CALL INPNUM('Enter option [C - bring up the system]',OPT,1,7,ST)
        CALL INPNUM('Enter option [C - bring up the system]',OPT
     *              , 1
     *              , 10
     *              , ST)
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
	IF(ST.EQ.-5) RETURN
	IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
C	GOTO(100,200,300,400,500,600,700) OPT
        GOTO(100,200,300,400,500,600,700,800,900,1000) OPT
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
	GOTO 10
C
C CDC 
C
100	CONTINUE
	CALL INPDAT(PAR.DAYCDC,ST)
	GOTO 10
C
C RESTART FROM CHECKPOINT
C
200	CONTINUE
	PAR.RESCHK = 1 - PAR.RESCHK
	GOTO 10
C
C BACKUP TAPE LOGGING
C
300	CONTINUE
	CALL INPNUM('Enter tape drive number (0-no tape logging)',
     *               PAR.TAPLOG,0,2,ST)
	IF(ST.NE.0) GOTO 300
	IF(PAR.TAPLOG .GT. 0) THEN
	   CALL TAPCHECK('MAG'//CHAR(PAR.TAPLOG+48)//':',ST)
	   IF(ST.NE.0) THEN
	      TYPE*,IAM(),'Cannot open the tape'
	      PAR.TAPLOG = 0
	      GOTO 300
	   ENDIF
	ENDIF
	GOTO 10
C
C BACKUP DISK LOGGING
C	
400	CONTINUE
	PAR.BAKLOG = 1 - PAR.BAKLOG
	GOTO 10
C
C CONNECT TO IPS
C
500	CONTINUE
	CALL INPNUM('Which Instant system will we talk to today',
     *              PAR.IPSCON,0,2,ST)
	IF(ST.NE.0) GOTO 500
	GOTO 10
C
C GVT END TIME
C
600	CONTINUE
	CALL INPTIM('Enter GVT end time for today (HH:MM:SS)',PAR.GVTTIM,ST)
	IF(ST.NE.0) GOTO 600
	GOTO 10
C
C EURO MIL PROJECT (700)
C	
700     CONTINUE
	PAR.EURCON = 1 - PAR.EURCON
        GOTO 10
C
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
C
C IGS PROJECT (800)
C       
800     CONTINUE
        PAR.IGSCON = 1 - PAR.IGSCON
        GOTO 10
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V05| Adding support for OLM
C----+------------------------------------------------------------------
C
C OLM PROJECT (900)
C       
900     CONTINUE
        PAR.OLMCON = 1 - PAR.OLMCON
        GOTO 10
		

1000    CONTINUE
        PAR.REGLOG = 1 - PAR.REGLOG
        GOTO 10		
C----+------------------------------------------------------------------
C V05| Adding support for OLM
C----+------------------------------------------------------------------

9000	FORMAT('+',A1,'[1;4H',A1,'#3','PORTUGAL ES EVOLUTION SYSTEM STARTUP')
9001	FORMAT('+',A1,'[2;4H',A1,'#4','PORTUGAL ES EVOLUTION SYSTEM STARTUP')
9010	FORMAT('+',A1,'[3;16H',A1,'#3',A)
9011	FORMAT('+',A1,'[4;16H',A1,'#4',A)
9020	FORMAT('+',A1,'[5;14H',A1,'#3',I2.2,'.',I2.2,'.',I4.4)
9021	FORMAT('+',A1,'[6;14H',A1,'#4',I2.2,'.',I2.2,'.',I4.4)
9002    FORMAT(//,15X,'1.  CDC',T45,I4,1X,'( ',7A2,' )')
9003	FORMAT(15X,'2.  Restart from checkpoint',T45,1X,A3)
9004    FORMAT(15X,'3.  Backup tape logging',T45,1X,A3,1X,
     *         '( tape in MAG',I1,': )')
90041   FORMAT(15X,'3.  Backup tape logging',T45,1X,A3)
9005	FORMAT(15X,'4.  Backup disk logging',T45,1X,A3)
9006	FORMAT(15X,'5.  Connect to IPS',T45,1X,A3,1X,'( IPS system ', I1, ' )')
90061	FORMAT(15X,'5.  Connect to IPS',T45,1X,A3)
9007	FORMAT(15X,'6.  GVT end time',T41,A8)
9008    FORMAT(15X,'7.  Connect to EuroMilhoes',T45,1X,A3)
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
9009    FORMAT(15X,'8.  Connect to IGS',T45,1X,A3)
C----+------------------------------------------------------------------
C V04| Adding support for IGS
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V05| Adding support for OLM
C----+------------------------------------------------------------------
9012    FORMAT(15X,'9.  Connect to Olimpo',T45,1X,A3)
9013    FORMAT(15X,'10. Script log Suppress',T45,1X,A3)
C----+------------------------------------------------------------------
C V05| Adding support for OLM
C----+------------------------------------------------------------------
9100	FORMAT(//,18X,'The system is in *** ',A4,' *** mode' //)
	END
