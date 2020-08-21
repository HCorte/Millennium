C PROGRAM CTLPRO
C
C V04 15-JUN-2000 OXK CT_EVNMASK from CTLEVN.DEF to CTLPRO.FOR
C V03 09-MAY-1991 MP  ADDED CALL TO SNIF_AND_WRKSET
C V02 07-JAN-1991 KWP INITIAL VAX VERSION
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C V00 12-DEC-1989 MBK ORIGINAL RELEASE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 2000 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose: Local CTLPRO communicates with remote CTLPRO on other systems
C          (primary and backup are involved). It is used to make sure
C          that the backup takes over only when the primary is realy down.
C          The task is trap driven.
C
C Refer to : CTIMTRAP.FOR
C            CMESTRAP.FOR
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	PROGRAM CTLPRO
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:CTLCOM.DEF'
	INCLUDE 'INCLIB:CTLEVN.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:LANEVN.DEF'
	INCLUDE 'INCLIB:X2TDBH.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INCLUDE '($IODEF)'
	INCLUDE '($SSDEF)'
	INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
C FOR SYS -> SAP THE CORRESPONDENCE IS STRAIGHTFORWARD
C
	INTEGER*4	LOCSAPSYS(MAX_SYSTEMS)	/2, 4, 6, 8, 10/
C
C FOR SAP -> SYS : n -> sys;  0 -> other
C
	INTEGER*4	LOCSYSSAP(0:CTLMAXSAP)	/0, 2, 4, 6, 8, 10, 59*0/
C
        INTEGER*4       CT_EVNMASK              !BITMAP OF ALL EVENTS SET

	INTEGER*4	CTLX2XTOUT,
     *			SAP,
     *			ST,
     *			STATUS,
     *			SYS
C
	CHARACTER*4	GXEVNNAM      
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SET-UP & INITIALIZATION
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
C WAIT FOR SOMEONE TO LET YOU GO
C
100	CONTINUE
	IF (THISSTA .NE. STAUP) THEN
	  CALL XWAIT(CTLINT, 1, ST)
	  GOTO 100
	ENDIF
C
C SET SOME STARTUP PARAMETERS
C
	CTLSTATUS      = CTLACTIVE
	CTLTEST        = 0
	CTLMAXTOUT     = LANDEL2 / CTLINT + 10
	CTLX2XTOUT     = CTLX2XMAX
	X2X_GAME_CHECK = X2X_GAMEC_REQ
C
	DO 200 SAP = 0, CTLMAXSAP
	  CTLSYSSAP(SAP)  = LOCSYSSAP(SAP)
	  CTLSAPSTA(SAP)  = CTLSTADOWN
	  CTLRSEQ(SAP)    = 0
	  CTLSSEQ(SAP)    = 0
	  CTLSAPTOUT(SAP) = 0
	  CTLSAPENA(SAP)  = 1
200	CONTINUE
C
	DO 300 SYS = 1, CTLMAXSYS
	  CTLSAPSYS(SYS) =  LOCSAPSYS(SYS)
300	CONTINUE
C
	CALL FASTSET(0, CTLEXECQ, CTLQSIZ + 2)
	CALL DEFLST(CTLEXECQ, CTLQSIZ)
C
C CREATE THE COMMON EVENT FLAG CLUSTER FOR LANPRO.
C
	STATUS = SYS$ASCEFC(%VAL(LN_EVNTIMER),
     *                      GXEVNNAM() // LN_EVNNAME, 0, 0)
	IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C CREATE THE COMMON EVENT FLAG CLUSTER FOR CTLPRO.
C
	STATUS = SYS$ASCEFC(%VAL(CT_EVNTIMER),
     *                      GXEVNNAM() // CT_EVNNAME, 0, 0)
	IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C SETUP THE MAILBOX FOR INTERTASK MESSAGES.
C IF MAILBOX DOES NOT EXIST, CREATE IT.
C
	STATUS = SYS$ASSIGN(GXEVNNAM() // CT_MESNAME, CT_MESCHANNEL,,)
	IF (.NOT. STATUS) THEN
	  STATUS = SYS$CREMBX(%VAL(1), CT_MESCHANNEL,,, 
     *                        %VAL('FD00'X),, GXEVNNAM() // CT_MESNAME)
	  IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	ENDIF
C
	CALL CT_START_TIME
	CALL CT_START_MESS
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C MAIN LOOP
C	
400	CONTINUE
	CT_EVNMASK = 0
C
	STATUS = SYS$WFLOR(%VAL(CT_EVNTIMER), %VAL(CT_EVNMASK))
	IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
	GOTO 400
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C END
C	
	END
