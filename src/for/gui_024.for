C GUI_024.FOR
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C This subroutine returns GUI FUNCTION.
C
C Input parameters:
C	NONE               
C
C Output parameters:
C
C	BYTE		OUTBUF(*)    OUTPUT MESSAGE
C	INTEGER*4	MES_LEN	     MESSAGE LENGTH
C	INTEGER*4	RET_CODE:
C		0		-  no error, message accepted;
C		value >= 11	-  error number to be sent to Client.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUI_024(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:CTLCOM.DEF'
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'
        INCLUDE 'INCLIB:DN_BLOCK.DEF'
        INCLUDE 'INCLIB:LANCOM.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
	CHARACTER*1	ID_SYS(0:MAX_SYSTEMS)
	DATA            ID_SYS/' ','A','B','C','D','E'/
        CHARACTER*8     CONSTA(0:5)     /'        ',
     *                                   'idle    ',
     *                                   'dead    ',
     *                                   'not init',
     *                                   'recovery',
     *                                   'normal  '/
C
        CHARACTER*8     AENA(0:1)       /'Disabled',
     *                                   'Enabled '/,
     *                  ASTATE(0:2)     /'Closed  ',
     *                                   'Pending ',
     *                                   'Open    '/
	INTEGER*4 SAP,SYS,SYS1
C

	INTEGER*4 STASYS(NETSYS),SERDISP(NETSYS)
	INTEGER*4 NUM_COLS,NUM_ROWS
C
        INTEGER*4  I,J
C
	RET_CODE = 0
C
        DO 700 SYS = 1, NETSYS
          STASYS(SYS) = 3                               ! NOT INIT
          IF (NETROUT(SYS, WAYINP) .EQ. ROUNO) THEN
            STASYS(SYS) = 0                             ! NO CONNECTION
          ELSEIF (NETSTAT(SYS, WAYINP) .EQ. NSTADEADP .OR.
     *            NETSTAT(SYS, WAYINP) .EQ. NSTADEADS) THEN
            STASYS(SYS) = 2                             ! DEAD
          ELSEIF (NETROUT(SYS, WAYINP) .EQ. ROUIDLE) THEN
            STASYS(SYS) = 1                             ! IDLE
          ELSEIF (NETROUT(SYS, WAYINP) .EQ. ROUACT) THEN
            SYS1 = SYS
            IF (NETSTAT(SYS, WAYINP) .EQ. NSTASEC) SYS1 = NODEID
            IF (NETMODE(SYS1, WAYINP) .EQ. FILMD) THEN
              STASYS(SYS) = 4                           ! RECOVERY
            ELSE
              STASYS(SYS) = 5                           ! NORMAL
            ENDIF
          ENDIF
700     CONTINUE
        DO 800 J = 1, NETSYS
          SERDISP(J) = 0
          IF (NETROUT(J, WAYINP) .EQ. ROUACT) THEN
            IF (NETSTAT(J, WAYINP) .EQ. NSTASEC) THEN
              SERDISP(J) = NXTSER
            ELSEIF (NETSTAT(J, WAYINP) .EQ. NSTAPRIM) THEN
              IF (NETMODE(J, WAYINP) .EQ. FILMD) THEN
                SERDISP(J) = NETSER(J, WAYINP)
              ELSE
                SERDISP(J) = NETHSER(J, WAYINP)
              ENDIF
            ENDIF
          ENDIF
          SERDISP(J) = MOD(SERDISP(J), SYSOFF)
800     CONTINUE
C
C BUILD GUI MESSAGE
C
	CALL GUIARG_INIT()
C
C RESULT SET 1
C
	NUM_COLS = 4
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
	CALL GUIARG_CHAR(OUTBUF,%REF(ID_SYS(NETMASTER(WAYINP))),1)
	CALL GUIARG_CHAR(OUTBUF,%REF(ID_SYS(NETBACKUP(WAYINP))),1)	
	CALL GUIARG_INT4(OUTBUF,NXTSER)
	CALL GUIARG_BYTE(OUTBUF,X2X_GAME_SAP)
C
C RESULT SET 2
C
	NUM_COLS = 12
	NUM_ROWS = MAX_SYSTEMS
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)

	DO I=1,MAX_SYSTEMS	    
	   CALL GUIARG_CHAR(OUTBUF,%REF(ID_SYS(I)),1)
	   CALL GUIARG_CHAR(OUTBUF,%REF(CONSTA(STASYS(I))),8)
	   CALL GUIARG_INT4(OUTBUF,SERDISP(I))
	   CALL GUIARG_INT4(OUTBUF,NETRETRY(I, WAYINP))

	   CALL GUIARG_INT4(OUTBUF,NETERR(I, WAYINP))
	   CALL GUIARG_INT4(OUTBUF,NETTIMER-NETTIM(I, WAYINP))

	   SAP = CTLSAPSYS(I)
	   CALL GUIARG_INT4(OUTBUF,SAP)
	   CALL GUIARG_CHAR(OUTBUF,%REF(ASTATE(CTLSAPSTA(SAP))),8)
	   CALL GUIARG_INT4(OUTBUF,CTLSAPTOUT(SAP))
	   CALL GUIARG_CHAR(OUTBUF,%REF(AENA(CTLSAPENA(SAP))),8)
	   CALL GUIARG_INT4(OUTBUF,CTLSSEQ(SAP))
	   CALL GUIARG_INT4(OUTBUF,CTLRSEQ(SAP))
	ENDDO
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
C
	END
