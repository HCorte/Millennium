C
C SUBROUTINE TRNTRACE
C $Log:   GXAFXT:[GOLS]TRNTRACE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:37:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:53:22   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - trntrace.for **
C
C TRNTRACE.FOR
C
C     18-MARCH-91 WS, WILL ALWAYS TRACE IF MONITOR SET TO 255
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C     TRNTRACE.FTN
C
C     GENERATE TRACE OF PROCOM BUFFER
C
C     TRNTRACE(PROBUF,FLAG)
C     IN:
C     PROBUF         -  PROCOM BUFFER # WITH TRACED TRANSACTION
C     FLAG           - 0 IF INPUT MESSAGE, .NON.0 FOR OUTPUT
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TRNTRACE(PROBUF,FLAG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 TOTAL_LENGTH, OFFSET, MESSAGE_NO, SERIAL_NO
	INTEGER*4 LEN, STATION, STATUS, PTL_PROBUF, TO_TRACE
	INTEGER*4 TERMINAL, FLAG, PROBUF
C
	TERMINAL=HPRO(TERNUM,PROBUF)
	IF (TERMINAL.LE.0 .OR. TERMINAL.GT. X2X_TERMS) RETURN
 
C
	CALL ILBYTE(TO_TRACE,IX2XT_TRACE_LIMIT,TERMINAL-1)
	IF (TO_TRACE.LE.0) RETURN
C
C     TRY TO GET BUFFER TO PUT TRACED TRANSACTION
C
	CALL GETBUF(PTL_PROBUF)
	IF (PTL_PROBUF.LE.0) RETURN
	IF (TO_TRACE.NE.255) TO_TRACE=TO_TRACE-1
	CALL ISBYTE(TO_TRACE,IX2XT_TRACE_LIMIT,TERMINAL-1)
C
C     SET UP THE MESSAGE TU SEND
C
C     SET HEADER
C
	CALL I4TOBUF2(X2X_MESTYP_CMD,PRO(INPTAB,PTL_PROBUF),
     *	                         X2PRO_MESTYP-1)
	CALL ISBYTE(X2X_TRATYP_GLO,PRO(INPTAB,PTL_PROBUF),X2PRO_TRATYP
     *	                                                     -1)
	CALL I4TOBUF2(TERMINAL,PRO(INPTAB,PTL_PROBUF),X2PRO_TERMINAL-1)
	STATUS=X2ERR_GLO_TRACE_TER_INP
	IF (FLAG.NE.0) STATUS=X2ERR_GLO_TRACE_TER_OUT
	CALL ISBYTE(STATUS,PRO(INPTAB,PTL_PROBUF),X2PRO_STATUS-1)
	STATION=X2XT_STATION_NO(TERMINAL)
	CALL I4TOBUF2(STATION,PRO(INPTAB,PTL_PROBUF),X2PRO_STATION-1)
	CALL ISBYTE(0,PRO(INPTAB,PTL_PROBUF),X2PRO_SSAP-1)
	CALL ISBYTE(X2PRO_MSGLEN,PRO(INPTAB,PTL_PROBUF),X2PRO_OFFSET-1)
C
C
C
C     SET DATA PART OF THE MESSAGE
C
	LEN=HPRO(INPLEN,PROBUF)
C
	CALL I4TOBUF2(LEN,PRO(INPTAB,PTL_PROBUF),X2PRO_MSGLEN-1)
	SERIAL_NO=PRO(SERIAL,PROBUF)
	CALL I4TOBUF4(SERIAL_NO,PRO(INPTAB,PTL_PROBUF),X2PRO_SERIAL-1)
	MESSAGE_NO=HPRO(MSGNUM,INPTAB)
	CALL I4TOBUF2(MESSAGE_NO,PRO(INPTAB,PTL_PROBUF),X2PRO_MSGNUM-1)
	OFFSET=(X2PRO_TRACE_MSG+3)/4-1
	TOTAL_LENGTH=X2PRO_TRACE_MSG+LEN
	IF (TOTAL_LENGTH.GT.X2PRO_MAXMES) TOTAL_LENGTH=X2PRO_MAXMES
	CALL FASTMOV(PRO(INPTAB,PROBUF),PRO(INPTAB+OFFSET,PTL_PROBUF),
     *	             (TOTAL_LENGTH+3)/4)
	HPRO(INPLEN,PTL_PROBUF)=TOTAL_LENGTH
	HPRO(TERNUM,PTL_PROBUF)=0
	HPRO(LINENO,PTL_PROBUF)=STATION
C
	HPRO(TRCODE,PTL_PROBUF)=TYPX2X_PRO
C
C     QUEUE TO DISPAT BACK
C
	CALL ABL(PTL_PROBUF,QUETAB(1,DIS),STATUS)
C
	RETURN
	END
