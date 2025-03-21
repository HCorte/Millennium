C
C SUBROUTINE BLDCON
C $Log:   GXAFXT:[GOLS]BLDCON.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:17:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:42:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gnconect.for **
C
*
* ========================================================
* SUBROUTINE BLDCON
*
* This subroutine will build the connection message and
* will send it to TCP/IP.
*
* Input parameters:
*
*     OUTCON      Int*4       Connection Number
*     MESTYP      Int*4       Message type
*     SUBTYP      Int*4       Message subtype
*     MSGID       Int*4       PC message identifier
*     MSGSEQ      Int*4       Message sequence number
*
* Ouput paramters:
*
*     MESLEN      Int*4       Message length
*
	SUBROUTINE BLDCON(OUTCON,MESTYP,SUBTYP,MSGID,MSGSEQ,MESLEN)
	IMPLICIT NONE
*
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
*
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GNSMES.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
*
	INTEGER*4   OUTCON              !Connection number
	INTEGER*4   MESTYP              !Message type
	INTEGER*4   SUBTYP              !Message subtype
	INTEGER*4   MSGID               !PC message identifier
	INTEGER*4   MSGSEQ              !Message sequence number
	INTEGER*4   MESLEN              !Message length
	INTEGER*4   MESS(400)           !Output buffer
	INTEGER*4   SSAP,DSAP           !Source SAP, Destination SAP
	INTEGER*4   OFF                 !Data message offset
	INTEGER*4   CONLAN,CONSTA       !Connection Lan/State
	INTEGER*4   CURTIM              !Current system time
*
* SEARCH THE CONNECTION TABLE FOR THE CONNECTION NUMBER
* TO DETERMINE THE SAPS.
*
	DO 100 SSAP=1,MAXSAP
	  DO 200 DSAP=1,MAXSAP
	    IF(CONNECTION(SSAP,DSAP).EQ.OUTCON) GOTO 300
200	  CONTINUE
100	CONTINUE
*
* CONNECTION WAS NOT FOUND.
*
	MESLEN=-1
	GOTO 8000
*
* CONNECTION FOUND, BUILD HEADER MESSAGE.
*
300	CONTINUE
	MSGSEQ=MSGSEQ+1
	CALL ISBYTE(GNHDRMES_PROTID_VAL,MESS,GNDWNMES_PROTID-1)
	CALL I4TOBUF2(MESTYP,MESS,GNDWNMES_MESTYP-1)
	CALL ISBYTE(SUBTYP,MESS,GNDWNMES_SUBTYP-1)
	CALL I4TOBUF2(MSGID,MESS,GNDWNMES_MSGID-1)
	CALL I4TOBUF2(MSGSEQ,MESS,GNDWNMES_SEQNUM-1)
	CALL I4TOBUF2(GNDWNMES_CMDDTA,MESS,GNDWNMES_DATOFF-1)
	CURTIM=P(ACTTIM)
	CALL I4TOBUF4(CURTIM,MESS,GNDWNMES_TIME-1)
	CALL ISBYTE(GNDWNMES_FLAGS_DATA,MESS,GNDWNMES_FLAGS-1)
	CALL ISBYTE(0,MESS,GNDWNMES_CMDSTS-1)
	CALL ISBYTE(0,MESS,GNDWNMES_CMDDTA-1)
	OFF=GNDWNMES_CMDDTA
*
* BUILD OUTPUT DATA MESSAGE PORTION.
*
	CALL I4TOBUF4(OUTCON,MESS,OFF+GNCONMES_NUMBER-1)
	CALL ISBYTE(SSAP,MESS,OFF+GNCONMES_SSAP-1)
	CALL ISBYTE(DSAP,MESS,OFF+GNCONMES_DSAP-1)
	CONLAN=CURLAN(OUTCON)
	CONSTA=LANCONN(OUTCON)
	CALL ISBYTE(CONLAN,MESS,OFF+GNCONMES_LAN-1)
	CALL ISBYTE(CONSTA,MESS,OFF+GNCONMES_STATE-1)
*
* STORE MESSAGE LENGTH INTO MESSAGE HEADER.
*
	MESLEN=OFF+GNCONMES_STATE
	CALL I4TOBUF2(MESLEN,MESS,GNDWNMES_MESLEN-1)
*
* SEND MESSAGE TO TCP/IP.
*
	CALL TCP_SNDBUF(MESS,MESLEN)
*
* PROGRAM EXIT.
*
8000	CONTINUE
	RETURN
	END
