C
C SUBROUTINE X2GETSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GETSTN.FOV                                 $
C  $Date::   17 Apr 1996 16:19:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2rcvbuf.for;1 **
C
C
C     X2GETSTN(BUFFER,STATION_NO,TERMINAL_NO,VERIFY)
C			       GET STATION NO FROM STATION MESSAGE
C     			       THIS ROUTINE IS VALID FOR DOWNLINE MESSAGES
C     IN -  STATION MESSAGE
C	 -  VERIFY FLAG
C     OUT - STATION_NO
C	  - TERMINAL_NO
C
C     NOTE: 
C       EXTENDED ERROR CODES OVERWRITE THE FIRST THREE BYTES OF THE STATION
C       MESSAGE (PROTO_ID,DATA_TYPE, AND CONFIGURATION CHKSUM). THE VERIFY
C       FLAG WILL SKIP THE CHECKS OF THESE FIELDS. STATION NUMBER REMAINS
C       INTACT.
C
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
	SUBROUTINE X2GETSTN(BUFFER,STATION_NO,TERMINAL_NO,VERIFY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	BYTE      BUFFER(0:*)     !ELIMINATE ILBYTE AND ISBYTE
	INTEGER*4 PROTID, STATION_NO,VERIFY
	INTEGER*4 PORT_BYTE,PORT,DROP,TERMINAL_NO,SDUT
C
	STATION_NO=-1
	TERMINAL_NO=-1
C
	PROTID = ZEXT (BUFFER(X2STMES_PROTID-1))
	IF (PROTID.NE.X2STMES_PROTID_VAL) THEN
	    IF (VERIFY.EQ.1) RETURN
	ENDIF
C
	SDUT = ZEXT(BUFFER(X2STMES_DATATYPE-1))
	CALL MOV2TOI4(STATION_NO,BUFFER,X2STMES_STATION_NO-1)
C
C IF TERMINAL MESSAGE THEN EXTRACT THE PORT AND DROP.
C
	IF(SDUT.NE.X2STMES_DATATYPE_DOWN .AND.
     *     SDUT.NE.X2STMES_DATATYPE_UP) THEN 
	   IF(VERIFY.EQ.1) RETURN
	ENDIF
C
	PORT_BYTE = ZEXT(BUFFER(X2STMES_PORTID-1))
	PORT=IAND(PORT_BYTE,X2STMES_PORT_MASK)
	PORT=PORT+1               !TRANSPORTS COUNTS PORTS FROM 0
	DROP = ZEXT(BUFFER(X2STMES_TERMID-1))
	DROP=DROP+1
C
	IF (STATION_NO.GT.0 .AND. STATION_NO.LE.X2X_STATIONS .AND.
     *      PORT.GT.0       .AND. PORT.LE.X2X_MAXPORT .AND.
     *      DROP.GT.0       .AND. DROP.LE.X2X_MAXTERMS) THEN
	    TERMINAL_NO=X2XS_TERMS(DROP,PORT,STATION_NO)
	ENDIF
C
	RETURN
	END
