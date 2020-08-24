C
C SUBROUTINE X2RESFE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RESFE.FOV                                  $
C  $Date::   17 Apr 1996 16:31:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2resfe.for;1 **
C
C X2RESFE.FOR
C
C V02 05-DEC-94 SCD Integrate UK changes into X2X Baseline
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This subroutine is used to tell the Front End processor
C to disconnect a station.  This routine is only used
C to also disable a station by sending a station configuration
C with the master enable flag turned off.  This routine
C is used only when invalid protocol is encountered.
C
C Calling Sequence:
C
C     CALL X2RESFE(TRABUF,MESS,ORGMESS,LEN)
C
C Input parameters:
C
C     TRABUF      Int*4(TRALEN)   Transaction buffer
C     ORGMESS     Int*4(*)        Message from station
C
C Output parameters:
C
C     MESS        Int*4(*)        Message to be sent to station.
C     MESLEN      Int*2           Length of output message (bytes)
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
	SUBROUTINE X2RESFE(TRABUF,MESS,ORGMESS,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:X2XPTL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*2   MESLEN          !Output message length
	INTEGER*4   ORGMESS(*)      !Station input message
	INTEGER*4   MESS(*)         !Station output message
	INTEGER*4   LEN             !Entire FE+STN message length
	INTEGER*4   TEMP            !Work variable
	INTEGER*4   OUTBYT          !Byte index for output message
	INTEGER*4   FLAGS, I, FEHDRLEN, TXHDRLEN
C
C GET THE TRANSPORT LAYER HEADER LENGTH.
C
	CALL ILBYTE(TXHDRLEN,ORGMESS,X2PRO_OFFSET-1)
	TXHDRLEN=TXHDRLEN-1
C
C GET THE FE HEADER LENGTH.
C
	CALL ILBYTE(FEHDRLEN,ORGMESS,X2FEMES_HEADLEN-1+TXHDRLEN)
	OUTBYT=0
	TEMP=0
	MESLEN=0
C
C COPY THE INPUT MESSAGE HEADER TO THE OUTPUT MESSAGE.
C NOTE: THE TRANSPORT HEADER AND FE HEADER ARE COPIED.
C
CV02	DO 100 I=TXHDRLEN+1,(TXHDRLEN+1)+FEHDRLEN-1
	DO 100 I=TXHDRLEN+1,(TXHDRLEN+1)+FEHDRLEN+1		!V02
	  OUTBYT=OUTBYT+1
	  CALL ILBYTE(TEMP,ORGMESS,I-1)
	  CALL ISBYTE(TEMP,MESS,OUTBYT-1)
100	CONTINUE
C
C CREATE THE OUTPUT FE MESSAGE. NOTE: ALL OUTPUT INFORMATION
C (FE CONNECT ID, ADDRESS LEN, DESTINATION ADDRESS, ETC) ARE
C CONTAINED IN THE ORGINAL INPUT MESSAGE AND HAVE BEEN COPIED
C TO THE OUTPUT MESSAGE.  ONLY THE MESSAGE TYPE, FLAGS, AND
C CONNECT/DISCONNECT CONTROL FIELDS ARE CHANGED.
C
	CALL ISBYTE(0,MESS,X2FEMES_HOST_ID-1)
	CALL ISBYTE(X2FEMES_MESTYP_DOWN,MESS,X2FEMES_MESTYP-1)
	CALL ISBYTE(0,MESS,X2FEMES_FLAGS-1)
	CALL ISBYTE(X2FEMES_UNCDDISC,MESS,X2FEMES_CONNCTL-1)
C
C FE MESSAGE COMPLETE, NOW BUILT A STATION MESSAGE TO
C INFORM THE STATION TO DISCONNECT AND RESET.
C
	CALL ISBYTE(X2STMES_PROTID_VAL,MESS,
     *	            X2STMES_PROTID-1+FEHDRLEN)
	CALL ISBYTE(X2STMES_DATATYPE_CMD_DOWN,MESS,
     *	            X2STMES_DATATYPE-1+FEHDRLEN)
	CALL ISBYTE(X2STMES_SOFT_RESET,MESS,
     *	            X2STMES_CODE-1+FEHDRLEN)
C
C ZERO OUT THE STATION NUMBER, CONFIGURATION, AND
C PORT NUMBER.
C
	TEMP=0
	CALL I4TOBUF2(TEMP,MESS,X2STMES_STATION_NO-1+FEHDRLEN)
	CALL ISBYTE(0,MESS,X2STMES_CONFCHK-1+FEHDRLEN)
	CALL ISBYTE(0,MESS,X2STMES_STN_PORT-1+FEHDRLEN)
C
C SET THE OUTGOING FLAGS. NOTE: DISCONNECT FLAG SET
C TO UNCONDITIONAL DISCONNECT.
C
	FLAGS=X2STMES_RE+X2STMES_DIS_UNC
	CALL ISBYTE(FLAGS,MESS,X2STMES_FLAGS-1+FEHDRLEN)
C
C SET THE STATION MESSAGE LENGTH AND DETERMINE THE OUTPUT MESLEN
C FOR THE ROUTINE.
C
	CALL I2TOBUF2(0,MESS,X2STMES_MESLEN-1+FEHDRLEN)
C
C SET THE ENTIRE MESSAGE LENGTH INTO THE FE MESSAGE.
C
	LEN=FEHDRLEN+X2STMES_HDRLEN
	CALL I4TOBUF2(LEN,MESS,X2FEMES_MESLEN-1)
C
C PROGRAM EXIT.
C
	MESLEN=LEN
	RETURN
	END
