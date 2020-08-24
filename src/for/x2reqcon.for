C
C SUBROUTINE X2REQCON
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2REQCON.FOV                                 $
C  $Date::   17 Apr 1996 16:31:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2reqcon.for;1 **
C
C X2REQCON.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C Calling Sequence:
C
C     CALL X2REQCON(TRABUF,MESS,ORGMESS,LEN)
C
C This subroutine will build a front end message requesting
C the it to sned statistics for the current status of a
C given station.
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
	SUBROUTINE X2REQCON(TRABUF,MESS,ORGMESS,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*2   MESLEN          !Output message length
	INTEGER*4   ORGMESS(*)      !Station input message
	INTEGER*4   MESS(*)         !Station output message
	INTEGER*4   STN             !Station number
	INTEGER*4   DTALEN
C
	STN=TRABUF(TXSTN)
	MESLEN=0
C
C BUILD OUTPUT FRONT END MESSAGE.
C
	CALL I4TOBUF2(X2FEMES_CONSTS_MSGRCV+1,MESS,X2FEMES_MESLEN-1)
	CALL ISBYTE(X2FEMES_PROTID_X2X,MESS,X2FEMES_PROTID-1)
	CALL ISBYTE(0,MESS,X2FEMES_HOST_ID-1)
	CALL ISBYTE(X2FEMES_MESTYP_CMD,MESS,X2FEMES_MESTYP-1)
	CALL ISBYTE(X2FEMES_FORM_X2X,MESS,X2FEMES_FORMAT-1)
	CALL I4TOBUF2(X2FEMES_MESCOD_CONSTS,MESS,X2FEMES_MESCOD-1)
	DTALEN=X2FEMES_CONSTS_MSGRCV-X2FEMES_DATALEN
	CALL I4TOBUF2(DTALEN,MESS,X2FEMES_DATALEN-1)
	CALL ISBYTE(0,MESS,X2FEMES_CONSTS_LINE-1)
	CALL I4TOBUF2(0,MESS,X2FEMES_CONSTS_CONID-1)
	CALL I4TOBUF4(0,MESS,X2FEMES_CONSTS_ABSTIM-1)
	CALL I4TOBUF4(0,MESS,X2FEMES_CONSTS_DURTIM-1)
	CALL I4TOBUF2(0,MESS,X2FEMES_CONSTS_ESTTIM-1)
	CALL ISBYTE(0,MESS,X2FEMES_CONSTS_TERCOD-1)
	CALL ISBYTE(0,MESS,X2FEMES_CONSTS_DIACOD-1)
	CALL ISBYTE(0,MESS,X2FEMES_CONSTS_NETERR-1)
	CALL ISBYTE(X2XS_ADRESS_LEN(STN),MESS,
     *	            X2FEMES_CONSTS_ADRLEN-1)
	CALL I4TOBUF4(X2XS_ADRESS(1,STN),MESS,
     *	            X2FEMES_CONSTS_STNADD-1)
	CALL I4TOBUF4(X2XS_ADRESS(2,STN),MESS,
     *	            X2FEMES_CONSTS_STNADD-1+4)
	CALL I4TOBUF2(0,MESS,X2FEMES_CONSTS_MSGSND-1)
	CALL I4TOBUF2(0,MESS,X2FEMES_CONSTS_MSGRCV-1)
	MESLEN=X2FEMES_CONSTS_MSGRCV+1
C
C PROGRAM EXIT.
C
	RETURN
	END
