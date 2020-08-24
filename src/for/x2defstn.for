C
C SUBROUTINE X2DEFSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2DEFSTN.FOV                                 $
C  $Date::   17 Apr 1996 16:14:46                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2defstn.for;1 **
C
C X2DEFSTN.FOR
C
C V04 05-DEC-94 SCD GET STATION AND STATION CLASS INFO FROM MEMORY INSTEAD
C		    OF READING FILES - Integrate UK changes into X2X Baseline
C V03 28-APR-94 XXX GET STATION CLASS INFO FROM MEMORY
C V02 25-MAR-94 GPR CORRECT THE STATION ADDRESS IN THE MESS BUFF
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will create the station default parameters
C message.
C
C Calling sequence:
C
C     CALL X2DEFSTN(TRABUF,MESS,MESLEN,LINE)
C
C Input parameters:
C
C     TRABUF      Int*4(TRALEN)       Transaction buffer
C
C Output parameters:
C
C     MESS        Int*4(*)            Output message
C     MESLEN      Int*2               Length of message buffer
C                                     Read error MESLEN=-1
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
	SUBROUTINE X2DEFSTN(TRABUF,MESS,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
CV04	INCLUDE 'INCLIB:X2XSTN.DEF'
CV04	INCLUDE 'INCLIB:X2XSCL.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*2   MESLEN                          !Output message length
	INTEGER*4   MESS(*)                         !Output message
	INTEGER*4   OFF                             !Data offset relative to hdr
	INTEGER*4   STN                             !Station number
        INTEGER*4   TEMP_ADR(2)                     !Address work variable
	INTEGER*4   CHKVAL
        LOGICAL     OLD_PROTO                       !Station protocol flag
	INTEGER*4   CLASS			    !V04
C
C INITIALIZE VARIABLES.
C
	STN=TRABUF(TXSTN)
	MESLEN=0
	OFF=X2STMES_DATA-1
C
C DETERMINE IF THIS IS AN OLD OR NEW STATION
        OLD_PROTO = .TRUE.
        IF((TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_DEFAULT_CONF2)
     *    .OR. (TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_RESET2))
     *                     OLD_PROTO = .FALSE.
C
C UPDATE THE STATION DATA HEADER.
C
	CALL ISBYTE(X2STMES_PROTID_VAL,MESS,X2STMES_PROTID-1)
	CALL ISBYTE(X2STMES_DATATYPE_CMD_DOWN,MESS,
     *	            X2STMES_DATATYPE-1)
	CALL ISBYTE(0,MESS,X2STMES_CONFCHK-1)
	CALL I4TOBUF2(STN,MESS,X2STMES_STATION_NO-1)
	CALL ISBYTE(X2STMES_DEF_CONF,MESS,X2STMES_CODE-1)
	CALL ISBYTE(X2STMES_DEF_STN,MESS,X2STMES_DEF_TYPE-1)
C
C CHECK FOR VALID STATION NUMBER.
C
	IF(CHKVAL(STN,1,X2X_STATIONS,' STATION NUMBER ').NE.0) THEN
	  MESLEN=-1
	  GOTO 8000
	ENDIF
C
C READ THE STATION RECORD.
C
CV04	CALL READW(X2XSTN_FDB,STN,X2XSTN_REC,ST)
CV04	IF(ST.NE.0) THEN
CV04	  CALL OPS('X2DEFSTN: Error reading X2XSTN.FIL ',STN,ST)
CV04	  MESLEN=-1
CV04	  GOTO 8000
CV04	ENDIF
C
C CHECK TO ENSURE THE STATION INFORMATION
C EXISTS.
C
CV04	IF(X2XSTN_REC(1).LE.0) THEN
CV04	  CALL OPS('X2DEFSTN:Station does not exist',STN,TRABUF(TSER))
CV04	  MESLEN=-1
CV04	  GOTO 8000
CV04	ENDIF
C
C READ THE STATION CLASS RECORD FROM MEMORY
C
CV04        DO 600 JJ = 1, X2XSC_REC_LEN
CV04            X2XSCL_REC(JJ) = X2XSC_MEM_STORE(JJ,X2XSTN_STNCLS)
CV04600     CONTINUE
C
C CHECK TO ENSURE THE STATION CLASS EXISTS.
C
	CLASS=X2XS_STNCLS(STN)					!V04
	IF(CLASS.LE.0) THEN					!V04
	  CALL OPS('X2DEFSTN:Class does not exist ',		!V04
     *	            STN,CLASS)					!V04
	  MESLEN=-1
	  GOTO 8000
	ENDIF
C
C STORE STATION PARAMETERS INTO MESSAGE.
C
	CALL ISBYTE(X2STMES_DEF_CONF,MESS,OFF+X2STMES_STN_CMD-1)
	CALL ISBYTE(0,MESS,OFF+X2STMES_STN_CNTNO-1)
	CALL I4TOBUF2(STN,MESS,OFF+X2STMES_STN_STNO-1)
	CALL ISBYTE(X2XS_ADRESS_LEN(STN),MESS,OFF+X2STMES_STN_ADR_LEN-1)!V04
        TEMP_ADR(1) = X2XS_ADRESS(1,STN)				!V04
        TEMP_ADR(2) = X2XS_ADRESS(2,STN)				!V04
        CALL X2QSHFT(TEMP_ADR,64-(X2XS_ADRESS_LEN(STN)*4))   !right justify-V04
	CALL I4TOBUF4(TEMP_ADR(1),MESS,OFF+X2STMES_STN_ADR-1)
        CALL I4TOBUF4(TEMP_ADR(2),MESS,OFF+X2STMES_STN_ADR-1+4)         ! V02
        CALL I4TOBUF2(X2XC_L2TOUT(CLASS),MESS,OFF+X2STMES_STN_L2TOUT-1) !V04
	CALL I4TOBUF2(X2XC_INTIM(CLASS),MESS,OFF+X2STMES_STN_INCALL-1)	!V04
	CALL I4TOBUF2(X2XC_OUTTIM(CLASS),MESS,OFF+X2STMES_STN_OUTCALL-1)!V04
	CALL I4TOBUF2(X2XC_RESTIM(CLASS),MESS,OFF+X2STMES_STN_RESP-1)	!V04
	CALL I4TOBUF2(X2XC_INTTIM(CLASS),MESS,OFF+X2STMES_STN_INTER-1)	!V04
C
        IF(.NOT.OLD_PROTO) THEN
           CALL I4TOBUF2(X2XC_SLPTIME(CLASS),MESS,			!V04
     *				      OFF+X2STMES_STN_SLPTIME-1)	!V04
           CALL I4TOBUF2(X2XC_AFTMAX(CLASS),MESS,			!V04
     *				      OFF+X2STMES_STN_AFTMAX-1)		!V04
           MESLEN=X2STMES_STN_AFTMAX+2
        ELSE
           MESLEN=X2STMES_STN_INTER+2
        ENDIF
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END
