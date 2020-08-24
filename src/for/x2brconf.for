C
C SUBROUTINE X2BRCONF
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BRCONF.FOV                                 $
C  $Date::   17 May 1996 11:44:26                                         $
C  $Revision::   1.2                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2brconf.for;1 **
C
C X2BRCONF.FOR
C
C V02 02-APR-96 wsm Fixed storing of relay address.
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will create the relay broadcast configuration
C for the specified station.  The station will receive the
C relay parameters and a list of all stations which the station
C will relay messages to.
C
C This routine assumes that all necessary files have been
C previously opened.
C
C Calling sequence:
C
C     CALL X2BRCONF(TRABUF,MESS,MESLEN)
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
	SUBROUTINE X2BRCONF(TRABUF,MESS,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*2   MESLEN                  !Output message length
	INTEGER*4   MESS(*)                 !Output message
	INTEGER*4   TEMP                    !Work variable
	INTEGER*4   OFF                     !Data offset relative to hdr
	INTEGER*4   LSTBYT                  !Byte offset into buffer
C
	INTEGER*4   J, TMP, LEN, CHNSTN, I, CHNPOS, CHNCNT
	INTEGER*4   GROUP, CHKVAL, STN
	INTEGER*4   TEMP_ADR(2)		    ! Work variable.
C
	CHARACTER   C1TEMP(4)*1             !Work variable
	EQUIVALENCE (TEMP,C1TEMP)
C
C INITIALIZE VARIABLES.
C
	STN=TRABUF(TXSTN)
	MESLEN=0
	OFF=X2STMES_DATA-1
	LSTBYT=0
C
C CHECK FOR VALID STATION NUMBER.
C
	IF(CHKVAL(STN,1,X2X_STATIONS,' STATION NUMBER ').NE.0) THEN
	  MESLEN=-1
	  GOTO 8000
	ENDIF
C
C STORE RELAY CONFIGURATION INTO MESSAGE.
C
	TEMP=0
	C1TEMP(1)=X2XS_CONF(STN)
	CALL ISBYTE(X2STMES_RELAY_RESP,MESS,OFF+X2STMES_CMD-1)
	CALL ISBYTE(TEMP,MESS,OFF+X2STMES_RELAY_CNTNO-1)
	CALL I4TOBUF2(STN,MESS,OFF+X2STMES_RELAY_STNO-1)
	CALL ISBYTE(X2XS_ADRESS_LEN(STN),MESS,OFF+
     *	            X2STMES_RELAY_CMD_ADRLEN-1)
	TMP=X2XS_ADRESS_LEN(STN)
	TEMP_ADR(1) = X2XS_ADRESS(1,STN)
	TEMP_ADR(2) = X2XS_ADRESS(2,STN)
        CALL X2QSHFT(TEMP_ADR,64-(TMP*4))
	CALL I4TOBUF4(TEMP_ADR(1),MESS,OFF+
     *	              X2STMES_RELAY_CMD_ADR-1)
	CALL I4TOBUF4(TEMP_ADR(2),MESS,OFF+
     *	              X2STMES_RELAY_CMD_ADR+4-1)
	CALL ISBYTE(X2XG_MAXSEG,MESS,OFF+X2STMES_RELAY_MAX_SEG-1)
	CALL ISBYTE(X2XG_RTYCNT,MESS,OFF+X2STMES_RELAY_RETRY_CNT-1)
	CALL I4TOBUF2(X2XG_RTYINT,MESS,OFF+X2STMES_RELAY_RETRY_INT-1)
	CALL I4TOBUF2(X2XG_ACKTIM,MESS,OFF+X2STMES_RELAY_ACK_TIME-1)
	CALL I4TOBUF2(X2XG_FWDTIM,MESS,OFF+X2STMES_RELAY_FWD_TIME-1)
	CALL ISBYTE(0,MESS,OFF+X2STMES_RELAY_FREE-1)
C
C EXTRACT PROGRAM VARIABLES.
C
	GROUP=X2XS_GROUP(STN)
	LSTBYT=X2STMES_RELAY_ADRLEN1
	CHNCNT=0
	IF(GROUP.EQ.0) GOTO 210
C
C DETERMINE THIS STATIONS POSITIION IN THE RELAY CHAIN.
C
	DO 100 CHNPOS=1,X2XG_CNT(GROUP)
	  IF(X2XG_LIST(CHNPOS,GROUP).EQ.STN) GOTO 150
100	CONTINUE
C
C STORE ALL OF THE STATIONS THAT THIS STATION RELAYS TO.
C NOTE: STATION ADDRESS IS OF VARIABLE LENGTH.
C
150	CONTINUE
	DO 200 I=CHNPOS+1,X2XG_CNT(GROUP)
	  CHNSTN=X2XG_LIST(I,GROUP)
	  IF(CHNSTN.EQ.0) GOTO 210
	  CALL ISBYTE(X2XS_ADRESS_LEN(CHNSTN),MESS,OFF+LSTBYT-1)
	  LSTBYT=LSTBYT+1
	  CHNCNT=CHNCNT+1
	  LEN=X2XS_ADRESS_LEN(CHNSTN)/2
	  TMP=X2XS_ADRESS_LEN(CHNSTN)
	  IF(MOD(TMP,2).NE.0) LEN=LEN+1
	  TEMP_ADR(1) = X2XS_ADRESS(1,CHNSTN)
	  TEMP_ADR(2) = X2XS_ADRESS(2,CHNSTN)
	  CALL X2QSHFT(TEMP_ADR,64-(TMP*4))
          IF (TEMP_ADR(2).NE.0) THEN
	    DO 220 J=4,1,-1
	      CALL ILBYTE(TEMP,TEMP_ADR(1),J-1)
	      CALL ISBYTE(TEMP,MESS,OFF+LSTBYT-1)
	      LSTBYT=LSTBYT+1
220	    CONTINUE
	    DO 230 J=4,9-LEN,-1
	      CALL ILBYTE(TEMP,TEMP_ADR(2),J-1)
	      CALL ISBYTE(TEMP,MESS,OFF+LSTBYT-1)
	      LSTBYT=LSTBYT+1
230	    CONTINUE
          ELSE
	    DO 240 J=LEN,1,-1
	      CALL ILBYTE(TEMP,TEMP_ADR(1),J-1)
	      CALL ISBYTE(TEMP,MESS,OFF+LSTBYT-1)
	      LSTBYT=LSTBYT+1
240	    CONTINUE
          ENDIF
200	CONTINUE
C
C STORE THE NUMBER OF STATIONS IN THE RELAY CHAIN.
C
210	CONTINUE
	CALL ISBYTE(CHNCNT,MESS,OFF+X2STMES_RELAY_ADR_CNT-1)
	MESLEN=LSTBYT-1
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END
