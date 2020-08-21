C
C SUBROUTINE X2BSCONF.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BSCONF.FOV                                 $
C  $Date::   17 Apr 1996 16:11:28                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C X2BSCONF.FTN
C
C V03 19-JUL-94 WS MULTINETWORK CHANGES
C V02 16-FEB-94 JWE Port from Sweden
C V01 18-DEC-91 WS/MF BROADCAST SERVER (BCST) - REWRITE OF X2BRCONF
C
C This subroutine creates the broadcast server (BCST) configuration
C message for the specified station.  The station receives the
C BCST parameters and a list of BCST addresses which the station
C is to connect to.
C
C In Ver., 1 BCST statistics assumes that the BCST2 address (future use)
C can be defined only after a valid BCST1 address. In other words,
C bit 1 of BITMAP reports status of BCST1 connection, and bit 2 of
C BCST2 connection.
C
C This routine decreases the count of active station connection(s)
C recorded in the bitmap and clears the bitmap. The bitmap and the count
C will be updated by the station BCST statistics message.
C
C Calling sequence:
C
C     CALL X2BSCONF(TRABUF,MESS,MESLEN)
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
C Copyright 1990, 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE X2BSCONF(TRABUF,MESS,MESLEN)
      IMPLICIT NONE
C
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE	'INCLIB:X2XCOM.DEF'
	INCLUDE	'INCLIB:X2STMES.DEF'
	INCLUDE	'INCLIB:DESTRA.DEF'
C
      INTEGER*2   MESLEN                  !Output message length
      INTEGER*4   MESS(*)                 !Output message
      INTEGER*4   TEMP                    !Work variable
      INTEGER*4   OFF                     !Data offset relative to hdr
      INTEGER*4   LSTBYT                  !Byte offset into buffer
      CHARACTER   C1TEMP(4)*1             !Work variable
      EQUIVALENCE (TEMP,C1TEMP)
C
      INTEGER*4   BCST_NUM                !BCST - from enable flags
      INTEGER*4   BCST_NUM_ACTUAL         !BCST - after verification
      INTEGER*4   BCST_AUTO_REPORT        !BCST Work variable
      INTEGER*4   BCST_RETRY_INTERVAL     !BCST Work variable
      INTEGER*4   BCST_RETRY_LIMIT        !BCST Work variable
      INTEGER*4   BCST_ENABLE1            !BCST Work variable
      INTEGER*4   BCST_ENABLE2            !BCST Work variable
      INTEGER*4   BCST_PACK_LEN           !BCST CURRENT ADDR FLD LEN
      INTEGER*4   BCST_PACK_TOT           !BCST ACCUM   ADDR FLD LEN
C
	INTEGER*4   STN
CV03	INTEGER*4   LAST_NO_BCST
	INTEGER*4   NPORT
	INTEGER	    CHKVAL
	INTEGER*4   CLASS		  !V03
	INTEGER*4   BCST_ENABLE		  !V03
C
	INTEGER*4   TMP_ADR(2)
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
C	START OF V03 CHANGE BLOCK
C
	CLASS=X2XS_STNCLS(STN)
	IF (CHKVAL(CLASS,1,X2XC_CLASSES,' CLASS NUMBER ').NE.0) THEN
      	  MESLEN=-1
	  GOTO 8000
	ENDIF

C DETERMINE ACTUAL VALUES FROM THE GBL AND STN BCST PARAMETERS
C
	BCST_ENABLE = ICHAR(X2XS_BCST_ENABLE(STN))
	BCST_ENABLE1 = X2XC_BCST_ENABLE1(CLASS)      ! work variable
	BCST_ENABLE2 = X2XC_BCST_ENABLE2(CLASS)      ! work variable
C
C	END OF V03 CHANGE BLOCK
C
      IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *   TYPE *,' BCST GBL: ',
     *     'ENA INTV LIM AUTO ROUTE :', 	   !V03
     *     X2X_BCST_ENABLE,		 	   !V03
     *     X2X_BCST_RETRY_INTERVAL,X2X_BCST_RETRY_LIMIT,
     *     X2X_BCST_AUTO_REPORT,   X2X_BCST_ROUTE
C
      IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *   TYPE *,' BCST STN: ',STN,
     *     ' ENA1 2 INTV LIM AUTO :',
     *     BCST_ENABLE1,       BCST_ENABLE2, BCST_ENABLE,		!V03
     *     X2XC_BCST_RETRY_INTERVAL(CLASS),X2XC_BCST_RETRY_LIMIT(CLASS),!V03
     *     X2XC_BCST_AUTO_REPORT(CLASS)          !  X2XS_BCST_ROUTE(STN)!V03
C
      IF (X2X_BCST_ENABLE .EQ. 0 .AND. BCST_ENABLE.EQ.0 ) THEN ! if global default - ENABLE BCST - V03
        IF(BCST_ENABLE1 .EQ. 0 .AND. BCST_ENABLE2 .EQ. 0) THEN
          BCST_NUM = 2                                 ! both ENABLED
        ELSEIF (BCST_ENABLE1 .EQ. 1 .AND. BCST_ENABLE2.EQ. 1) THEN
          BCST_NUM = 0                                 ! both DISABLED
        ELSE
          BCST_NUM = 1                                 ! one ENABLED
        ENDIF
      ELSE
        BCST_NUM = 0                                   ! all DISABLED
      ENDIF
C
      IF (X2X_BCST_AUTO_REPORT .EQ. 0) THEN ! if global default - ENABLE AUTO
        BCST_AUTO_REPORT = X2XC_BCST_AUTO_REPORT(CLASS)	!V03
      ELSE
        BCST_AUTO_REPORT  = 1                         !DISABLE
      ENDIF
C
      IF (X2X_BCST_RETRY_INTERVAL .EQ. 0) THEN  ! if global default set to 0
        BCST_RETRY_INTERVAL = X2XC_BCST_RETRY_INTERVAL(CLASS)	!V03
      ELSE
        BCST_RETRY_INTERVAL  = X2X_BCST_RETRY_INTERVAL
      ENDIF
C
      IF (X2X_BCST_RETRY_LIMIT .EQ. 0) THEN   ! if global retry count "forever"
        BCST_RETRY_LIMIT = X2XC_BCST_RETRY_LIMIT(CLASS)	!V03
      ELSE
        BCST_RETRY_LIMIT = X2X_BCST_RETRY_LIMIT
      ENDIF
C
      IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *  TYPE *,' BCST CONF: ',
     *     ' ENA1 2 INTV LIM AUTO :',
     *     BCST_ENABLE1,       BCST_ENABLE2,
     *     BCST_RETRY_INTERVAL,BCST_RETRY_LIMIT,
     *     BCST_AUTO_REPORT                          !   BCST_ROUTE
C
      IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *  TYPE *,' BCST NUM: ', BCST_NUM
C
C
C STORE BCST CONFIGURATION INTO MESSAGE.
C
      CALL ISBYTE(X2STMES_BCST_RESP,MESS,OFF+X2STMES_CMD-1)     !CMD
      TEMP=0
      C1TEMP(4)=X2XS_CONF(STN)
      CALL ISBYTE(TEMP,MESS,OFF+X2STMES_BCST_CNTNO-1)           !CNTNO
      CALL I4TOBUF2(STN,MESS,OFF+X2STMES_BCST_STNO-1)           !STNNO
      CALL ISBYTE(X2XS_ADRESS_LEN(STN),MESS,OFF+                !STN ADRLEN
     *            X2STMES_BCST_CMD_ADRLEN-1)
      TMP_ADR(1) = X2XS_ADRESS(1,STN)
      TMP_ADR(2) = X2XS_ADRESS(2,STN)
      CALL X2QSHFT(TMP_ADR, 64-(X2XS_ADRESS_LEN(STN)*4))
      CALL I4TOBUF4(TMP_ADR(1),MESS,OFF+                !STN ADDR
     *              X2STMES_BCST_CMD_ADR-1)
      CALL I4TOBUF4(TMP_ADR(2),MESS,OFF+
     *              X2STMES_BCST_CMD_ADR+4-1)
      CALL ISBYTE(BCST_AUTO_REPORT,                             !AUTO REPORT
     *                   MESS,OFF+X2STMES_BCST_AUTO_REPORT-1)
      CALL I4TOBUF2(BCST_RETRY_INTERVAL,                        !RETRY INT
     *                   MESS,OFF+X2STMES_BCST_RETRY_INT-1)
      CALL I4TOBUF2(BCST_RETRY_LIMIT,                           !RETRY CNT
     *                   MESS,OFF+X2STMES_BCST_RETRY_CNT-1)
      CALL ISBYTE(BCST_NUM,                                     !BCST ADDR
     *                   MESS,OFF+X2STMES_BCST_ADDR_NUM-1)
      LSTBYT = X2STMES_BCST_ADRLEN           ! prior to fst BCST len field
CV03     LAST_NO_BCST=LSTBYT
C
      BCST_PACK_LEN = 0
      BCST_PACK_TOT = 0
      BCST_NUM_ACTUAL = 0

      IF (BCST_NUM .GT. 0 .AND. BCST_ENABLE1 .EQ. 0 ) THEN    ! PORT1 enabled
         NPORT = X2XC_BCST_NET_PORT1(CLASS)		      !V03
         IF (NPORT .EQ. 0 .OR. NPORT .GT. X2X_NETWORK_PORTS) THEN
            CALL OPS('Invalid BCST1 port for station ',NPORT,STN)
         ELSEIF (X2XPN_TYPE(NPORT) .EQ. X2XPT_BCST1) THEN
           IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *       TYPE *,' BCST STN/NPORT1: ', STN,NPORT
           CALL ISBYTE(X2XPN_ADDLEN(NPORT),MESS,                !BCST1 ADDRLEN
     *             OFF+X2STMES_BCST_ADRLEN-1)
	   TMP_ADR(1) = X2XPN_ADRESS(1,NPORT)
	   TMP_ADR(2) = X2XPN_ADRESS(2,NPORT)
	   CALL X2QSHFT(TMP_ADR,64-(X2XPN_ADDLEN(NPORT)*4))
           CALL I4TOBUF4(TMP_ADR(1),MESS,            !BCST1 ADDR
     *             OFF+X2STMES_BCST_ADDR-1)
           CALL I4TOBUF4(TMP_ADR(2),MESS,
     *             OFF+X2STMES_BCST_ADDR+4-1)
C
C          Round up to even num of BCD, then even num of bytes and add 1
C
           BCST_PACK_LEN =(((1+ X2XPN_ADDLEN(NPORT))/2)+1)/2*2+1
           BCST_PACK_TOT = BCST_PACK_TOT +BCST_PACK_LEN
           LSTBYT = LSTBYT + BCST_PACK_LEN
           BCST_NUM_ACTUAL = BCST_NUM_ACTUAL + 1

C	START OF V03 CHANGE BLOCK
      	   IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *	      TYPE *,'BCST - NPORT, GBL ACT#: ',NPORT,
     *		  X2XPN_BCST_ACTIVE_CNT(NPORT)
C
      	   IF(IAND(X2XS_BCST_ACTIVE_BITMAP(STN),
     *	      X2STMES_STSBCST_CONN1_MASK).NE.0)          ! bit 1 was set
     *	     X2XPN_BCST_ACTIVE_CNT(NPORT) = 
     *		      MAX(X2XPN_BCST_ACTIVE_CNT(NPORT)-1,0)
      	ELSE
      	      CALL OPS('Invalid BCST1 port type for stn:',NPORT,STN)
      	ENDIF
      ENDIF
C	END OF V03 CHANGE BLOCK
C
      IF (BCST_NUM .GT. 0 .AND. BCST_ENABLE2 .EQ. 0 ) THEN  !PORT2 enabled
         NPORT = X2XC_BCST_NET_PORT2(CLASS)		    !V03
         IF (NPORT .EQ. 0 .OR. NPORT .GT. X2X_NETWORK_PORTS) THEN
           CALL OPS('Invalid BCST2 port for station ',NPORT,STN)
         ELSEIF (X2XPN_TYPE(NPORT).EQ.X2XPT_BCST2) THEN
           IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *       TYPE *,' BCST STN/NPORT2: ', STN,NPORT
           CALL ISBYTE(X2XPN_ADDLEN(NPORT),MESS,                !BCST2 ADDRLEN
     *            OFF+BCST_PACK_TOT+X2STMES_BCST_ADRLEN-1)
	   TMP_ADR(1) = X2XPN_ADRESS(1,NPORT)
	   TMP_ADR(2) = X2XPN_ADRESS(2,NPORT)
	   CALL X2QSHFT(TMP_ADR,64-(X2XPN_ADDLEN(NPORT)*4))
           CALL I4TOBUF4(TMP_ADR(1),MESS,            !BCST2 ADDR
     *            OFF+BCST_PACK_TOT+X2STMES_BCST_ADDR-1)
           CALL I4TOBUF4(TMP_ADR(2),MESS,
     *            OFF+BCST_PACK_TOT+X2STMES_BCST_ADDR+4-1)
C
C          Round up to even num of BCD, then even num of bytes and add 1
C
           BCST_PACK_LEN =(((1+ X2XPN_ADDLEN(NPORT))/2)+1)/2*2+1
           BCST_PACK_LEN = BCST_PACK_LEN + 1                    ! ADDLEN FIELD
           BCST_PACK_TOT = BCST_PACK_TOT +BCST_PACK_LEN
           LSTBYT = LSTBYT + BCST_PACK_LEN
           BCST_NUM_ACTUAL = BCST_NUM_ACTUAL + 1
C***       IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
C*** *        TYPE *,'BCST PACK: ',BCST_PACK_LEN, BCST_PACK_TOT,LSTBYT
C	START OF V03 CHANGE BLOCK
      	   IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     *	      TYPE *,'BCST - NPORT, GBL ACT#: ',NPORT,
     *		  X2XPN_BCST_ACTIVE_CNT(NPORT)
C
      	   IF(IAND(X2XS_BCST_ACTIVE_BITMAP(STN),
     *	      X2STMES_STSBCST_CONN2_MASK).NE.0)          ! bit 1 was set
     *	     X2XPN_BCST_ACTIVE_CNT(NPORT) = 
     *		      MAX(X2XPN_BCST_ACTIVE_CNT(NPORT)-1,0)
C	END OF V03 CHANGE BLOCK
         ELSE
           CALL OPS('Invalid BCST2 port TYPE for stn. ',NPORT,STN)
         ENDIF
      ENDIF
C
      CALL ISBYTE(BCST_NUM_ACTUAL,MESS,OFF+X2STMES_BCST_ADDR_NUM-1) !NUM BCSTs
      X2XS_BCST_NUM(STN) = BCST_NUM_ACTUAL
C
      MESLEN=LSTBYT-1
C
C***  CLEAR BITS, UPDATE COUNTS. THE FIRST BIT MUST BE BCST1,...
C
C
C
      X2XS_BCST_ACTIVE_BITMAP(STN)=0
C
      IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0)
     * TYPE *,'RETURN BCST - NUM_ACT,LEN,ACT#: ',
     * BCST_NUM_ACTUAL, MESLEN				!V03
C
C PROGRAM EXIT.
C
8000  CONTINUE
      RETURN
      END
