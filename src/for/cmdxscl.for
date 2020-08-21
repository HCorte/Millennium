C
C SUBROUTINE CMDXSCL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CMDXSCL.FOV                                  $
C  $Date::   17 Apr 1996 12:39:50                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - cmdxsub.for;1 **
C
C V09 22-AUG-95 DAS MODIFIED TO HANDLE BACKGROUND DOWNLOADS
C V08 10-MAR-95 SCD LOAD X2XC_DIAL_OV_OVERRIDE (especially important for
C                    X.28 GVTs)
C V07 29-DEC-94 WJK MOVE UNSOLICITED STATION CONNECT AND DISCONNECT FROM GLOBAL
C                   TO STATION CLASS
C V06 20-OCT-94 GPR LOAD OPTIMIZE PORTS VALUE
C V05 20-OCT-94 GPR LOAD UNSO MSGS VALUE
C V04 22-SEP-94 GPR LOAD DIAL TYPE
C V03 21-AUG-94 GPR ALLOW FOR MORE STATION CLASS FIELDS TO BE LOADED
C V02 20-JUL-94 WS MULTINETWORK CHANGES
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
C
C =============================================================
C CMDXSCL
C
C This subroutine loads the station class information into
C common.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CMDXSCL(FIELD,ALLREC,ADDFLG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
C
	INTEGER*4   FIELD           !Field number
	INTEGER*4   ALLREC(128)      !Record buffer
	INTEGER*4   ADDFLG          !New Station class - NOT USED
	INTEGER*4   CLASS	    !Station class number
	INTEGER*4   HNTPRT, I, STN
	INTEGER*4   TEMP, CONF
	INTEGER*4   SUBNETWORK
C
C STORE THE INFORMATION INTO COMMON.
C

	CALL FASTMOV(ALLREC,X2XSCL_REC,128)
	CLASS = X2XSCL_CLASS			
	IF(CLASS.LE.0.OR.CLASS.GT.X2XC_CLASSES) GOTO 8000 

        DO 100 I=1,X2X_STATIONS
	  IF (X2XS_STNCLS(I).NE.CLASS) GOTO 100
          CALL ILBYTE(CONF,IX2XS_CONF,I-1)
          CONF=CONF+'00000020'X
          CALL ISBYTE(CONF,IX2XS_CONF,I-1)
100     CONTINUE    

C***	TYPE *,'FIELD, CLASS ',FIELD, CLASS
        IF(FIELD.EQ.1)  THEN                              !V03
          X2XC_CLASS(CLASS)  = X2XSCL_CLASS               !V03
        ELSEIF(FIELD.EQ.5)  THEN                          !V03
          X2XC_INTIM(CLASS)  = X2XSCL_INTIM               !  
        ELSEIF(FIELD.EQ.6)  THEN                          !  
          X2XC_OUTTIM(CLASS) = X2XSCL_OUTTIM              !  
        ELSEIF(FIELD.EQ.7)  THEN                          !  
          X2XC_RESTIM(CLASS) = X2XSCL_RESTIM              !  
        ELSEIF(FIELD.EQ.8)  THEN                          !  
          X2XC_INTTIM(CLASS) = X2XSCL_INTTIM              !  
        ELSEIF(FIELD.EQ.9)  THEN                          !  
          X2XC_STSTIM(CLASS) = X2XSCL_STSTIM              !  
        ELSEIF(FIELD.EQ.10) THEN                          !  
          X2XC_MSTENA(CLASS) = CHAR(X2XSCL_MSTENA)        !  
        ELSEIF(FIELD.EQ.11) THEN                          !  
          X2XC_RETRY(CLASS)  = X2XSCL_RETRY               !  
C
C	***** Start V03 changes *****
C
        ELSEIF(FIELD.EQ.12) THEN                          !  
          X2XC_PROTO(CLASS)  = X2XSCL_PROTO               !  
        ELSEIF(FIELD.EQ.13) THEN                          !  
          X2XC_PRTCNT(CLASS)  = X2XSCL_PRTCNT
        ELSEIF(FIELD.EQ.14) THEN
          X2XC_TYPE(CLASS)  = X2XSCL_TYPE
        ELSEIF(FIELD.EQ.15) THEN
          X2XC_DELACK(CLASS)  = X2XSCL_DELACK
        ELSEIF(FIELD.EQ.16) THEN
          X2XC_ERRREP(CLASS)  = X2XSCL_ERRREP
        ELSEIF(FIELD.EQ.17) THEN
          X2XC_STNDIS(CLASS)  = X2XSCL_STNDIS
        ELSEIF(FIELD.EQ.18) THEN
          X2XC_FEDIS(CLASS)  = X2XSCL_FEDIS
	ELSEIF(FIELD.GE.19 .AND. FIELD.LE.25) THEN
	  IF (FIELD-18.LE.X2X_MAXPRT_ASSIGN)
     *	      X2XC_NETPORT(FIELD-18,CLASS)=X2XSCL_NETPORT(FIELD-18)
        ELSEIF(FIELD.EQ.26) THEN                          !  
          X2XC_POLTIM(CLASS)  = X2XSCL_POLTIM             !  
        ELSEIF(FIELD.EQ.27) THEN
          X2XC_ADDLEN(CLASS)  = X2XSCL_ADDLEN
        ELSEIF(FIELD.EQ.28) THEN
          X2XC_NETSTAT(CLASS)  = X2XSCL_NETSTAT
	ELSEIF(FIELD.EQ.29) THEN
	  X2XC_AUTOUPD(CLASS) = X2XSCL_AUTOUPD
	ELSEIF(FIELD.EQ.30) THEN
	  X2XC_BAUD(CLASS) = X2XSCL_BAUD
	ELSEIF(FIELD.EQ.31) THEN
	  X2XC_TTN_PORT1(CLASS) = X2XSCL_TTN_PORT1
	ELSEIF(FIELD.EQ.32) THEN
	  X2XC_TTN_PORT2(CLASS) = X2XSCL_TTN_PORT2
C
C	***** End V03 changes *****
C
        ELSEIF(FIELD.EQ.33)  THEN                         !  
          X2XC_SITE(1,CLASS) = X2XSCL_SITE1               !  
        ELSEIF(FIELD.EQ.34)  THEN                         !  
          X2XC_SITE(2,CLASS) = X2XSCL_SITE2               !  
        ELSEIF(FIELD.EQ.35)  THEN                         !  
          X2XC_SITE(3,CLASS) = X2XSCL_SITE3               !  
        ELSEIF(FIELD.EQ.36)  THEN                         !  
          X2XC_SITE(4,CLASS) = X2XSCL_SITE4               !  
        ELSEIF(FIELD.EQ.37)  THEN                         !  
          X2XC_SITE(5,CLASS) = X2XSCL_SITE5               !  
        ELSEIF(FIELD.EQ.38)  THEN                         !  
          X2XC_SITE(6,CLASS) = X2XSCL_SITE6               !  
        ELSEIF(FIELD.EQ.39)  THEN                         !  
          X2XC_SITE(7,CLASS) = X2XSCL_SITE7               !  
        ELSEIF(FIELD.EQ.40)  THEN                         !  
          X2XC_DIAL_ENABLE(CLASS) = CHAR(X2XSCL_DIALENA)  !  
C
C	***** Start V03 changes *****
C
        ELSEIF(FIELD.EQ.41)  THEN
          X2XC_DIAL_PORT(1,CLASS) = X2XSCL_DIAL_PORT1
        ELSEIF(FIELD.EQ.42)  THEN
          X2XC_DIAL_PORT(2,CLASS) = X2XSCL_DIAL_PORT2
C
C	***** End V03 changes *****
C
	ELSEIF(FIELD.EQ.43)  THEN
	  X2XC_L2TOUT(CLASS) = X2XSCL_L2TOUT
	ELSEIF(FIELD.EQ.44)  THEN
	X2XC_DISX25ROM(CLASS) = X2XSCL_DISX25ROM
	ELSEIF(FIELD.EQ.45)  THEN
	X2XC_DISDUPROM(CLASS) = X2XSCL_DISDUPROM
	ELSEIF(FIELD.EQ.46)  THEN
	X2XC_DISX25RAM(CLASS) = X2XSCL_DISX25RAM
        ELSEIF(FIELD.EQ.50) THEN
          X2XC_SITE(8,CLASS) = X2XSCL_X32_SITE1
        ELSEIF(FIELD.EQ.51) THEN
          X2XC_SITE(9,CLASS) = X2XSCL_X32_SITE2
        ELSEIF(FIELD.EQ.52) THEN
          X2XC_SLPTIME(CLASS) = X2XSCL_SLPTIME
        ELSEIF(FIELD.EQ.53) THEN
          X2XC_AFTMAX(CLASS) = X2XSCL_AFTMAX
        ELSEIF(FIELD.EQ.59) THEN			  !V03
          X2XC_EVSN_LEN(CLASS) = X2XSCL_EVSN_LEN	  !V03
        ELSEIF(FIELD.EQ.60) THEN
          X2XC_CLOCK(CLASS) =  CHAR(X2XSCL_CLOCK)
        ELSEIF(FIELD.EQ.61) THEN
          X2XC_SYNC(CLASS)  =  CHAR(X2XSCL_SYNC)
        ELSEIF(FIELD.EQ.62) THEN
          X2XC_ABS_TIM(CLASS) = X2XSCL_ABS_DISC_TIM
        ELSEIF(FIELD.EQ.63) THEN                                    !V08
          X2XC_DIAL_OV_OVERRIDE(CLASS) = X2XSCL_DIAL_OV_OVERRIDE    !V08
C	START OF V02 CHANGE BLOCK
        ELSEIF(FIELD.EQ.64) THEN
	  X2XC_SUBNETWORK(CLASS) = X2XSCL_SUBNETWORK
	ELSEIF(FIELD.EQ.65) THEN
	    X2XC_BCST_RETRY_INTERVAL(CLASS)=X2XSCL_BCST_RETRY_INTERVAL
	ELSEIF(FIELD.EQ.66) THEN
	    X2XC_BCST_RETRY_LIMIT(CLASS)=X2XSCL_BCST_RETRY_LIMIT
	ELSEIF(FIELD.EQ.67) THEN
	    X2XC_BCST_AUTO_REPORT(CLASS)= X2XSCL_BCST_AUTO_REPORT
	ELSEIF(FIELD.EQ.68) THEN
	    X2XC_BCST_ENABLE1(CLASS)= X2XSCL_BCST_ENABLE1
	ELSEIF(FIELD.EQ.69) THEN
	    X2XC_BCST_ENABLE2(CLASS)= X2XSCL_BCST_ENABLE2
	ELSEIF(FIELD.EQ.70) THEN
      	  X2XC_BCST_NET_PORT1(CLASS)=  X2XSCL_BCST_NET_PORT1
      	  IF( X2XSCL_BCST_ENABLE1 .EQ.0 ) THEN
      	      IF( X2XSCL_BCST_NET_PORT1 .EQ. X2XPT_UNDEF) THEN
      		  CALL OPS('BCST1 port: UNDEF, CLASS ',CLASS,0)
	      ELSEIF(X2XPN_TYPE(X2XSCL_BCST_NET_PORT1).NE.
     *		  X2XPT_BCST1) THEN
      		  CALL OPS('BCST1 port: bad type, CLASS ',CLASS,
     *                       X2XPN_TYPE(X2XSCL_BCST_NET_PORT1))
      	      ENDIF
      	  ENDIF
	ELSEIF(FIELD.EQ.71) THEN
      	  X2XC_BCST_NET_PORT2(CLASS)=X2XSCL_BCST_NET_PORT2
      	  IF( X2XSCL_BCST_ENABLE2 .EQ.0 ) THEN
      	      IF( X2XSCL_BCST_NET_PORT2 .EQ. X2XPT_UNDEF) THEN
      		  CALL OPS('BCST2 port: UNDEF, CLASS ',CLASS,0)
	      ELSEIF(X2XPN_TYPE(X2XSCL_BCST_NET_PORT2).NE.
     *		  X2XPT_BCST2) THEN
      		  CALL OPS('BCST2 port: bad type, CLASS ',CLASS,
     *                       X2XPN_TYPE(X2XSCL_BCST_NET_PORT2))
      	      ENDIF
      	  ENDIF
C	END OF V02 CHANGE BLOCK
	ELSEIF (FIELD.EQ.72) THEN
	    X2XC_REPCLS(CLASS)=X2XSCL_REPCLS
	ELSEIF (FIELD.EQ.73) THEN
	    X2XC_DEF_PORT_OVERRIDE(CLASS)=X2XSCL_DEF_PORT_OVERRIDE
	ELSEIF (FIELD.EQ.74) THEN
	    X2XC_MAX_THRUPUT(CLASS)=X2XSCL_MAX_THRUPUT
	    SUBNETWORK=X2XC_SUBNETWORK(CLASS)
	    X2XSN_MAX_THRUPUT(SUBNETWORK)=X2XSCL_MAX_THRUPUT
	ELSEIF (FIELD.EQ.75) THEN					    !V04
	    X2XC_DIAL_TYPE(CLASS)=X2XSCL_DIAL_TYPE			    !V04
	ELSEIF (FIELD.EQ.76) THEN					    !V05
	    X2XC_UNSO_MSGS(CLASS)=X2XSCL_UNSO_MSGS			    !V05
	ELSEIF (FIELD.EQ.77) THEN					    !V06
	    X2XC_OPTIMIZE_SITES(CLASS)=X2XSCL_OPTIMIZE_SITES		    !V06
	ELSEIF (FIELD.EQ.78) THEN
	    X2XC_MAX_THRUPUT_SEND(CLASS)=X2XSCL_MAX_THRUPUT_SEND
	    SUBNETWORK=X2XC_SUBNETWORK(CLASS)
	    X2XSN_MAX_THRUPUT_SEND(SUBNETWORK)=X2XSCL_MAX_THRUPUT_SEND
        ELSEIF(FIELD.EQ.79) THEN                                        ! V07
            X2XC_UNSO_STN_CON(CLASS) = X2XSCL_UNSSTCON                  ! V07
        ELSEIF(FIELD.EQ.80) THEN                                        ! V07
            X2XC_UNSO_STN_DIS(CLASS) = X2XSCL_UNSSTDIS                  ! V07
        ELSEIF (FIELD.EQ.81) THEN
            DO I=1,4
                BX2XC_ROMREV1(I,CLASS)=ICHAR(X2XSCL_ROMREV1(I))
            END DO
        ELSEIF (FIELD.EQ.82) THEN
            DO I=1,4
                BX2XC_ROMREV2(I,CLASS)=ICHAR(X2XSCL_ROMREV2(I))
            END DO
        ELSEIF (FIELD.EQ.83) THEN
            DO I=1,4
                BX2XC_ROMREV3(I,CLASS)=ICHAR(X2XSCL_ROMREV3(I))
            END DO
        ELSEIF (FIELD.EQ.84) THEN
            X2XC_DLL_USE_ENVELOPE(CLASS)=X2XSCL_DLL_USE_ENVELOPE
        ELSEIF (FIELD.EQ.85) THEN
            X2XC_REQ_BACKGROUND_DELAY(CLASS)=X2XSCL_REQ_BACKGROUND_DELAY
        ELSEIF (FIELD.EQ.86) THEN
          X2XC_DLL_ENABLE_BACKGROUND(CLASS)=X2XSCL_DLL_ENABLE_BACKGROUND
        ELSEIF (FIELD.EQ.87) THEN
            X2XC_DLL_BACKGROUND_FLAG(CLASS)=X2XSCL_DLL_BACKGROUND_FLAG
        ELSEIF (FIELD.EQ.88) THEN
            X2XC_ENABLE_TEST_DLL(CLASS)=X2XSCL_ENABLE_TEST_DLL
        ELSEIF (FIELD.EQ.89) THEN
            X2XC_REQ_FORGROUND_DELAY(CLASS)=X2XSCL_REQ_FORGROUND_DELAY
        ELSEIF (FIELD.EQ.90) THEN
            X2XC_DLL_ENABLE_FORGROUND(CLASS)=X2XSCL_DLL_ENABLE_FORGROUND
        ENDIF
C
8000    CONTINUE
	RETURN
	END
