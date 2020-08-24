C
C SUBROUTINE X2LODSCL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LODSCL.FOV                                 $
C  $Date::   17 Apr 1996 16:22:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lodscl.for;1 **
C
C X2LODSCL.FOR
C
C V09  15-sep-95 das Changed for background loads
C V08  10-MAR-94 SCD LOAD X2XC_DIAL_OV_OVERRIDE (especially important for
C                    X.28 GVTs)
C V07  29-DEC-94 WJK MOVE UNSOLICITED STATION CONNECT AND DISCONNECT FROM GLOBAL
C                    TO STATION CLASS, RECORD
C V06  20-OCT-94 GPR LOAD UNSO MSGS VALUE - Integrate UK changes into 
C		     X2X Baseline
C V05  22-SEP-94 GPR LOAD DIAL TYPE - Integrate UK changes into X2X Baseline
C V04  21-AUG-94 GPR LOAD STATION CLASS FILEDS WHICH WERE ADDED TO X2XCOM -
C		     Integrate UK changes into X2X Baseline
C V03  09-JUN-94 WS  Subnetwork stuff -Integrate UK changes into X2X Baseline
C V02 28-APR-94 XXX GET STATION CLASS INFO FROM MEMORY
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will load the GTECH Distributed Network
C common from the Station CLASS data files.
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
	SUBROUTINE X2LODSCL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
 
C
	CHARACTER   X2FILNAM*20             !File name function
        INTEGER*4   CLASS                   !Station class
        INTEGER*4   ST,TXT	  		    !STATUS
	INTEGER*4   I			    !V03
	INTEGER*4   SUBNETWORK		    !V03
C
C OPEN THE STATION CLASS FILE.
C
	CALL OPENX2X(X2FILNAM(XSCL),2)
C
C READ THE STATION CLASS ALLOWING CHECKSUMS
C Store fields required by X2STCONF into X2XCOM.
C
 
	DO 100 CLASS = 1, X2XC_CLASSES			    ! max class nr
C
C           Initialise memory common for stations class to -1
C
            X2XSC_MEM_STORE(1,CLASS) = -1
C
	    CALL READX2X(2,CLASS,X2XSCL_REC,ST)	
C
	    IF(ST.EQ.144 .OR. X2XSCL_REC(1) .LE. 0) THEN
	      BX2XC_STATE(CLASS) = X2XC_UNDEFINED	    ! 0 - undefined
	      GOTO 100
            ELSE     ! STORE FIELDS OF THIS CLASS INTO COMMON. 	
	      BX2XC_STATE(CLASS)       = X2XC_DEFINED	    ! 1 - defined
              BX2XC_MSTENA(CLASS)      = X2XSCL_MSTENA
              BX2XC_DIAL_ENABLE(CLASS) = X2XSCL_DIALENA
              BX2XC_CLOCK(CLASS)       = X2XSCL_CLOCK
              BX2XC_SYNC(CLASS)        = X2XSCL_SYNC
C
              X2XC_INTIM(CLASS)     = X2XSCL_INTIM
              X2XC_OUTTIM(CLASS)    = X2XSCL_OUTTIM
              X2XC_RESTIM(CLASS)    = X2XSCL_RESTIM
              X2XC_INTTIM(CLASS)    = X2XSCL_INTTIM
              X2XC_STSTIM(CLASS)    = X2XSCL_STSTIM
              X2XC_RETRY(CLASS)     = X2XSCL_RETRY
              X2XC_ABS_TIM(CLASS)   = X2XSCL_ABS_DISC_TIM
C
	      X2XC_SITE(1,CLASS) = X2XSCL_SITE1  ! Site for network port
	      X2XC_SITE(2,CLASS) = X2XSCL_SITE2  ! Site for network port
	      X2XC_SITE(3,CLASS) = X2XSCL_SITE3  ! Site for network port 	
	      X2XC_SITE(4,CLASS) = X2XSCL_SITE4  ! Site for network port
	      X2XC_SITE(5,CLASS) = X2XSCL_SITE5  ! Site for network port
	      X2XC_SITE(6,CLASS) = X2XSCL_SITE6  ! Site for network port
	      X2XC_SITE(7,CLASS) = X2XSCL_SITE7  ! Site for network port
C
              X2XC_SITE(8,CLASS) = X2XSCL_X32_SITE1
              X2XC_SITE(9,CLASS) = X2XSCL_X32_SITE2
              X2XC_DISX25ROM(CLASS) = X2XSCL_DISX25ROM        !X25 rom
              X2XC_DISDUPROM(CLASS) = X2XSCL_DISDUPROM        !dup rom
              X2XC_DISX25RAM(CLASS) = X2XSCL_DISX25RAM        !x25 ram
              X2XC_DISDUPRAM(CLASS) = X2XSCL_DISDUPRAM        !dup rom
              X2XC_SLPTIME(CLASS)   = X2XSCL_SLPTIME          !SLEEP TIME
              X2XC_AFTMAX(CLASS)    = X2XSCL_AFTMAX           !AFTER HOUR CNT
              X2XC_NAAS_LENGTH(CLASS)=X2XSCL_NAAS_LENGTH
              DO 50 TXT=1,X2XSCL_NAAS_LENGTH
                X2XC_NAAS_TEXT(CLASS,TXT)=X2XSCL_NAAS_TEXT(TXT)
50            CONTINUE

              X2XC_DIAL_OV_OVERRIDE(CLASS)=X2XSCL_DIAL_OV_OVERRIDE       !V08
	      X2XC_BCST_RETRY_INTERVAL(CLASS)=X2XSCL_BCST_RETRY_INTERVAL !V03
	      X2XC_BCST_RETRY_LIMIT(CLASS)=X2XSCL_BCST_RETRY_LIMIT       !V03
	      X2XC_BCST_AUTO_REPORT(CLASS)=X2XSCL_BCST_AUTO_REPORT       !V03
	      X2XC_BCST_ENABLE1(CLASS)=X2XSCL_BCST_ENABLE1		 !V03
	      X2XC_BCST_ENABLE2(CLASS)=X2XSCL_BCST_ENABLE2		 !V03
	      X2XC_BCST_NET_PORT1(CLASS)=X2XSCL_BCST_NET_PORT1		 !V03
	      X2XC_BCST_NET_PORT2(CLASS)=X2XSCL_BCST_NET_PORT2		 !V03
	      X2XC_SUBNETWORK(CLASS)= X2XSCL_SUBNETWORK			 !V03
	      X2XC_TTN_PORT1(CLASS)=X2XSCL_TTN_PORT1			 !V03
	      X2XC_TTN_PORT2(CLASS)=X2XSCL_TTN_PORT2			 !V03
	      X2XC_L2TOUT(CLASS)=X2XSCL_L2TOUT				 !V03
	      X2XC_POLTIM(CLASS)=X2XSCL_POLTIM				 !V03
	      X2XC_PROTO(CLASS)=X2XSCL_PROTO				 !V03
	      X2XC_REPCLS(CLASS)=X2XSCL_REPCLS				 !V03
	      X2XC_DEF_PORT_OVERRIDE(CLASS)=X2XSCL_DEF_PORT_OVERRIDE	 !V03
	      X2XC_BAUD(CLASS)=X2XSCL_BAUD				 !V03
      	      DO 200 I=1,X2X_MAXPRT_ASSIGN				 !V03
      		  X2XC_NETPORT(I,CLASS)=X2XSCL_NETPORT(I)		 !V03
200   	      CONTINUE							 !V03
C
C	      ***** Start V04 changes *****
C
	      X2XC_ADDLEN(CLASS)=X2XSCL_ADDLEN
	      X2XC_AUTOUPD(CLASS)=X2XSCL_AUTOUPD
	      X2XC_DELACK(CLASS)=X2XSCL_DELACK
	      X2XC_DIAL_PORT(1,CLASS)=X2XSCL_DIAL_PORT1
	      X2XC_DIAL_PORT(2,CLASS)=X2XSCL_DIAL_PORT2
	      X2XC_ERRREP(CLASS)=X2XSCL_ERRREP
	      X2XC_EVSN_LEN(CLASS)=X2XSCL_EVSN_LEN
	      X2XC_FEDIS(CLASS)=X2XSCL_FEDIS
	      X2XC_NETSTAT(CLASS)=X2XSCL_NETSTAT
	      X2XC_PRTCNT(CLASS)=X2XSCL_PRTCNT
	      X2XC_CLASS(CLASS)=X2XSCL_CLASS
	      X2XC_STNDIS(CLASS)=X2XSCL_STNDIS
	      X2XC_TYPE(CLASS)=X2XSCL_TYPE
	      X2XC_DESC(CLASS)=X2XSCL_DESC
	      X2XC_MAX_THRUPUT(CLASS)=X2XSCL_MAX_THRUPUT
	      X2XC_MAX_THRUPUT_SEND(CLASS)=X2XSCL_MAX_THRUPUT_SEND
	      X2XC_DIAL_TYPE(CLASS)=X2XSCL_DIAL_TYPE			    !V05
	      X2XC_UNSO_MSGS(CLASS)=X2XSCL_UNSO_MSGS			    !V06
              X2XC_UNSO_STN_CON(CLASS) = X2XSCL_UNSSTCON ! V07 UNSOLICIT CONNECT
              X2XC_UNSO_STN_DIS(CLASS) = X2XSCL_UNSSTDIS ! V07 UNSOLICIT DISCONT
C
	      SUBNETWORK=X2XSCL_SUBNETWORK
	      IF (X2XSN_MAX_THRUPUT(SUBNETWORK).EQ.0) THEN
		X2XSN_MAX_THRUPUT(SUBNETWORK)=X2XSCL_MAX_THRUPUT
	      ENDIF
	      IF (X2XSN_MAX_THRUPUT_SEND(SUBNETWORK).EQ.0) THEN
		X2XSN_MAX_THRUPUT_SEND(SUBNETWORK)=X2XSCL_MAX_THRUPUT_SEND
	      ENDIF
C
C	      ***** End V04 changes *****
C
C       DLL PARAMETERS
C
              X2XC_DLL_USE_ENVELOPE(CLASS)=X2XSCL_DLL_USE_ENVELOPE
              X2XC_REQ_BACKGROUND_DELAY(CLASS)
     *             =X2XSCL_REQ_BACKGROUND_DELAY
              X2XC_DLL_ENABLE_BACKGROUND(CLASS)
     *             =X2XSCL_DLL_ENABLE_BACKGROUND
              X2XC_DLL_BACKGROUND_FLAG(CLASS)=X2XSCL_DLL_BACKGROUND_FLAG
              X2XC_ENABLE_TEST_DLL(CLASS)=X2XSCL_ENABLE_TEST_DLL
              X2XC_REQ_FORGROUND_DELAY(CLASS)=X2XSCL_REQ_FORGROUND_DELAY
              X2XC_DLL_ENABLE_FORGROUND(CLASS)
     *             =X2XSCL_DLL_ENABLE_FORGROUND

              DO I=1,4
                BX2XC_ROMREV1(I,CLASS)=ICHAR(X2XSCL_ROMREV1(I))
                BX2XC_ROMREV2(I,CLASS)=ICHAR(X2XSCL_ROMREV2(I))
                BX2XC_ROMREV3(I,CLASS)=ICHAR(X2XSCL_ROMREV3(I))
              END DO
C
C
            ENDIF					
C
C  Store the class record into memory common
C
CV03            DO 60 TXT = 1, X2XSC_REC_LEN
CV03               X2XSC_MEM_STORE(TXT,CLASS) = X2XSCL_REC(TXT)
CV0360          CONTINUE
C 
100	CONTINUE					! next class
C 
8000	CONTINUE
	CALL CLOSX2X(2)
C
	RETURN
	END
