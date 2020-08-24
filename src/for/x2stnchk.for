C
C SUBROUTINE X2STNCHK
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STNCHK.FOV                                 $
C  $Date::   17 Apr 1996 16:36:04                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2stnchk.for;1 **
C
C X2STNCHK.FOR
C
C V05 12-DEC-94 DAS Integrate UK changes into X2X Baseline
C V04 22-AUG-94 GPR USE DIAL ENABLE AND DIAL PORTS FROM STATION CLASS
C V03 28-APR-94 XXX GET STATION 
C V02 25-MAR-94 GPR MODIFIED TO BUILD THE CHKBUF PROPERLY
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This subroutine will calculate the default parameter
C checksums for a given station.
C
C Input parameters:
C
C     STN     Int*4       Station number
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
	SUBROUTINE X2STNCHK(STN,TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C***	INCLUDE 'INCLIB:X2XSTN.DEF'
C***	INCLUDE 'INCLIB:X2XNPC.DEF'
C***	INCLUDE 'INCLIB:X2XSCL.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
C
	INTEGER*2   CHKSUM                  !Checksum variable
	INTEGER*2   CHKBUF(400)             !Checksum buffer
	INTEGER*4   STN                     !Station number
        INTEGER*4   PORT_COUNT              !SUM OF NETWORK + X32 PORTS
	INTEGER*4   TEMP, OUTCALL, I, CHKLEN, LSTBYT
        INTEGER*4   REC
        INTEGER*4   DISMODE                 !Real disconnection mode      ! V02
        INTEGER*4   TEMP_ADR(2)             !Address work variable        ! V02
        LOGICAL     DIALPRT                 !Dialup port flag             ! V02
        LOGICAL     OLD_PROTO               !STATION PROTOCOL REVISION FLAG
C
	INTEGER*4   CLASS, ADR_LEN
C
C DETERMINE IF THIS IS THE OLD STATION PROTOCOL OR NEW
C
	CLASS=X2XS_STNCLS(STN)
	IF (CLASS.LE.0) THEN
            CALL OPS('x2stnchk - no class for st ',STN,CLASS)
	    GOTO 8000
	ENDIF

        OLD_PROTO = .TRUE.
        IF((TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_DEFAULT_CONF2)
     *    .OR. (TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_RESET2))
     *                     OLD_PROTO = .FALSE.
C
C OPEN THE NETWORK PORT CONFIGURATION FILE.
C
C
C STORE DEFAULT TTN CHECKSUMS FOR BOTH LINES.
C
C****	IF(X2XSCL_TTN_PORT1.NE.0)
C****     *	  X2XS_TTN1_CHKSUM(STN)=X2XD_CHKSUM(X2XSCL_TTN_PORT1)
C****	IF(X2XSCL_TTN_PORT2.NE.0)
C****     *	  X2XS_TTN2_CHKSUM(STN)=X2XD_CHKSUM(X2XSCL_TTN_PORT2)
C
	IF(X2XC_TTN_PORT1(CLASS).NE.0)
     *	  X2XS_TTN1_CHKSUM(STN)=X2XD_CHKSUM(X2XC_TTN_PORT1(CLASS))
	IF(X2XC_TTN_PORT2(CLASS).NE.0)
     *	  X2XS_TTN2_CHKSUM(STN)=X2XD_CHKSUM(X2XC_TTN_PORT2(CLASS))
C
C LOAD DEFAULT STATION CLASS PARAMETERS AND CHECKSUM.
C
	LSTBYT=1
	ADR_LEN=X2XS_ADRESS_LEN(STN)
	CALL ISBYTE(ADR_LEN,CHKBUF,LSTBYT-1)
	LSTBYT=LSTBYT+1

C       ***** START V02 CHANGES *****

        TEMP_ADR(1) = X2XS_ADRESS(1,STN)
        TEMP_ADR(2) = X2XS_ADRESS(2,STN)
        CALL X2QSHFT(TEMP_ADR,64-(ADR_LEN*4))          !right justify

        CALL I4TOBUF4(TEMP_ADR(1),CHKBUF,LSTBYT-1)
        LSTBYT=LSTBYT+4
        CALL I4TOBUF4(TEMP_ADR(2),CHKBUF,LSTBYT-1)
        LSTBYT=LSTBYT+4

C       ***** END V02 CHANGES *****

        CALL I4TOBUF2(X2XC_L2TOUT(CLASS),CHKBUF,LSTBYT-1)
	LSTBYT=LSTBYT+2
	CALL I4TOBUF2(X2XC_INTIM(CLASS),CHKBUF,LSTBYT-1)
	LSTBYT=LSTBYT+2
	CALL I4TOBUF2(X2XC_OUTTIM(CLASS),CHKBUF,LSTBYT-1)
	LSTBYT=LSTBYT+2
	CALL I4TOBUF2(X2XC_RESTIM(CLASS),CHKBUF,LSTBYT-1)
	LSTBYT=LSTBYT+2
	CALL I4TOBUF2(X2XC_INTTIM(CLASS),CHKBUF,LSTBYT-1)
	LSTBYT=LSTBYT+2
        IF(.NOT.OLD_PROTO) THEN
          CALL I4TOBUF2(X2XC_SLPTIME(CLASS),CHKBUF,LSTBYT-1)
          LSTBYT=LSTBYT+2
          CALL I4TOBUF2(X2XC_AFTMAX(CLASS),CHKBUF,LSTBYT-1)
          LSTBYT=LSTBYT+2
        ENDIF
	CHKLEN=LSTBYT-1
C
C CALCULATE CHECKSUMS.
C
	CALL X2STRSUM(CHKBUF,CHKLEN,CHKSUM)
	X2XS_STN_CHKSUM(STN)=CHKSUM
C
C LOAD DEFAULT INITIAL CALL PARAMETERS AND CHECKSUM.
C
	LSTBYT=1
        IF(.NOT.OLD_PROTO) THEN
          PORT_COUNT = X2XS_MAXDEF + X2X_MAXX32_ASSIGN
        ELSE
          PORT_COUNT = X2XS_MAXDEF
        ENDIF
C
C UP TO SIX (6) SLOTS ARE AVAILABLE - BUT IF
C DIALUP PORTS ARE ASSIGNED AND DIAL ENABLE IS ON, THEN
C USE X2X_MAXDIAL_ASSIGN (2) OF THE SLOTS FOR DIALUP.
C THE LAST TWO DEFAULT SLOTS WILL USE THE X.32 PORTS
C
        DO 120 I=1,PORT_COUNT
          DIALPRT=.FALSE.                                             ! V02
C
C IN ADDITION TO THE DIALUP PORTS ASSIGNED TWO X32 PORTS MUST ALSO    ! V02
C BE ASSIGNED IF DIALUP IS ENABLED                                    ! V02
C
C FIND THE APPROPRIATE NETWORK PORT (NORMAL OR DIALUP OR X32).
C
C****	  IF ((I.GE.1).AND.(I.LE.X2XS_MAXDEF)) REC=X2XS_DEF_PORT(I,STN) 
          IF((I.GE.1).AND.(I.LE.X2XS_MAXDEF)) THEN
	    REC=X2XS_DEF_PORT(I,STN)
	    IF (X2XC_DEF_PORT_OVERRIDE(CLASS).NE.0) THEN
		REC=X2XC_DEF_PORT_OVERRIDE(CLASS)
		IF (I.GT.1) REC=0
	    ENDIF
	  ENDIF		  
C
C IF DIAL IS ENABLED THEN THIS WILL OVERRIDE SLOT 3 & 4 ASSIGNED ABOVE
C
          IF(BX2XC_DIAL_ENABLE(CLASS).EQ.0) THEN		      !V04
            IF((I.GT.2).AND.(I.LE.X2XS_MAXDEF)) THEN
               DIALPRT = .TRUE.                                       ! V02
               REC = X2XS_DIAL_PORT(I-2,STN)
	       IF(REC.EQ.0) THEN                                      !V04
                 REC = X2XC_DIAL_PORT(I-2,CLASS)                      !V04
               ENDIF                                                  !V04

             ENDIF
          ENDIF
C
          IF(I.GT.X2XS_MAXDEF) REC = X2XS_X32PORT(I-4,STN)
C
C
C IF RECORD DOES NOT EXIST, CLEAR IT OUT, OTHERWISE SET
C OUTCALL TO VALID.
C
          OUTCALL=0
	  IF(X2XPN_OUTCALL(REC).EQ.0 .AND. X2XPN_TYPE(REC).NE.0) OUTCALL=1
	  CALL I4TOBUF2(OUTCALL,CHKBUF,LSTBYT-1)
	  LSTBYT=LSTBYT+2
C
C SET CALL FLAGS.
C
	  TEMP=0
	  IF(BX2XPN_FAST(REC).EQ.0)    TEMP=TEMP+X2STMES_CALL_FS
	  IF(BX2XPN_REVCHRG(REC).EQ.0) TEMP=TEMP+X2STMES_CALL_RC
C         ***** START V02 CHANGES *****

          IF(DIALPRT) THEN
            TEMP=TEMP+X2STMES_CALL_DIAL
            IF(X2XC_DISDUPROM(CLASS).GT.0) THEN
                 DISMODE=X2XC_DISDUPROM(CLASS)
            ELSE
              DISMODE=X2XPN_DDIS(REC)
            ENDIF
          ELSE
            IF(X2XC_DISX25ROM(CLASS).GT.0) THEN
              DISMODE=X2XC_DISX25ROM(CLASS)
            ELSE
              DISMODE=X2XPN_DDIS(REC)
            ENDIF
          ENDIF

C         ***** END V02 CHANGES *****

	  CALL I4TOBUF2(TEMP,CHKBUF,LSTBYT-1)
	  LSTBYT=LSTBYT+2
C
          CALL I4TOBUF2(DISMODE,CHKBUF,LSTBYT-1)                        ! V02
	  LSTBYT=LSTBYT+2
	  CALL I4TOBUF2(X2XPN_RETCNT(REC),CHKBUF,LSTBYT-1)
	  LSTBYT=LSTBYT+2
	  CALL I4TOBUF2(X2XPN_RETTIM(REC),CHKBUF,LSTBYT-1)
	  LSTBYT=LSTBYT+2
	  CALL ISBYTE(X2XPN_ADDLEN(REC),CHKBUF,LSTBYT-1)
	  LSTBYT=LSTBYT+1

C         ***** START V02 CHANGES *****

          IF((X2XPN_HUNT_ADR(1,REC).NE.0) .AND.
     *       (X2XPN_HUNT_ADR(2,REC).NE.0)) THEN
 
           TEMP_ADR(1) = X2XPN_HUNT_ADR(1,REC)
            TEMP_ADR(2) = X2XPN_HUNT_ADR(2,REC)
          ELSE
            TEMP_ADR(1) = X2XPN_ADRESS(1,REC)
            TEMP_ADR(2) = X2XPN_ADRESS(2,REC)
          ENDIF
          CALL X2QSHFT(TEMP_ADR,64-(X2XPN_ADDLEN(REC)*4))
          CALL I4TOBUF4(TEMP_ADR(1),CHKBUF,LSTBYT-1)
          LSTBYT=LSTBYT+4
          CALL I4TOBUF4(TEMP_ADR(2),CHKBUF,LSTBYT-1)
          LSTBYT=LSTBYT+4

C         ***** END V02 CHANGES *****

120	CONTINUE
C
C CALCULATE CHECKSUM.
C
	CHKLEN=LSTBYT-1
	CALL X2STRSUM(CHKBUF,CHKLEN,CHKSUM)
	X2XS_CALL_CHKSUM(STN)=CHKSUM
C
8000	CONTINUE
	RETURN
	END
