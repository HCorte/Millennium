C SUBROUTINE X2INIT
C
C V05 16-JUN-2000 OXK Cleanup w/ WARNINGS=ALL
C V04 05-DEC-1994 SCD Integrate UK changes into X2X Baseline
C V03 02-AUG-1994 SCD Added call to READ_DUMRNG to read range of dummy
C                   stations and data initialization to support GVT
C                   installation
C V02 01-APR-1994 GPR Remove commented (Dead) code
C V01 01-DEC-1991 XXX RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will load X2XCOM with all necessary stored
C in the X2X database.
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
	SUBROUTINE X2INIT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:CTLCOM.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:X2XREL.DEF'
C
        INCLUDE 'INCLIB:NOTEVN.DEF'
        INCLUDE '($SYSSRVNAM)'
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
C
	INTEGER*4   CONFIGURATION, STN, ANS, FLAG, OFF
C
	CHARACTER   PROMPT*60           !OUTPUT PROMPT
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	   TYPE *,'X2INIT '
C
C DATA INITIALIZATION
C
        WRITE(5,*) IAM(),'Loading X2XCOM'
	CALL X2XCLR
	CALL X2SETPRO            !SET OUTPUT QUEUE
	CALL X2SETPOST		 !SET X2XBLD POSTING QUEUE - V04
C
C CHECK TO ENSURE THAT EDIT CHECKS HAVE BEEN PERFORMED FROM
C BLDX2X.
C
	CALL X2CHKEXT(FLAG)
	IF(FLAG.NE.0) THEN
	  WRITE(5,9000) IAM(),CHAR(7)
	  WRITE (PROMPT,9010)
	  CALL WIMG(5,PROMPT)
	  CALL YESNO(ANS)
	  IF(ANS.NE.1) CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C CLEAR COMMON.
C
	CTLX2XLOC=CTLX2XOK
	THISSTA=0
C
C SET PRINT FLAGS.
C
	DO 100 STN=1,X2X_STATIONS
	  X2X_STN_PRINT(STN)=X2X_NONE_PRINT
100	CONTINUE
C
	DO 200 STN=1,X2X_SAP
	  X2X_SAP_PRINT(STN)=X2X_NONE_PRINT
200	CONTINUE
C
	DO 300 STN=1,X2ERR_MAX_ERR
	  X2X_MSG_PRINT(STN)=X2X_NONE_PRINT
300	CONTINUE
C
C LOAD THE GLOBAL PARAMETERS.
C
        WRITE(5,*) IAM(),'Loading global parameters'
	CALL X2LODGBL
C
C LOAD THE NETWORK PORT CONFIGURATION.
C
        WRITE(5,*) IAM(),'Loading network ports'
	CALL X2LODNPC
C
C LOAD THE LOCAL PORT CONFIGURATION.
C
        WRITE(5,*) IAM(),'Loading local ports'
	CALL X2LODLPC
C
C LOAD THE TTN CHECKSUMS.
C
        WRITE(5,*) IAM(),'Loading checksums'
	CALL X2LODTTN
C
C LOAD THE STATION CLASS DATA REQUIRED BY X2STCONF INTO X2XCOM	  
C
        WRITE(5,*) IAM(),'Loading station classes'
	CALL X2LODSCL				
C
C LOAD THE STATION INFORMATION AND SET THE CONFIGURATION
C CHECKSUM FOR EACH STATION.
C
        WRITE(5,*) IAM(),'Loading stations'
	CALL X2LODSTN
	CONFIGURATION=IAND(DAYCDC,63)   !HOW TO ENSURE RESET RUN??
C                                     ;START AFTER BEGINING OF RESET
	CONFIGURATION=CONFIGURATION+256*CONFIGURATION+
     *	   (256*256)*CONFIGURATION+(256*256*256)*CONFIGURATION
	CALL FASTSET(CONFIGURATION,IX2XS_CONF,X2X_STATIONS/4)
C
C LOAD TERMINAL INFORMATION.
C
        WRITE(5,*) IAM(),'Loading terminals'
	CALL X2LODTER
C
C LOAD THE BROADCAST RELAY INFORMATION.
C
        WRITE(5,*) IAM(),'Loading relay processes'	!V04
	CALL X2LODBRO
        DO 400 STN=1,X2X_STATIONS
          X2XR_STATION_STATE(STN) = X2XR_NOT_IN_RELAY  
400     CONTINUE
C
C 	***** Start V04 changes *****
C
C LOAD RELAY GROUPS.
C
        WRITE(5,*) IAM(),'Loading relay groups'
	CALL X2LODGRP
C
C LOAD THE NETWORK PORT ASSIGNMENT TABLES.
C
	IF(X2X_DISTRIBUTE_X25) THEN
	  WRITE(5,*) IAM(),'Loading Distributed network ports'
	  CALL X2LODNET
	ENDIF
C
C 	***** End V04 changes *****
C
C 	***** Start V03 changes *****

        CALL READ_DUMRNG		!V03 - must be called AFTER X2XCLR,
					!which needs to know the # of dummy
					!stations in each class
	DO 20 OFF = 1,X2XC_CLASSES
	   IF (X2XC_DUMMY_STN_COUNT(OFF) .NE. 0)
     *	       CALL FASTSET(X2XC_DUMMY_AVAILABLE,
     *			    X2XC_DUMMY_FREE_LIST(1,OFF),
     *			    X2XC_DUMMY_STN_COUNT(OFF))
  20	CONTINUE

C 	***** End V03 changes *****

	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	     TYPE *,'RETURN X2INIT'
	RETURN
C
C     ==================== Format Statements ================
C
9000	FORMAT(1X,A,'WARNING:  BLDX2X Edit Checks have not been ',
     *	          ' successfully completed!! ',A)
9010	FORMAT(1X,'Do you wish to continue [Y/N] ')
	END
