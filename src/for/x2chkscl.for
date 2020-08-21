C
C SUBROUTINE X2CHKSCL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKSCL.FOV                                 $
C  $Date::   17 Apr 1996 16:12:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C X2CHKSCL.FOR
C
C V04 12-DEC-94 GPR Integrate UK changes into X2X Baseline
C V03 23-AUG-94 GPR ADD CHECKS FOR NETWORK PORTS
C V02 09-JUL-92 NJA FIXED PROBLEM WITH FIELD OVERFLOWS.
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will perform edit checks on the
C Station Class records..  Any errors encountered
C will be displayed to the screen, and if the input print
C flag is set, will also print the errors to the printer.
C
C Calling sequence:
C
C     CALL X2CHKSCL(PRTFLG,FAST,ERRCNT)
C
C Input parameters:
C
C     PRTFLG      Logical     Display errors to printer
C     FAST	  Logical     Fast editcheck flag
C
C Output parameters:
C
C     ERRCNT      Int*4       Count of errors detected
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
	SUBROUTINE X2CHKSCL(PRTFLG,FAST,ERRCNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
	INCLUDE 'INCLIB:X2XTTN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4   ST                          !Read status
	INTEGER*4   I,J                         !Array index
        INTEGER*4   ERRCNT                      !Number of errors
        INTEGER*4   MODCNT                      !Count of modified records
        INTEGER*4   MODTBL(100)                 !Modified SCL records
	INTEGER*4   X2FILSEC, REC
        INTEGER*4   ANS
        INTEGER*4   NPCTYP(X2X_NETWORK_PORTS)   !Type of network ports	  !V03
	CHARACTER   X2FILNAM*20                 !File name function
        CHARACTER   PROMPT*60
	LOGICAL     PRTFLG                      !Print error flag
	LOGICAL	    FAST			!Fast editcheck error
        LOGICAL     DONE                        !Done reading             !V03

        CHARACTER*20 PASPAS						  !V04
        EQUIVALENCE(PAS,PASPAS)						  !V04
        CHARACTER*8 PAS							  !V04
C
	WRITE(5,9020)
	IF(PRTFLG) WRITE(6,9020)
	ERRCNT=0
        MODCNT=0
C
C OPEN THE STATION CLASS FILE FOR BUFFERED I/O.
C
	CALL OPENX2X(X2FILNAM(XSCL),1)
C
C OPEN THE TITAN FILE. (BUFFERED I/O)
C
	CALL OPENX(3,X2FILNAM(XTTN),4,0,0,ST)
	IF(ST.NE.0)THEN
	  CALL OS32ER(5,X2FILNAM(XTTN),'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
	CALL IOINIT(X2XTTN_FDB,3,X2FILSEC(XTTN)*256)
C
C OPEN THE NETWORK PORT CONFIGURATION FILE. (BUFFERED I/O)
C AND READ ALL THE NETWORK PORT TYPES
C
C	***** Start V03 changes *****
C
	CALL OPENX2X(X2FILNAM(XNPC),2)
C
        CALL FASTSET(0,NPCTYP,X2X_NETWORK_PORTS)
        REC=1
        DONE=.FALSE.
        DO WHILE ((.NOT.DONE).AND.(REC.LE.X2X_NETWORK_PORTS))
	  CALL READX2X(2,REC,X2XNPC_REC,ST)
          IF(ST.EQ.-99) THEN
            WRITE(5,9050) IAM()
            ERRCNT=ERRCNT+1
            DONE=.TRUE.
          ELSEIF (ST.EQ.144) THEN
            DONE=.TRUE.
	  ELSE
	    NPCTYP(REC)=X2XNPC_TYPE
	    REC=REC+1
	  ENDIF
	ENDDO
        CALL CLOSX2X(2)
C
C	***** End V03 changes *****
C
C READ THROUGH STATION CLASS FILE SKIPPING EMPTY SLOTS.
C
	REC=0
100	CONTINUE
	  REC=REC+1
	  CALL READX2X(1,REC,X2XSCL_REC,ST)
	  IF(ST.EQ.144) GOTO 8000
	  IF(X2XSCL_REC(1).LE.0) GOTO 100
C
C IF FAST CHECK OF DATABASE, CHECK THE BITMAP TO
C DETERMINE IF THE RECORD HAS BEEN MODIFIED.
C
	  IF(FAST .AND. 
     *       X2XSCL_BITMAP.EQ.0 .AND. 
     *       X2XSCL_BITMAP2.EQ.0 .AND.
     *       X2XSCL_BITMAP3.EQ.0 .AND.
     *       X2XSCL_BITMAP4.EQ.0) GOTO 100
C
C STORE MODIFIED RECORDS.
C
          IF(X2XSCL_BITMAP.NE.0 .OR.
     *      X2XSCL_BITMAP2.NE.0 .OR.
     *      X2XSCL_BITMAP3.NE.0 .OR.
     *      X2XSCL_BITMAP4.NE.0) THEN
            MODCNT=MODCNT+1
            MODTBL(MODCNT)=REC
          ENDIF
C
C CHECK TO MAKE SURE THAT THE TITAN PARAMETERS EXIST.
C

	  DO 102 I=1,2
	    IF(X2XSCL_TTN(I).NE.0) THEN
	      CALL READW(X2XTTN_FDB,X2XSCL_TTN(I),X2XTTN_REC,ST)
	      IF(ST.NE.0)THEN
	        CALL OS32ER(5,X2FILNAM(XTTN),'READW',ST,REC)
	        CALL GPAUSE
	      ENDIF
	      IF(X2XTTN_REC(1).LE.0) THEN
	        ERRCNT=ERRCNT+1
	        WRITE(5,9000) X2XSCL_TTN(I)
	        IF(PRTFLG) WRITE(6,9000) X2XSCL_TTN(I)
	      ENDIF
	    ENDIF
102	  CONTINUE
C
C	  ***** Start V03 changes *****
C
C 	  Check to make sure the network port type matches the
C 	  station type.
C
	  DO I=1,X2XS_MAXNET
	    IF(X2XSCL_NETPORT(I).NE.0) THEN
              IF(X2XSCL_TYPE.EQ.X2XSCT_X21SWC .AND.
     *          (NPCTYP(X2XSCL_NETPORT(I)).NE.X2XPT_X21.AND.
     *           NPCTYP(X2XSCL_NETPORT(I)).NE.X2XPT_DIALUP)) THEN
                WRITE(5,9210) REC, X2XSCL_NETPORT(I)
                IF(PRTFLG) WRITE(5,9210) REC, X2XSCL_NETPORT(I)
                ERRCNT=ERRCNT+1
              ENDIF
              IF(X2XSCL_TYPE.EQ.X2XSCT_X25SVC .AND.
     *          (NPCTYP(X2XSCL_NETPORT(I)).NE.X2XPT_X25.AND.
     *           NPCTYP(X2XSCL_NETPORT(I)).NE.X2XPT_DIALUP)) THEN
                WRITE(5,9220) REC, X2XSCL_NETPORT(I)
                IF(PRTFLG) WRITE(5,9220) REC, X2XSCL_NETPORT(I)
                ERRCNT=ERRCNT+1
              ENDIF
              IF(X2XSCL_TYPE.EQ.X2XSCT_X28PAD .AND.
     *          (NPCTYP(X2XSCL_NETPORT(I)).NE.X2XPT_X25.AND.
     *           NPCTYP(X2XSCL_NETPORT(I)).NE.X2XPT_DIALUP)) THEN
                WRITE(5,9220) REC, X2XSCL_NETPORT(I)
                IF(PRTFLG) WRITE(5,9220) REC, X2XSCL_NETPORT(I)
                ERRCNT=ERRCNT+1
              ENDIF
C
C 	      Check for duplicate defined host address codes.
C
	      DO J=1,X2XS_MAXNET
                IF(J.EQ.I) THEN
		  CONTINUE
		ELSEIF(X2XSCL_NETPORT(I).EQ.X2XSCL_NETPORT(J)) THEN
                  WRITE(5,9080) REC,X2XSCL_NETPORT(J),'    '
                  IF(PRTFLG) THEN
		    WRITE(6,9080) REC,X2XSCL_NETPORT(J),'    '
		  ENDIF
                  ERRCNT=ERRCNT+1
                ENDIF
	      ENDDO
C
            ENDIF
	  ENDDO
C
C 	  If any dialup ports have been defined, make sure that
C 	  they are defined and that they are dialup.
C
          DO I=1,X2XS_MAXDIAL
            IF(X2XSCL_DIAL_PORT(I).NE.0) THEN
C
C 	      Check to make sure the port is dialup type.
C
              IF(NPCTYP(X2XSCL_DIAL_PORT(I)).NE.X2XPT_DIALUP) THEN
                WRITE(5,9230) REC, X2XSCL_DIAL_PORT(I)
                IF(PRTFLG) WRITE(5,9230) REC, X2XSCL_DIAL_PORT(I)
                ERRCNT=ERRCNT+1
              ENDIF
C
C 	      Check for duplicate defined dial address codes.
C
              DO J=1,X2XS_MAXDIAL
                IF(J.EQ.I) THEN
		  CONTINUE
                ELSEIF(X2XSCL_DIAL_PORT(I).EQ.
     *		  X2XSCL_DIAL_PORT(J)) THEN
                  WRITE(5,9180) REC
                  IF(PRTFLG) THEN
		    WRITE(6,9180) REC
		  ENDIF
                  ERRCNT=ERRCNT+1
                ENDIF
	      ENDDO
            ENDIF
	  ENDDO
C
C 	  Verify the number of host addresses and ensure the
C 	  host codes exist for x32 ports
C
          DO I=1,X2XS_MAXX32
            IF(X2XSCL_X32(I).NE.0) THEN
C
C 	      Check to make sure the x32 network port type matches the
C 	      station type.
C
              IF(X2XSCL_TYPE.EQ.X2XSCT_X21SWC .AND.
     *          (NPCTYP(X2XSCL_X32(I)).NE.X2XPT_X21.AND.
     *           NPCTYP(X2XSCL_X32(I)).NE.X2XPT_DIALUP)) THEN
                WRITE(5,9210) REC, X2XSCL_X32(I)
                IF(PRTFLG) WRITE(5,9210) REC, X2XSCL_X32(I)
                ERRCNT=ERRCNT+1
              ENDIF
              IF(X2XSCL_TYPE.EQ.X2XSCT_X25SVC .AND.
     *          (NPCTYP(X2XSCL_X32(I)).NE.X2XPT_X25.AND.
     *           NPCTYP(X2XSCL_X32(I)).NE.X2XPT_DIALUP)) THEN
                WRITE(5,9220) REC, X2XSCL_X32(I)
                IF(PRTFLG) WRITE(5,9220) REC, X2XSCL_X32(I)
                ERRCNT=ERRCNT+1
              ENDIF
C
C 	      Check for duplicate defined host address codes.
C 	      no check is done to see if the x32 ports are the 
C	      same as the host ports
C
              DO J=1,X2XS_MAXX32
                IF(J.EQ.I) THEN
		  CONTINUE
		ELSEIF(X2XSCL_X32(I).EQ.X2XSCL_X32(J)) THEN
                  WRITE(5,9080) REC,X2XSCL_X32(J),'X32 '
		  IF(PRTFLG) WRITE(6,9080) REC, X2XSCL_X32(J),'X32 '
                  ERRCNT=ERRCNT+1
                ENDIF
	      ENDDO

            ENDIF
	  ENDDO
C
C	  ***** End V03 changes *****
C
C READ THE NEXT RECORD.
C
	  GOTO 100
C
C PROGRAM EXIT.
C
8000	CONTINUE
        WRITE(5,9030) ERRCNT
        IF(PRTFLG) WRITE(6,9030) ERRCNT
        IF(MODCNT.GT.0) THEN
8010	  CONTINUE
          WRITE(PROMPT,9040)
          CALL WIMG(5,PROMPT)
          CALL YESNO(ANS)
          IF(ANS.EQ.1) THEN
C
C	    ***** Start V04 changes *****
C
	    CALL CLRSCR(5)
	    CALL PASSWORD(5,PASPAS)
	    IF (PAS .NE. 'X2CHKMEN') GOTO 8010
C
	    TYPE *,'This function is no longer supported'
CV04            CALL X2SCLUPD(MODCNT,MODTBL)
C
C	    ***** End V04 changes *****
C
          ENDIF
        ENDIF
C
C CLOSE ALL FILES AND RETURN
C
	CALL CLOSX2X(1)
	CALL CLOSX2X(2)
	CALL CLOSEFIL(X2XTTN_FDB)
	RETURN
C
C     =================== Format Statements ==================
C
9000	FORMAT(3X,'Titan record ',I3,' is not defined')
9020	FORMAT(1X,'Begin Edit Check of Station Class')
9030	FORMAT(1X,'End Edit Check of Station Class - 'I5,
     *	          ' error(s) encountered')
9040	FORMAT(1X,'Do you wish to update stations effected by class Y/N ')
C
C	***** Start V04 changes *****
C
9050	FORMAT(1X,A,'Locked record encounted - check aborted ')
9080	FORMAT(3X,'Station Class ',I5,' has duplicate host codes defined ',
     *             I6,1X,A4)
9160	FORMAT(3X,'Station Class ',I5,' has duplicate default ports ')
9180	FORMAT(3X,'Station Class ',I5,' has duplicate dial ports ')
9210	FORMAT(3X,'Station Class ',I5,' is X.21 and net port ',I3,
     *            ' is incompatible')
9220	FORMAT(3X,'Station Class ',I5,' is X.25 and net port ',I3,
     *            ' is incompatible')
9230	FORMAT(3X,'Station Class ',I5,' has dialup port ',I3,' which is',
     *            ' is incompatible')
C
C	***** End V04 changes *****
C
	END
