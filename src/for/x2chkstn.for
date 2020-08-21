C SUBROUTINE X2CHKSTN
C
C V08 15-JUN-2000 OXK X2XCHK_SCL from X2XCHK.DEF to X2CHKSTN.FOR
C V07 19-OCT-1994 GPR DON'T PRINT ERROR IF DUPLICATE PVC PORT FOR USAT
C V06 27-SEP-1994 GPR DON'T PRINT ERROR IF STATION IS BCST AND ADDRESS
C		    MATCHES NETWORK PORT ADDRESS
C V05 22-AUG-1994 GPR USE STATION CLASS VALUES WHERE POSSIBLE
C V04 15-AUG-1994 GPR SPEED UP STATION CHECKS
C V03 03-FEB-1994 GPR USE I5 FORMAT FOR STATION AND TERMINAL TYPE-OUTS
C V02 09-JUL-1992 NJA FIXED PROBLEM WITH FIELD OVERFLOWS.
C V01 01-DEC-1991 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will perform edit checks on the
C Station configuration file.  Any errors encountered
C will be displayed to the screen, and if the input print
C flag is set, will also print the errors to the printer.
C
C Calling sequence:     CALL X2CHKSTN(PRTFLG,FAST,ERRCNT)
C Input parameters:
C     PRTFLG      Logical     Display errors to printer
C     FAST	  Logical     Fast editcheck flag
C Output parameters:
C     ERRCNT      Int*4       Count of errors detected
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2CHKSTN(PRTFLG,FAST,ERRCNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
	INCLUDE 'INCLIB:X2XSPC.DEF'
	INCLUDE 'INCLIB:X2XCHK.DEF'
	INCLUDE 'INCLIB:X2XGRP.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INTEGER*4   X2XCHK_SCL(X2XCHK_CLASS_PRTCNT,X2XC_CLASSES)

	INTEGER*2   PORTS(X2X_STATIONS)         !Ports per station
	INTEGER*4   ST                          !Read status
	INTEGER*4   REC                         !Record pointer
	INTEGER*4   I,J,K                       !Array index
	INTEGER*4   KEY(3)                      !Keys to sort on
        INTEGER*4   EOFCNT                      !End of file 
        INTEGER*4   NPCTYP(X2X_NETWORK_PORTS)   !Type of network ports
        INTEGER*4   STNTYP(X2X_STATIONS)	!Type of Station
        INTEGER*4   LSTSCL			!Last record pointers      
	INTEGER*4   ERRCNT                      !Number of errors
	INTEGER*4   STNCNT                      !Number of active stations
	INTEGER*4   FULLWORD                    !Fullword
        INTEGER*4   TTNANS
	CHARACTER   X2FILNAM*20                 !File name function
	LOGICAL     PRTFLG                      !Print error flag
	LOGICAL	    FAST			!Fast editcheck flag
        LOGICAL     GTECH_DIAL                  !Gtech dial stations
        LOGICAL     GTX_DIAL                    !GTX dial stations
        LOGICAL     SAME_DATA                   !Duplicate data for stations
        LOGICAL     ZERO_DATA                   !Zero data for station
	EQUIVALENCE (PORTS,FULLWORD)

	INTEGER*4   PROTOCOL
C

	WRITE(6,9020)
	IF(PRTFLG) WRITE(6,9020)
C
C  ***** Start V04 changes *****
C
C  TTN PARAMETERS ARE NOT USED FOR REMOTE STATIONS 
C  QUERY USER IF THESE ARE TO BE INCLUDED IN EDIT CHECKS  
C
CCC     CALL WIMG(6,' Do you wish to edit check TTN parameters?[Y/N]')
CCC     CALL YESNO(TTNANS)
C
C	Always do the TTN checks
C
	TTNANS=1
C
C  ***** End V04 changes *****
C
        IF(TTNANS.EQ.3) GOTO 90000
C
	ERRCNT=0
        EOFCNT=0
	STNCNT=0
        LSTSCL=0
        GTECH_DIAL = .FALSE.
        GTX_DIAL   = .FALSE.
        CALL FASTSET(0,NPCTYP,X2X_NETWORK_PORTS)
C
C OPEN THE STATION CONFIGURATION FILE.
C
	CALL OPENX2X(X2FILNAM(XSTN),1)
C
C OPEN THE HOST ADDRESS FILE.
C
	CALL OPENX(2,X2FILNAM(XNPC),4,0,0,ST)
	CALL IOINIT(X2XNPC_FDB,2,X2XNPC_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XNPC),'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
C
C OPEN THE STATION CLASS FILE.
C
	CALL OPENX(3,X2FILNAM(XSCL),4,0,0,ST)
	CALL IOINIT(X2XSCL_FDB,3,X2XSCL_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XSCL),'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
C
C	***** Start V04 changes *****
C
C	STORE THE STATION CLASS DATA
C
        DO I=1,X2XC_CLASSES
          CALL READW(X2XSCL_FDB,I,X2XSCL_REC,ST)
          IF(ST.NE.0.AND.ST.NE.144) THEN
            CALL OS32ER(5,X2FILNAM(XSCL),'READW',ST,2)
            CALL GPAUSE
          ELSE
            X2XCHK_SCL(X2XCHK_CLASS,I)=X2XSCL_CLASS
            X2XCHK_SCL(X2XCHK_CLASS_PRTCNT,I)=X2XSCL_PRTCNT             !V05
            X2XCHK_SCL(X2XCHK_CLASS_PROTO,I)=X2XSCL_PROTO
          ENDIF
        ENDDO
C
C	***** End V04 changes *****
C
C BUILD AN ARRAY CONTAINING THE NUMBER OF PORTS FOR EACH STATION.
C
	CALL PORTCNT(PORTS)
	REC=0
	X2XCHK_STNCNT=0
C
C READ THROUGH ENTIRE FILE SKIPPING EMPTY SLOTS.
C
100	CONTINUE
	  REC=REC+1
	  CALL READX2X(1,REC,X2XSTN_REC,ST)
	  IF(ST.EQ.144) GOTO 8000
C
C SKIP EMPTY RECORDS.
C
          IF(X2XSTN_REC(1).LE.0) THEN
            GOTO 100
          ELSE
            EOFCNT=0
          ENDIF
C
C IF USAT STATION CHECK FOR VALID NETWORK PORT TYPE
C
          IF(X2XSTN_PVCPORT.NE.0) THEN
            IF(X2XSTN_TYPE.EQ.X2XSCT_USAT_PVC) THEN
               CALL READW(X2XNPC_FDB,X2XSTN_PVCPORT,
     *                    X2XNPC_REC,ST)
               IF(X2XNPC_TYPE.NE.X2XPT_USAT_LAPB) THEN
                  WRITE(6,9036) REC, X2XSTN_PVCPORT  
                  IF(PRTFLG) WRITE(6,9036) REC, 
     *			     X2XCHK_STNREC(X2XCHK_STN_REC,I)
                  ERRCNT=ERRCNT+1
               ENDIF 
             ENDIF
          ENDIF  
C
C STORE THE HOST ADDRESS AND SERIAL NUMBER.
C
C	  ***** Start V04 changes *****
C
	  X2XCHK_STNCNT=X2XCHK_STNCNT+1
	  X2XCHK_STNADR(X2XCHK_STN,X2XCHK_STNCNT)=X2XSTN_STN
	  DO K=1,X2X_ADRESS_MAXLEN
	    X2XCHK_STNADR(X2XCHK_STN_ADR+K-1,X2XCHK_STNCNT)=X2XSTN_ADDRES(K)
	  ENDDO
	  X2XCHK_STNSER(X2XCHK_STN,X2XCHK_STNCNT)=X2XSTN_STN
	  X2XCHK_STNSER(X2XCHK_STN_SER,X2XCHK_STNCNT)=X2XSTN_SERIAL
	  X2XCHK_STNREC(X2XCHK_STN,X2XCHK_STNCNT)=X2XSTN_STN
	  X2XCHK_STNREC(X2XCHK_STN_REC,X2XCHK_STNCNT)=I
	  X2XCHK_STNPVC(X2XCHK_STN,X2XCHK_STNCNT)=X2XSTN_STN
	  X2XCHK_STNPVC(X2XCHK_STN_PVC,X2XCHK_STNCNT)=X2XSTN_PVCPORT
	  X2XCHK_STNEVSN(X2XCHK_STN,X2XCHK_STNCNT)=X2XSTN_STN
	  DO K=1,X2X_EVSN_MAXLEN
	    X2XCHK_STNEVSN(X2XCHK_STN_EVSN+K-1,X2XCHK_STNCNT)=X2XSTN_EVSN(K)
	  ENDDO
	  STNTYP(X2XSTN_STN)=X2XSTN_TYPE	!V07
C
C	  ***** End V04 changes *****
C
C IF FAST CHECK OF DATABASE, CHECK THE BITMAP TO
C DETERMINE IF THE RECORD HAS BEEN MODIFIED.
C
	  IF(FAST .AND. 
     *       X2XSTN_BITMAP.EQ.0 .AND. 
     *       X2XSTN_BITMAP2.EQ.0 .AND.
     *       X2XSTN_BITMAP3.EQ.0 .AND.
     *       X2XSTN_BITMAP4.EQ.0) GOTO 100
C
C CHECK FOR DUPLICATE STATION/HOST ADDRESSES.
C
	  DO 125 I=1,X2XCHK_NPCCNT
	    IF(X2XSTN_ADDRES(1).EQ.X2XCHK_NPCADR(1,I) .AND.
     *	       X2XSTN_ADDRES(2).EQ.X2XCHK_NPCADR(2,I)) THEN
C
C	      ***** Start V06 changes *****
C
	      IF(NPCTYP(X2XCHK_NPCREC(I)).EQ.0) THEN
		CALL READW(X2XNPC_FDB,X2XCHK_NPCREC(I),
     *			   X2XNPC_REC,ST)
		IF(ST.NE.0.AND.ST.NE.144) THEN
		  CALL OS32ER(5,X2FILNAM(XSTN),'READW',ST,
     *			      X2XCHK_NPCREC(I))
		  CALL GPAUSE
		ENDIF
		IF(ST.EQ.144 .OR. X2XNPC_REC(1).EQ.0) THEN
		  WRITE(6,9070) REC, X2XCHK_NPCREC(I)
		  IF(PRTFLG) WRITE(6,9070) REC, X2XCHK_NPCREC(I)
		  ERRCNT=ERRCNT+1
		ENDIF
		NPCTYP(I)=X2XNPC_TYPE
	      ENDIF
C
	      IF(.NOT.((X2XSTN_STATION_TYPE.EQ.X2XST_BCST).AND.
     *		       ((NPCTYP(I).EQ.X2XPT_BCST1).OR.
     *		        (NPCTYP(I).EQ.X2XPT_BCST2)))) THEN
	         WRITE(6,9060) REC, X2XCHK_NPCREC(I)
	         IF(PRTFLG) WRITE(6,9060) REC, X2XCHK_NPCREC(I)
	         ERRCNT=ERRCNT+1
	      ENDIF
C
C	      ***** End V06 changes *****
C
	    ENDIF
125	  CONTINUE
C
C CHECK TO ENSURE THE STATION CLASS EXISTS.
C
C	***** Start V04 chnages ***
C
	IF(LSTSCL.NE.X2XSTN_STNCLS) THEN
	  IF(X2XCHK_SCL(X2XCHK_CLASS,X2XSTN_STNCLS).EQ.0) THEN
	    WRITE(6,9050) REC, X2XSTN_STNCLS
	    IF(PRTFLG) WRITE(6,9050) REC, X2XSTN_STNCLS
	      ERRCNT=ERRCNT+1
	  ENDIF
	  LSTSCL=X2XSTN_STNCLS
        ENDIF
C
C	***** End V04 chnages ***
C
C
C CHECK TO ENSURE THE REPORT CLASS EXISTS.
C
C
C VERIFY THE NUMBER OF HOST ADDRESSES AND ENSURE THE
C HOST CODES EXIST.
C
	  DO 130 I=1,7
            IF(X2XSTN_NETPORT(I).NE.0) THEN
              IF(NPCTYP(X2XSTN_NETPORT(I)).EQ.0) THEN
                 CALL READW(X2XNPC_FDB,X2XSTN_NETPORT(I),
     *                     X2XNPC_REC,ST)
                IF(ST.NE.0.AND.ST.NE.144) THEN
                  CALL OS32ER(5,X2FILNAM(XSTN),'READW',ST,
     *                          X2XSTN_NETPORT(I))
                  CALL GPAUSE
                ENDIF
                IF(ST.EQ.144 .OR. X2XNPC_REC(1).EQ.0) THEN
                  WRITE(6,9070) REC, X2XSTN_NETPORT(I)
                  IF(PRTFLG) WRITE(6,9070) REC, X2XSTN_NETPORT(I)
                  ERRCNT=ERRCNT+1
                ENDIF
                NPCTYP(X2XSTN_NETPORT(I))=X2XNPC_TYPE
              ELSE
                X2XNPC_TYPE=NPCTYP(X2XSTN_NETPORT(I))
              ENDIF
C
C CHECK TO MAKE SURE THE NETWORK PORT TYPE MATCHES THE
C STATION TYPE.
C
             IF(X2XSTN_TYPE.EQ.X2XSCT_X21SWC .AND.
     *          (X2XNPC_TYPE.NE.X2XPT_X21.AND.
     *           X2XNPC_TYPE.NE.X2XPT_DIALUP)) THEN
                WRITE(6,9210) REC, X2XSTN_NETPORT(I)
                IF(PRTFLG) WRITE(6,9210) REC, X2XSTN_NETPORT(I)
                ERRCNT=ERRCNT+1
              ENDIF
              IF(X2XSTN_TYPE.EQ.X2XSCT_X25SVC .AND.
     *          (X2XNPC_TYPE.NE.X2XPT_X25.AND.
     *           X2XNPC_TYPE.NE.X2XPT_DIALUP)) THEN
                WRITE(6,9220) REC, X2XSTN_NETPORT(I)
                IF(PRTFLG) WRITE(6,9220) REC, X2XSTN_NETPORT(I)
                ERRCNT=ERRCNT+1
              ENDIF
             IF(X2XSTN_TYPE.EQ.X2XSCT_X28PAD .AND.
     *          (X2XNPC_TYPE.NE.X2XPT_X25.AND.
     *           X2XNPC_TYPE.NE.X2XPT_DIALUP)) THEN
                WRITE(6,9220) REC, X2XSTN_NETPORT(I)
                IF(PRTFLG) WRITE(6,9220) REC, X2XSTN_NETPORT(I)
                ERRCNT=ERRCNT+1
             ENDIF
C
C
C CHECK FOR DUPLICATE DEFINED HOST ADDRESS CODES.
C
	      DO 134 J=1,7
                IF(J.EQ.I) GOTO 134
                IF(X2XSTN_NETPORT(J).EQ.0 .OR.
     *             X2XSTN_NETPORT(I).EQ.0) GOTO 134
                IF(X2XSTN_NETPORT(I).EQ.X2XSTN_NETPORT(J)) THEN
                  WRITE(6,9080) REC,X2XSTN_NETPORT(J),'    '
                  IF(PRTFLG) WRITE(6,9080) REC,X2XSTN_NETPORT(J),'    '
                  ERRCNT=ERRCNT+1
                ENDIF
134	      CONTINUE
	    ENDIF
130	  CONTINUE
C
C IF ANY DEFAULT PORTS HAVE BEEN DEFINED, MAKE SURE THAT
C THEY ARE DEFINED.
C
	  DO 138 I=1,4
            IF(X2XSTN_DEF_PORT(I).NE.0) THEN
              IF(NPCTYP(X2XSTN_DEF_PORT(I)).EQ.0) THEN
                CALL READW(X2XNPC_FDB,X2XSTN_DEF_PORT(I),
     *                     X2XNPC_REC,ST)
                IF(ST.NE.0.AND.ST.NE.144) THEN
                  CALL OS32ER(5,X2FILNAM(XNPC),'READW',ST,
     *                          X2XSTN_DEF_PORT(I))
                  CALL GPAUSE
                ENDIF
                IF(ST.EQ.144 .OR. X2XNPC_REC(1).EQ.0) THEN
                  WRITE(6,9150) REC, X2XSTN_DEF_PORT(I)
                  IF(PRTFLG) WRITE(6,9150) REC, X2XSTN_DEF_PORT(I)
                  ERRCNT=ERRCNT+1
                ENDIF
                NPCTYP(X2XSTN_DEF_PORT(I))=X2XNPC_TYPE
              ELSE
                X2XNPC_TYPE=NPCTYP(X2XSTN_DEF_PORT(I))
              ENDIF
C
C CHECK FOR DUPLICATE DEFINED DEFAULT ADDRESS CODES.
C
	      DO 139 J=1,4
	        IF(J.EQ.I) GOTO 139
	        IF(X2XSTN_DEF_PORT(I).EQ.X2XSTN_DEF_PORT(J)) THEN
	          WRITE(6,9160) REC
	          IF(PRTFLG) WRITE(6,9160) REC
	          ERRCNT=ERRCNT+1
	        ENDIF
139	      CONTINUE
	    ENDIF
138	  CONTINUE
C
C IF ANY DIALUP PORTS HAVE BEEN DEFINED, MAKE SURE THAT
C THEY ARE DEFINED AND THAT THEY ARE DIALUP.
C
          DO 140 I=1,X2XSTN_MAXDIAL
            IF(X2XSTN_DIAL_PORT(I).NE.0) THEN
              IF(NPCTYP(X2XSTN_DIAL_PORT(I)).EQ.0) THEN
                CALL READW(X2XNPC_FDB,X2XSTN_DIAL_PORT(I),
     *                     X2XNPC_REC,ST)
                IF(ST.NE.0.AND.ST.NE.144) THEN
                  CALL OS32ER(5,X2FILNAM(XNPC),'READW',ST,
     *                          X2XSTN_DIAL_PORT(I))
                  CALL GPAUSE
                ENDIF
                IF(ST.EQ.144 .OR. X2XNPC_REC(1).EQ.0) THEN
                  WRITE(6,9170) REC, X2XSTN_DIAL_PORT(I)
                  IF(PRTFLG) WRITE(6,9170) REC, X2XSTN_DIAL_PORT(I)
                  ERRCNT=ERRCNT+1
                ENDIF
                NPCTYP(X2XSTN_DIAL_PORT(I))=X2XNPC_TYPE
              ELSE
                X2XNPC_TYPE=NPCTYP(X2XSTN_DIAL_PORT(I))
              ENDIF
C
C CHECK TO MAKE SURE THE PORT IS DIALUP TYPE.
C              
              IF(X2XNPC_TYPE.NE.X2XPT_DIALUP) THEN
	        WRITE(6,9230) REC, X2XSTN_DIAL_PORT(I)
	        IF(PRTFLG) WRITE(6,9230) REC, X2XSTN_DIAL_PORT(I)
	        ERRCNT=ERRCNT+1
	      ENDIF
C
C CHECK FOR DUPLICATE DEFINED DIAL ADDRESS CODES.
C
	      DO 141 J=1,X2XSTN_MAXDIAL
	        IF(J.EQ.I) GOTO 141
	        IF(X2XSTN_DIAL_PORT(I).EQ.X2XSTN_DIAL_PORT(J)) THEN
	          WRITE(6,9180) REC
	          IF(PRTFLG) WRITE(6,9180) REC
	          ERRCNT=ERRCNT+1
	        ENDIF
141	      CONTINUE
	    ENDIF
140	  CONTINUE
C
C VERIFY THE NUMBER OF HOST ADDRESSES AND ENSURE THE
C HOST CODES EXIST FOR X32 PORTS
C
          DO 230 I=1,X2XSTN_MAXX32
            IF(X2XSTN_X32_PORT(I).NE.0) THEN
              IF(NPCTYP(X2XSTN_X32_PORT(I)).EQ.0) THEN
                CALL READW(X2XNPC_FDB,X2XSTN_X32_PORT(I),
     *                     X2XNPC_REC,ST)
                IF(ST.NE.0.AND.ST.NE.144) THEN
                  CALL OS32ER(5,X2FILNAM(XSTN),'READW',ST,
     *                          X2XSTN_X32_PORT(I))
                  CALL GPAUSE
                ENDIF
                IF(ST.EQ.144 .OR. X2XNPC_REC(1).EQ.0) THEN
                  WRITE(6,9070) REC, X2XSTN_X32_PORT(I)
                  IF(PRTFLG) WRITE(6,9070) REC, X2XSTN_X32_PORT(I)
                  ERRCNT=ERRCNT+1
                ENDIF
                NPCTYP(X2XSTN_X32_PORT(I))=X2XNPC_TYPE
              ELSE
                X2XNPC_TYPE=NPCTYP(X2XSTN_X32_PORT(I))
              ENDIF
C
C CHECK TO MAKE SURE THE X32 NETWORK PORT TYPE MATCHES THE
C STATION TYPE.
C
              IF(X2XSTN_TYPE.EQ.X2XSCT_X21SWC .AND.
     *          (X2XNPC_TYPE.NE.X2XPT_X21.AND.
     *           X2XNPC_TYPE.NE.X2XPT_DIALUP)) THEN
                WRITE(6,9210) REC, X2XSTN_X32_PORT(I)
                IF(PRTFLG) WRITE(6,9210) REC, X2XSTN_X32_PORT(I)
                ERRCNT=ERRCNT+1
              ENDIF
              IF(X2XSTN_TYPE.EQ.X2XSCT_X25SVC .AND.
     *          (X2XNPC_TYPE.NE.X2XPT_X25.AND.
     *           X2XNPC_TYPE.NE.X2XPT_DIALUP)) THEN
                WRITE(6,9220) REC, X2XSTN_X32_PORT(I)
                IF(PRTFLG) WRITE(6,9220) REC, X2XSTN_X32_PORT(I)
                ERRCNT=ERRCNT+1
              ENDIF
C
C CHECK FOR DUPLICATE DEFINED HOST ADDRESS CODES.
C NO CHECK IS DONE TO SEE IF THE X32 PORTS ARE THE SAME AS THE HOST PORTS
C
              DO 234 J=1,X2XSTN_MAXX32
                IF(J.EQ.I) GOTO 234
                IF(X2XSTN_X32_PORT(J).EQ.0 .OR.
     *             X2XSTN_X32_PORT(I).EQ.0) GOTO 234
                IF(X2XSTN_X32_PORT(I).EQ.X2XSTN_X32_PORT(J)) THEN
                  WRITE(6,9080) REC,X2XSTN_X32_PORT(J),'X32 '
                IF(PRTFLG) WRITE(6,9080) REC, X2XSTN_X32_PORT(J),'X32 '
                  ERRCNT=ERRCNT+1
                ENDIF
234           CONTINUE
            ENDIF
230       CONTINUE
C
C VERIFY THE NUMBER OF INPUT PORTS MATCH THE NUMBER OF
C DEFINED PORTS.
C
C ***** Start V05 changes *****
C
          IF(PORTS(REC).NE.
     *      X2XCHK_SCL(X2XCHK_CLASS_PRTCNT,LSTSCL)) THEN
            WRITE(6,9100) REC, PORTS(REC),
     *        X2XCHK_SCL(X2XCHK_CLASS_PRTCNT,LSTSCL)
            IF(PRTFLG) WRITE(6,9100) REC, PORTS(REC),
     *        X2XCHK_SCL(X2XCHK_CLASS_PRTCNT,LSTSCL)
            ERRCNT=ERRCNT+1
          ENDIF
C
C ***** End V05 changes *****
C
C MAKE SURE STATION HAS PVC PORT ASSIGNMENT (ASYNC PVC TYPE)
C
          IF(X2XSTN_TYPE.EQ.X2XSCT_ASYPVC .OR.
     *       X2XSTN_TYPE.EQ.X2XSCT_USAT_PVC) THEN
           IF(X2XSTN_PVCPORT.EQ.0) THEN
             WRITE(6,9033) REC
             IF(PRTFLG) WRITE(6,9033) REC
             ERRCNT=ERRCNT+1
           ENDIF
          ENDIF
C
	  PROTOCOL=0
	  IF (X2XSTN_STNCLS.NE.0) PROTOCOL=X2XSCL_PROTO
          IF(X2XSTN_TYPE.EQ.X2XSCT_GTECH_DIAL) GTECH_DIAL=.TRUE.
          IF(X2XSTN_TYPE.EQ.X2XSCT_ASYPVC .OR.
     *       X2XSTN_TYPE.EQ.X2XSCT_USAT_PVC .AND.
     *       PROTOCOL.EQ.3) GTX_DIAL=.TRUE.
C*****     *       X2XSTN_PROTO.EQ.3) GTX_DIAL=.TRUE.
	  STNCNT=STNCNT+1
	  GOTO 100
C
C PROGRAM EXIT.
C
8000	CONTINUE
C
C	***** Start V04 changes *****
C
C       SORT THE ADDRESS ARRAY
C
        DO K=1,3
          IF(K.LE.X2X_ADRESS_MAXLEN) THEN
            KEY(K)=X2XCHK_STN_ADR+K-1
	  ELSE
	    KEY(K)=0
          ENDIF
        ENDDO
C
D	TYPE *,'X2XCHK_STN+X2X_ADRESS_MAXLEN, ',
D    *	  'X2XCHK_STNCNT,KEY(1),KEY(2),KEY(3) ',
D    *	  X2XCHK_STN+X2X_ADRESS_MAXLEN,
D    *	  X2XCHK_STNCNT,KEY(1),KEY(2),KEY(3)

        CALL I4XSORT(X2XCHK_STNADR,X2XCHK_STN+X2X_ADRESS_MAXLEN,
     *               X2XCHK_STNCNT,KEY(1),KEY(2),KEY(3))

D	TYPE *,'after i4xsort '

C
C       SORT THE EVSN ARRAY
C
        DO K=1,3
          IF(K.LE.X2X_EVSN_MAXLEN) THEN
            KEY(K)=X2XCHK_STN_EVSN+K-1
	  ELSE
	    KEY(K)=0
          ENDIF
        ENDDO
C
        CALL I4XSORT(X2XCHK_STNEVSN,X2XCHK_STN+X2X_EVSN_MAXLEN,
     *               X2XCHK_STNCNT,KEY(1),KEY(2),KEY(3))
C
C       SORT THE PVC ARRAY
C
        DO K=1,3
          IF(K.LE.1) THEN
            KEY(K)=2
	  ELSE
	    KEY(K)=0
          ENDIF
        ENDDO
C
        CALL I4XSORT(X2XCHK_STNPVC,X2XCHK_STN_PVC,
     *               X2XCHK_STNCNT,KEY(1),KEY(2),KEY(3))
C
C       SORT THE SERIAL ARRAY
C
        DO K=1,3
          IF(K.LE.1) THEN
            KEY(K)=2
	  ELSE
	    KEY(K)=0
          ENDIF
        ENDDO
C
        CALL I4XSORT(X2XCHK_STNSER,X2XCHK_STN_SER,
     *               X2XCHK_STNCNT,KEY(1),KEY(2),KEY(3))
C
        DO I=1,X2XCHK_STNCNT-1
C
C         CHECK IF ZERO ADDRESS
C
          ZERO_DATA=.TRUE.
          K=1
          DO WHILE ((ZERO_DATA).AND.(K.LE.X2X_ADRESS_MAXLEN))
            IF(X2XCHK_STNADR(X2XCHK_STN_ADR+K-1,I).NE.0) THEN
              ZERO_DATA=.FALSE.
            ENDIF
            K=K+1
          ENDDO
C
C         CHECK FOR DUPLICATE STATION ADDRESSES IF NOT ZERO

          IF(.NOT.ZERO_DATA) THEN
            SAME_DATA=.TRUE.
            K=1
            DO WHILE ((SAME_DATA).AND.(K.LE.X2X_ADRESS_MAXLEN))
              IF((X2XCHK_STNADR(X2XCHK_STN_ADR+K-1,I).NE.
     *          X2XCHK_STNADR(X2XCHK_STN_ADR+K-1,I+1))) THEN
                SAME_DATA=.FALSE.
              ENDIF
              K=K+1
            ENDDO
            IF(SAME_DATA) THEN
	      WRITE(6,9010) X2XCHK_STNADR(X2XCHK_STN,I),
     *		  	    X2XCHK_STNADR(X2XCHK_STN,I+1)
	      IF(PRTFLG) WRITE(6,9010) X2XCHK_STNADR(X2XCHK_STN,I),
     *			 X2XCHK_STNADR(X2XCHK_STN,I+1)
	      ERRCNT=ERRCNT+1
            ENDIF
          ENDIF
C
C         CHECK IF ZERO EVSN
C
          ZERO_DATA=.TRUE.
          K=1
          DO WHILE ((ZERO_DATA).AND.(K.LE.X2X_EVSN_MAXLEN))
            IF(X2XCHK_STNEVSN(X2XCHK_STN_EVSN+K-1,I).NE.0) THEN
              ZERO_DATA=.FALSE.
            ENDIF
            K=K+1
          ENDDO
C
C         CHECK FOR DUPLICATE EVSN IF NOT ZERO
C
          IF(.NOT.ZERO_DATA) THEN
            SAME_DATA=.TRUE.
            K=1
            DO WHILE ((SAME_DATA).AND.(K.LE.X2X_EVSN_MAXLEN))
              IF((X2XCHK_STNEVSN(X2XCHK_STN_EVSN+K-1,I).NE.
     *          X2XCHK_STNEVSN(X2XCHK_STN_EVSN+K-1,I+1))) THEN
                SAME_DATA=.FALSE.
              ENDIF
              K=K+1
            ENDDO
            IF(SAME_DATA) THEN
	      WRITE(6,9250)X2XCHK_STNEVSN(X2XCHK_STN,I),
     *			   X2XCHK_STNEVSN(X2XCHK_STN,I+1)
              IF(PRTFLG) WRITE(6,9250)X2XCHK_STNEVSN(X2XCHK_STN,I),
     *                   X2XCHK_STNEVSN(X2XCHK_STN,I+1)
              ERRCNT=ERRCNT+1
            ENDIF
          ENDIF
C
C	  CHECK FOR DUPLICATE PVC IF NOT ZERO AND NOT USAT PORT		  !V07
C
	  IF((X2XCHK_STNPVC(X2XCHK_STN_PVC,I).NE.0) .AND.
     *	     (STNTYP(X2XCHK_STNPVC(X2XCHK_STN,I)).NE.
     *	      X2XSCT_USAT_PVC)) THEN					  !V07
	    IF(X2XCHK_STNPVC(X2XCHK_STN_PVC,I).EQ.
     *	       X2XCHK_STNPVC(X2XCHK_STN_PVC,I+1)) THEN
	      WRITE(6,9035) X2XCHK_STNPVC(X2XCHK_STN,I),
     *			    X2XCHK_STNPVC(X2XCHK_STN,I+1)		  !V07
	      IF(PRTFLG) WRITE(6,9035)X2XCHK_STNPVC(X2XCHK_STN,I),
     *                   X2XCHK_STNPVC(X2XCHK_STN,I+1)			  !V07
	      ERRCNT=ERRCNT+1
	    ENDIF
	  ENDIF
C
C	  CHECK FOR DUPLICATE SERIAL IF NOT ZERO
C
	  IF(X2XCHK_STNSER(X2XCHK_STN_SER,I).NE.0) THEN
	    IF(X2XCHK_STNSER(X2XCHK_STN_SER,I).EQ.
     *	      X2XCHK_STNSER(X2XCHK_STN_SER,I+1)) THEN
	      WRITE(6,9040) X2XCHK_STNSER(X2XCHK_STN,I),
     *			    X2XCHK_STNSER(X2XCHK_STN,I+1)
	      IF(PRTFLG) WRITE(6,9040)X2XCHK_STNSER(X2XCHK_STN,I),
     *                      X2XCHK_STNSER(X2XCHK_STN,I+1)
	      ERRCNT=ERRCNT+1
	    ENDIF
	  ENDIF
        ENDDO
C
C	***** End V04 changes *****
C
C IF ANY GTECH DIALUP STATIONS WERE DEFFINED, MAKE SURE THERE IS AT LEAST
C ONE GTX DIAL STATION (PSUEDO STATION ) DEFINED TO SUPPORT IT
C
        IF(GTECH_DIAL.AND.(.NOT.GTX_DIAL)) THEN
          WRITE(6,9260) CHAR(7)
          IF(PRTFLG) WRITE(9260)
          ERRCNT=ERRCNT+1
        ENDIF
C
C CHECK IF MAXIMUM NUMBER OF STATIONS DEFINED
C
	IF(STNCNT.GT.X2X_STATIONS) THEN
	  WRITE(6,9110) CHAR(7)
	  IF(PRTFLG) WRITE(6,9110)
	  ERRCNT=ERRCNT+1
	ENDIF
90000   CONTINUE
	CALL CLOSX2X(1)
	CALL CLOSEFIL(X2XNPC_FDB)
	CALL CLOSEFIL(X2XSCL_FDB)
	WRITE(6,9030) ERRCNT
	IF(PRTFLG) WRITE(6,9030) ERRCNT
	RETURN
C
C     =================== Format Statements ==================
C
9010	FORMAT(3X,'Station ',I5,' has duplicate address',
     *	          ' with station ',I5)
9020	FORMAT(1X,'Begin Edit Check of Station Configuration')
9030	FORMAT(1X,'End Edit Check of Station Configuration - 'I5,
     *	          ' error(s) encountered')
9033	FORMAT(3X,'Station ',I5,' has no PVC port Assignment')
9035	FORMAT(3X,'Station ',I5,' has duplicate PVC port #',
     *	          ' with station ',I5)					! V03
9036    FORMAT(3X,'GSAT station ',I5,' has invalid PVC port type ',
     *            'Port # ',I5)  
9040	FORMAT(3X,'Station ',I5,' has duplicate serial #',
     *	          ' with station ',I5)					! V03
9050	FORMAT(3X,'Station ',I5,' has an invalid station class',
     *	          ' code of ',I5)
9052	FORMAT(3X,'Station ',I5,' has an invalid report class',
     *	          ' code of ',I5)
9060	FORMAT(3X,'Station ',I5,' has the same address as ',
     *	          ' host code ',I5)
9070	FORMAT(3X,'Station ',I5,' has invalid host address ',
     *	          ' code ',I5)
9080    FORMAT(3X,'Station ',I5,' has duplicate host codes defined ',
     *             I6,1X,A4)
9100	FORMAT(3X,'Station ',I5,' input port count does not ',
     *	          ' match defined ',I5,'/',I5)
9110	FORMAT(3X,'FATAL ERROR: too many stations have ',
     *	          'been defined',A)
9150	FORMAT(3X,'Station ',I5,' has invalid default port ',I5)
9160	FORMAT(3X,'Station ',I5,' has duplicate default ports ')
9170	FORMAT(3X,'Station ',I5,' has invalid dialup port ',I5)
9180	FORMAT(3X,'Station ',I5,' has duplicate default ports ')
9210	FORMAT(3X,'Station ',I5,' is X.21 and net port ',I3,
     *	          ' is incompatible')
9220	FORMAT(3X,'Station ',I5,' is X.25 and net port ',I3,
     *	          ' is incompatible')
9230	FORMAT(3X,'Station ',I5,' has dialup port ',I3,' which is',
     *	          ' is incompatible')
9250    FORMAT(3X,'Station ',I5,' has duplicate Extended Ver Ser. Num.',
     *         1X,' with station ',I5)
9260    FORMAT(3X,'Gtech Dialup stations defined without',1X,
     *            'GTX Dialup support',A)
	END
