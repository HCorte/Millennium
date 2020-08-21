C
C SUBROUTINE X2LODNPC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LODNPC.FOV                                 $
C  $Date::   17 Apr 1996 16:21:46                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lodnpc.for;1 **
C
C X2LODNPC.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will load the GTECH Distributed Network
C common from the Network Port Configuration file.
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
	SUBROUTINE X2LODNPC
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
C
	INTEGER*4   PORT            !Port number
	INTEGER*4   ST              !Status
	INTEGER*4   PRTCNT          !Number of ports loaded
	INTEGER*4   HNTPRT, I
        INTEGER*4   PVC_IDX/0/      !Index to vector of USAT circuits
	CHARACTER   X2FILNAM*20     !File name function
C
C OPEN THE NETWORK PORT CONFIGURATION FILE.
C
	PRTCNT=0
	I=0
	CALL OPENLOD(X2FILNAM(XNPC),1)
C
C LOOP THROUGH ALL NETWORK PORTS IN THE FILE.
C
	PORT=0
100	CONTINUE
	  PORT=PORT+1
	  IF(PORT.GT.X2X_NETWORK_PORTS) GOTO 8000
	  CALL READLOD(1,PORT,X2XNPC_REC,ST)
	  IF(ST.EQ.144) GOTO 8000
C
C SKIP UNUSED TRANSACTIONS.
C
	  IF(X2XNPC_REC(1).LE.0) THEN
	    IF(PORT.LT.X2XNPC_RANGE(2,1)) THEN
	      GOTO 100
	    ELSE
	      GOTO 8000
	    ENDIF
	  ENDIF
C
C STORE THE INFORMATION INTO COMMON.
C
	  X2XPN_CAPACITY(PORT)=X2XNPC_CAPACITY
	  X2XPN_ADRESS(1,PORT)=X2XNPC_ADDRES(1)
	  X2XPN_ADRESS(2,PORT)=X2XNPC_ADDRES(2)
	  X2XPN_STATE(PORT)=X2XNPC_STATE
	  X2XPN_OUTCALL(PORT)=X2XNPC_ASSIGN
	  X2XPN_TYPE(PORT)=X2XNPC_TYPE
	  X2XPN_NUMPVC(PORT)=X2XNPC_NUMPVC
C
          IF(X2XPN_TYPE(PORT).EQ.X2XPT_USAT_LAPB) THEN
            PVC_IDX = PVC_IDX+1
            IF (PVC_IDX.GT.X2X_MAXPVC_LINES) THEN
              TYPE *,'X2LODNPC:TOO MANY PORTS WITH PVC CIRCUITS ',PORT
            ELSE
              X2XPN_PVC_INDEX(PORT)= PVC_IDX          ! POINTS TO USAT CKTS
            ENDIF
          ENDIF
C
	  X2XPN_HUNT_ADR(1,PORT)=X2XNPC_HUNTADR(1)
	  X2XPN_HUNT_ADR(2,PORT)=X2XNPC_HUNTADR(2)
C 
	  X2XPN_FAST(PORT)   = CHAR(X2XNPC_FAST)	! V02
	  X2XPN_REVCHRG(PORT)= CHAR(X2XNPC_REVCHRG)	! V02
C 
	  X2XPN_DDIS(PORT)   = X2XNPC_DDIS		! V02
	  X2XPN_RETCNT(PORT) = X2XNPC_RETCNT		! V02
	  X2XPN_RETTIM(PORT) = X2XNPC_RETTIM		! V02
	  X2XPN_ADDLEN(PORT) = X2XNPC_ADDLEN		! V02
C 
C
C SEARCH THROUGH THE HUNT ADDRESS TO SEE IF WE HAVE A
C NEW ONE.
C
	  IF(X2XNPC_HUNTADR(1).EQ.0 .AND.
     *	     X2XNPC_HUNTADR(2).EQ.0) GOTO 210
C
	  DO 200 I=1,X2X_MAX_HUNT
	    HNTPRT=X2XPN_HUNT(I)
	    IF(HNTPRT.EQ.0) GOTO 205
	    IF(X2XNPC_HUNTADR(1).EQ.X2XPN_HUNT_ADR(1,HNTPRT).AND.
     *	       X2XNPC_HUNTADR(2).EQ.X2XPN_HUNT_ADR(2,HNTPRT))
     *	         GOTO 210
	    IF(X2XPN_HUNT(I).EQ.0) GOTO 205
200	  CONTINUE
C
C HUNT ADDRESS IS A NEW ONE, SO STORE IT.
C
205	  CONTINUE
	  IF(I.LE.X2X_MAX_HUNT) THEN
	    X2XPN_HUNT(I)=PORT
	  ENDIF
C
210	  CONTINUE
	  PRTCNT=PRTCNT+1
	  IF(PORT.LT.X2XNPC_RANGE(2,1)) GOTO 100
C
C PROGRAM TERMINAL.
C
8000	CONTINUE
C***  WRITE(5,9010) PRTCNT
	CALL CLOSLOD(1)
	RETURN
C
C     ==================== Format Statements ==================
C
C***9000  FORMAT(1X,'Loading Network Port Configuration')
C***9010  FORMAT(1X,I6,' network ports loaded')
	END
